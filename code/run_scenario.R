# run_scenario.R
# Run tariff scenarios and compare to baseline
#
# Usage:
#   Interactive: source("run_scenario.R") then call run_scenario("ieepa_zero")
#   Command line: Rscript run_scenario.R ieepa_zero
#
# This script:
#   1. Validates the scenario exists in data/scenarios/rates.csv
#   2. Sets SCENARIO_NAME global variable
#   3. Sources us_tariff_calculation.R (which loads rates/shares from CSV)
#   4. Sources relative_advantage.R (final calculations)
#   5. Optionally runs flow-level comparison against baseline

library(data.table)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Find the most recent baseline file in results/
#' @return Path to latest baseline .RData file
find_latest_baseline <- function() {
  files <- list.files(
    "results",
    pattern = "processed_us_imports_with_rates_baseline_.*\\.RData",
    full.names = TRUE
  )
  if (length(files) == 0) {
    stop("No baseline file found in results/. Run baseline scenario first.")
  }
  # Return most recently modified
  files[which.max(file.mtime(files))]
}

#' List all available scenarios from rates.csv
#' @return Character vector of scenario names
list_scenarios <- function() {
  rates <- fread("data/scenarios/rates.csv")
  unique(rates$scenario)
}

#' Run a tariff calculation scenario
#' @param scenario_name Name of scenario (must exist in rates.csv)
#' @param base_file Optional path to baseline file for comparison
#' @param run_comparison If TRUE, compare results to baseline
#' @param output_dir Directory for output files (default: "results")
#' @return Path to output .RData file
run_scenario <- function(scenario_name,
                         base_file = NULL,
                         run_comparison = TRUE,
                         output_dir = "results") {

  cat("\n")
  cat("=============================================================\n")
  cat(sprintf("  Running scenario: %s\n", scenario_name))
  cat("=============================================================\n\n")

  # Validate scenario exists
  available <- list_scenarios()
  if (!scenario_name %in% available) {
    stop(sprintf(
      "Scenario '%s' not found in rates.csv. Available: %s",
      scenario_name,
      paste(available, collapse = ", ")
    ))
  }

  # Find baseline for comparison (if needed)
  if (is.null(base_file) && run_comparison && scenario_name != "baseline") {
    base_file <- find_latest_baseline()
    cat(sprintf("Using baseline for comparison: %s\n\n", basename(base_file)))
  }

  # Construct output filename with scenario and date
  date_stamp <- format(Sys.Date(), "%Y%m%d")
  output_filename <- sprintf(
    "processed_us_imports_with_rates_%s_%s.RData",
    scenario_name,
    date_stamp
  )
  output_path <- file.path(output_dir, output_filename)

  # Set global variables for the calculation scripts
  SCENARIO_NAME <<- scenario_name
  SCENARIO_OUTPUT_PATH <<- output_path

  cat("Step 1: Running us_tariff_calculation.R...\n")
  cat("-------------------------------------------------------------\n")
  source("code/us_tariff_calculation.R")

  cat("\nStep 2: Running relative_advantage.R...\n")
  cat("-------------------------------------------------------------\n")
  source("code/relative_advantage.R")

  # Run flow comparison if requested and not baseline
  if (run_comparison && !is.null(base_file) && scenario_name != "baseline") {
    cat("\nStep 3: Running flow-level comparison...\n")
    cat("-------------------------------------------------------------\n")
    source("code/scenario_flow_comparison.R")
    comparison_result <- compare_flows(
      baseline_path = base_file,
      scenario_path = output_path,
      scenario_name = scenario_name,
      output_dir = output_dir
    )
    cat(sprintf("  Comparison saved to: %s\n", output_dir))
  }

  # Clean up global variables
  if (exists("SCENARIO_NAME", envir = .GlobalEnv)) {
    rm(SCENARIO_NAME, envir = .GlobalEnv)
  }
  if (exists("SCENARIO_OUTPUT_PATH", envir = .GlobalEnv)) {
    rm(SCENARIO_OUTPUT_PATH, envir = .GlobalEnv)
  }

  cat("\n")
  cat("=============================================================\n")
  cat(sprintf("  Scenario '%s' complete!\n", scenario_name))
  cat(sprintf("  Results saved to: %s\n", output_path))
  cat("=============================================================\n\n")

  return(output_path)
}

#' Verify new baseline matches existing baseline exactly
#' @param existing_baseline Path to existing baseline file
#' @param new_baseline Path to new baseline file (from CSV-based calculation)
#' @return TRUE if match, error if mismatch
verify_baseline_match <- function(existing_baseline, new_baseline) {
  cat("\n")
  cat("=============================================================\n")
  cat("  Verification Test: Comparing Baseline Outputs\n")
  cat("=============================================================\n\n")

  # Load existing baseline
  cat("Loading existing baseline...\n")
  env1 <- new.env()
  load(existing_baseline, envir = env1)
  existing_data <- copy(env1$trade.data)

  # Load new baseline
  cat("Loading new baseline...\n")
  env2 <- new.env()
  load(new_baseline, envir = env2)
  new_data <- copy(env2$trade.data)

  # Merge on key columns
  cat("Comparing final_rate values at flow level...\n")
  comparison <- merge(
    existing_data[, .(hs8, i, trade_value_usd,
                      final_rate_existing = final_rate,
                      ieepa_rate_existing = ieepa_rate)],
    new_data[, .(hs8, i,
                 final_rate_new = final_rate,
                 ieepa_rate_new = ieepa_rate)],
    by = c("hs8", "i"),
    all = FALSE
  )

  # Calculate differences
  comparison[, rate_diff := final_rate_new - final_rate_existing]
  comparison[, ieepa_diff := ieepa_rate_new - ieepa_rate_existing]

  # Summary statistics
  max_rate_diff <- max(abs(comparison$rate_diff), na.rm = TRUE)
  max_ieepa_diff <- max(abs(comparison$ieepa_diff), na.rm = TRUE)

  # Trade-weighted averages
  existing_avg <- comparison[, weighted.mean(final_rate_existing, trade_value_usd, na.rm = TRUE)]
  new_avg <- comparison[, weighted.mean(final_rate_new, trade_value_usd, na.rm = TRUE)]

  cat(sprintf("\nResults:\n"))
  cat(sprintf("  Flows compared: %s\n", format(nrow(comparison), big.mark = ",")))
  cat(sprintf("  Max final_rate difference: %.6f\n", max_rate_diff))
  cat(sprintf("  Max ieepa_rate difference: %.6f\n", max_ieepa_diff))
  cat(sprintf("  Trade-weighted avg (existing): %.4f%%\n", existing_avg))
  cat(sprintf("  Trade-weighted avg (new):      %.4f%%\n", new_avg))
  cat(sprintf("  Trade-weighted avg difference: %.6f pp\n", abs(new_avg - existing_avg)))

  # Verify
  tolerance <- 0.0001  # Allow for floating point differences

  if (max_rate_diff > tolerance) {
    # Show some discrepancies
    discrepancies <- comparison[abs(rate_diff) > tolerance]
    cat("\n\nDISCREPANCIES FOUND:\n")
    print(head(discrepancies[order(-abs(rate_diff))], 20))
    stop(sprintf("VERIFICATION FAILED: Max rate difference (%.4f) exceeds tolerance (%.4f)",
                 max_rate_diff, tolerance))
  }

  if (abs(existing_avg - new_avg) > tolerance) {
    stop(sprintf("VERIFICATION FAILED: Trade-weighted averages differ by %.6f pp",
                 abs(existing_avg - new_avg)))
  }

  cat("\n")
  cat("=============================================================\n")
  cat("  ✓ VERIFICATION PASSED\n")
  cat("  New baseline matches existing baseline exactly!\n")
  cat("=============================================================\n\n")

  return(TRUE)
}

# -----------------------------------------------------------------------------
# Command-line execution
# -----------------------------------------------------------------------------

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) >= 1) {
    scenario <- args[1]

    if (scenario == "--list") {
      cat("Available scenarios:\n")
      cat(paste(" -", list_scenarios(), collapse = "\n"))
      cat("\n")
    } else if (scenario == "--verify") {
      # Special mode: verify baseline match
      existing <- if (length(args) >= 2) args[2] else find_latest_baseline()
      SCENARIO_NAME <<- "baseline"
      source("code/us_tariff_calculation.R")
      source("code/relative_advantage.R")
      new_baseline <- "results/processed_us_imports_with_rates_baseline_verification.RData"
      verify_baseline_match(existing, new_baseline)
    } else {
      run_scenario(scenario)
    }
  } else {
    cat("Usage: Rscript run_scenario.R <scenario_name>\n")
    cat("       Rscript run_scenario.R --list\n")
    cat("       Rscript run_scenario.R --verify [existing_baseline]\n\n")
    cat("Available scenarios:\n")
    cat(paste(" -", list_scenarios(), collapse = "\n"))
    cat("\n")
  }
}
