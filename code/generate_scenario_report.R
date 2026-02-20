# =============================================================================
# SCENARIO REPORT GENERATOR
# =============================================================================
# Generates a 1-pager scenario report (MD + DOCX) comparing a scenario to baseline
#
# Usage:
#   source("code/generate_scenario_report.R")
#   generate_scenario_report("ieepa_full_strike_down")
#
# Or called automatically from us_tariff_calculation_db.R for non-baseline scenarios
# =============================================================================

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Generate Scenario Report
#' 
#' @param scenario_name Name of the scenario (must match RData file)
#' @param output_dir Output directory (default: results/scenario_reports/[scenario_name])
#' @param baseline_name Name of baseline scenario (default: "baseline")
#' @return List with paths to generated files
generate_scenario_report <- function(scenario_name,
                                     output_dir = NULL,
                                     baseline_name = "baseline") {
  
  cat("\n=============================================================================\n")
  cat("GENERATING SCENARIO REPORT\n")
  cat("=============================================================================\n")
  cat(sprintf("Scenario: %s vs %s\n\n", scenario_name, baseline_name))
  
  # Set output directory (same as data: results/scenarios/{scenario_name}/)
  if (is.null(output_dir)) {
    output_dir <- sprintf("results/scenarios/%s", scenario_name)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load centralized configuration if not already loaded (includes MFN_SUFFIX)
  if (!exists("MFN_SUFFIX")) {
    source("code/date_config.R")
  }

  # =============================================================================
  # ALWAYS LOAD THREE DATASETS:
  # 1. HTS Schedule Baseline (schedule24_baseline) - ALWAYS the reference
  # 2. Baseline with chosen MFN source (e.g., boe24_baseline)
  # 3. Scenario with chosen MFN source (e.g., boe24_ieepa_strike_down)
  # =============================================================================

  # File paths
  hts_baseline_file <- sprintf("results/processed_us_imports_with_rates_schedule24_%s.RData", baseline_name)
  mfn_baseline_file <- sprintf("results/processed_us_imports_with_rates_%s_%s.RData", MFN_SUFFIX, baseline_name)

  # Scenario file: check output_dir first (if provided), then scenarios subfolder
  scenario_basename <- paste0("processed_us_imports_with_rates_", MFN_SUFFIX, "_", scenario_name)
  if (!is.null(output_dir) && file.exists(file.path(output_dir, sprintf("%s.RData", scenario_basename)))) {
    scenario_file <- file.path(output_dir, sprintf("%s.RData", scenario_basename))
  } else {
    scenario_file <- sprintf("results/scenarios/%s/%s.RData", scenario_name, scenario_basename)
  }

  # Check file existence
  if (!file.exists(hts_baseline_file)) {
    stop(sprintf("HTS Schedule baseline file not found: %s\nRun: SCENARIO_NAME=baseline MFN_RATE_SOURCE=hts_schedule Rscript code/us_tariff_calculation_db.R", hts_baseline_file))
  }
  if (!file.exists(mfn_baseline_file)) {
    stop(sprintf("MFN baseline file not found: %s", mfn_baseline_file))
  }
  if (!file.exists(scenario_file)) {
    stop(sprintf("Scenario file not found: %s", scenario_file))
  }

  # Determine if using alternative MFN source (not HTS schedule)
  using_alt_mfn <- (MFN_SUFFIX != "schedule24")

  # Load HTS Schedule baseline (always the reference)
  cat(sprintf("Loading HTS Schedule baseline: %s\n", hts_baseline_file))
  load(hts_baseline_file)
  hts_baseline <- as.data.table(us_imports)
  rm(us_imports)

  # Load MFN baseline (may be same as HTS baseline if MFN_SUFFIX == "schedule24")
  if (using_alt_mfn) {
    cat(sprintf("Loading %s baseline: %s\n", MFN_SUFFIX, mfn_baseline_file))
    load(mfn_baseline_file)
    mfn_baseline <- as.data.table(us_imports)
    rm(us_imports)
  } else {
    # If using HTS schedule, MFN baseline is same as HTS baseline
    mfn_baseline <- hts_baseline
  }

  # Load scenario
  cat(sprintf("Loading scenario: %s\n", scenario_file))
  load(scenario_file)
  scenario <- as.data.table(us_imports)
  rm(us_imports)

  # For backward compatibility, keep 'baseline' as the MFN baseline
  baseline <- mfn_baseline

  cat(sprintf("Loaded: HTS baseline (%d flows), MFN baseline (%d flows), Scenario (%d flows)\n",
              nrow(hts_baseline), nrow(mfn_baseline), nrow(scenario)))
  
  # =============================================================================
  # CALCULATE CHANGES
  # =============================================================================
  
  cat("\n1. Calculating rate changes...\n")
  
  # Determine safe join keys (older exports may not include un_code)
  join_keys <- c("hs_8digit", "un_code")
  if (!all(join_keys %in% names(baseline)) || !all(join_keys %in% names(scenario))) {
    join_keys <- c("hs_8digit", "iso_code")
  }
  if (!all(join_keys %in% names(baseline)) || !all(join_keys %in% names(scenario))) {
    stop(sprintf(
      "Cannot merge baseline and scenario: missing join keys. Need either {hs_8digit, un_code} or {hs_8digit, iso_code}. Baseline has: %s; Scenario has: %s",
      paste(names(baseline), collapse = ", "),
      paste(names(scenario), collapse = ", ")
    ))
  }

  # Build robust subsets (older exports may not include un_code)
  base_cols <- intersect(
    unique(c(
      join_keys,
      "exporter", "us_imports_bn",
      "hts_rate", "s232_rate", "ieepa_rate", "emergency_rate", "s301_rate", "rate"
    )),
    names(baseline)
  )
  scen_cols <- intersect(
    unique(c(
      join_keys,
      "hts_rate", "s232_rate", "ieepa_rate", "emergency_rate", "s301_rate", "rate"
    )),
    names(scenario)
  )

  base_dt <- baseline[, ..base_cols]
  scen_dt <- scenario[, ..scen_cols]

  # Rename rate columns to *_base / *_scen
  base_rename <- c(
    "hts_rate" = "hts_rate_base",
    "s232_rate" = "s232_rate_base",
    "ieepa_rate" = "ieepa_rate_base",
    "emergency_rate" = "emergency_rate_base",
    "s301_rate" = "s301_rate_base",
    "rate" = "rate_base"
  )
  scen_rename <- c(
    "hts_rate" = "hts_rate_scen",
    "s232_rate" = "s232_rate_scen",
    "ieepa_rate" = "ieepa_rate_scen",
    "emergency_rate" = "emergency_rate_scen",
    "s301_rate" = "s301_rate_scen",
    "rate" = "rate_scen"
  )

  base_present <- intersect(names(base_rename), names(base_dt))
  if (length(base_present) > 0) {
    setnames(base_dt, old = base_present, new = unname(base_rename[base_present]))
  }
  scen_present <- intersect(names(scen_rename), names(scen_dt))
  if (length(scen_present) > 0) {
    setnames(scen_dt, old = scen_present, new = unname(scen_rename[scen_present]))
  }

  merged <- merge(base_dt, scen_dt, by = join_keys)
  
  # Calculate differences
  merged[, `:=`(
    rate_change = rate_scen - rate_base,
    hts_change = hts_rate_scen - hts_rate_base,
    s232_change = s232_rate_scen - s232_rate_base,
    ieepa_change = ieepa_rate_scen - ieepa_rate_base,
    emergency_change = emergency_rate_scen - emergency_rate_base,
    s301_change = s301_rate_scen - s301_rate_base
  )]
  
  # =============================================================================
  # AGGREGATE STATISTICS (for all three datasets)
  # =============================================================================

  cat("2. Computing aggregate statistics...\n")

  total_imports <- sum(merged$us_imports_bn)

  # Weighted averages for MFN baseline and scenario
  baseline_avg <- sum(merged$rate_base * merged$us_imports_bn) / total_imports
  scenario_avg <- sum(merged$rate_scen * merged$us_imports_bn) / total_imports

  # Component averages for MFN baseline and scenario
  stats <- list(
    total_imports_bn = total_imports,
    baseline_rate = baseline_avg,
    scenario_rate = scenario_avg,
    rate_change = scenario_avg - baseline_avg,
    hts_base = sum(merged$hts_rate_base * merged$us_imports_bn) / total_imports,
    hts_scen = sum(merged$hts_rate_scen * merged$us_imports_bn) / total_imports,
    s232_base = sum(merged$s232_rate_base * merged$us_imports_bn) / total_imports,
    s232_scen = sum(merged$s232_rate_scen * merged$us_imports_bn) / total_imports,
    ieepa_base = sum(merged$ieepa_rate_base * merged$us_imports_bn) / total_imports,
    ieepa_scen = sum(merged$ieepa_rate_scen * merged$us_imports_bn) / total_imports,
    emergency_base = sum(merged$emergency_rate_base * merged$us_imports_bn) / total_imports,
    emergency_scen = sum(merged$emergency_rate_scen * merged$us_imports_bn) / total_imports,
    s301_base = sum(merged$s301_rate_base * merged$us_imports_bn) / total_imports,
    s301_scen = sum(merged$s301_rate_scen * merged$us_imports_bn) / total_imports
  )

  # =============================================================================
  # HTS SCHEDULE BASELINE STATISTICS (always the reference)
  # =============================================================================

  hts_total_imports <- sum(hts_baseline$us_imports_bn)
  hts_avg <- sum(hts_baseline$rate * hts_baseline$us_imports_bn) / hts_total_imports

  hts_stats <- list(
    total_rate = hts_avg,
    hts_rate = sum(hts_baseline$hts_rate * hts_baseline$us_imports_bn) / hts_total_imports,
    s232_rate = sum(hts_baseline$s232_rate * hts_baseline$us_imports_bn) / hts_total_imports,
    ieepa_rate = sum(hts_baseline$ieepa_rate * hts_baseline$us_imports_bn) / hts_total_imports,
    emergency_rate = sum(hts_baseline$emergency_rate * hts_baseline$us_imports_bn) / hts_total_imports,
    s301_rate = sum(hts_baseline$s301_rate * hts_baseline$us_imports_bn) / hts_total_imports
  )

  cat(sprintf("   HTS baseline: %.2f%%, MFN baseline: %.2f%%, Scenario: %.2f%%\n",
              hts_stats$total_rate, stats$baseline_rate, stats$scenario_rate))

  # =============================================================================
  # COMPUTE CONTRIBUTIONS (formula-based, using shared function)
  # =============================================================================
  # These contributions are calculated so they SUM to the total rate

  cat("   Computing tariff contributions (formula-based)...\n")

  # Source shared contribution calculation function
  source("code/compute_contributions.R")

  # Compute contributions for HTS baseline, MFN baseline, and scenario
  hts_baseline <- compute_contributions(hts_baseline, verbose = FALSE)
  baseline <- compute_contributions(baseline, verbose = FALSE)
  scenario <- compute_contributions(scenario, verbose = FALSE)

  # Get aggregated contributions (these add up to total rate)
  contrib_hts <- aggregate_contributions(hts_baseline)
  contrib_base <- aggregate_contributions(baseline)
  contrib_scen <- aggregate_contributions(scenario)

  # Get S232 sub-type breakdown (these add up to total S232 contribution)
  s232_hts_subtypes <- aggregate_s232_subtypes(hts_baseline, hts_total_imports)
  s232_base_subtypes <- aggregate_s232_subtypes(baseline, total_imports)
  s232_scen_subtypes <- aggregate_s232_subtypes(scenario, total_imports)

  # Structure for easier access in markdown generation (now with HTS)
  contrib <- list(
    # HTS Schedule baseline (always the reference)
    hts_hts = contrib_hts$hts_layer,
    s232_hts = contrib_hts$s232_layer,
    ieepa_baseline_hts = contrib_hts$ieepa_baseline_layer,
    ieepa_topup_hts = contrib_hts$ieepa_topup_layer,
    emergency_hts = contrib_hts$emergency_layer,
    s301_hts = contrib_hts$s301_layer,
    # MFN baseline (chosen MFN source)
    hts_base = contrib_base$hts_layer,
    hts_scen = contrib_scen$hts_layer,
    s232_base = contrib_base$s232_layer,
    s232_scen = contrib_scen$s232_layer,
    ieepa_baseline_base = contrib_base$ieepa_baseline_layer,
    ieepa_baseline_scen = contrib_scen$ieepa_baseline_layer,
    ieepa_topup_base = contrib_base$ieepa_topup_layer,
    ieepa_topup_scen = contrib_scen$ieepa_topup_layer,
    emergency_base = contrib_base$emergency_layer,
    emergency_scen = contrib_scen$emergency_layer,
    s301_base = contrib_base$s301_layer,
    s301_scen = contrib_scen$s301_layer
  )

  s232_breakdown <- list(
    # HTS baseline
    steel_hts = s232_hts_subtypes$steel,
    alu_hts = s232_hts_subtypes$aluminum,
    copper_hts = s232_hts_subtypes$copper,
    lumber_hts = s232_hts_subtypes$lumber,
    auto_hts = s232_hts_subtypes$auto,
    mhdv_hts = s232_hts_subtypes$mhdv_parts,
    # MFN baseline
    steel_base = s232_base_subtypes$steel,
    steel_scen = s232_scen_subtypes$steel,
    alu_base = s232_base_subtypes$aluminum,
    alu_scen = s232_scen_subtypes$aluminum,
    copper_base = s232_base_subtypes$copper,
    copper_scen = s232_scen_subtypes$copper,
    lumber_base = s232_base_subtypes$lumber,
    lumber_scen = s232_scen_subtypes$lumber,
    auto_base = s232_base_subtypes$auto,
    auto_scen = s232_scen_subtypes$auto,
    mhdv_base = s232_base_subtypes$mhdv_parts,
    mhdv_scen = s232_scen_subtypes$mhdv_parts
  )

  ieepa_breakdown <- list(
    baseline_10_hts = contrib$ieepa_baseline_hts,
    baseline_10_base = contrib$ieepa_baseline_base,
    baseline_10_scen = contrib$ieepa_baseline_scen,
    topup_hts = contrib$ieepa_topup_hts,
    topup_base = contrib$ieepa_topup_base,
    topup_scen = contrib$ieepa_topup_scen
  )

  # Verify contributions sum to total
  sum_hts <- contrib$hts_hts + contrib$s232_hts + contrib$ieepa_baseline_hts +
             contrib$ieepa_topup_hts + contrib$emergency_hts + contrib$s301_hts
  sum_base <- contrib$hts_base + contrib$s232_base + contrib$ieepa_baseline_base +
              contrib$ieepa_topup_base + contrib$emergency_base + contrib$s301_base
  cat(sprintf("   HTS contribution sum: %.2f%% (total: %.2f%%)\n", sum_hts, hts_stats$total_rate))
  cat(sprintf("   MFN contribution sum: %.2f%% (total: %.2f%%)\n", sum_base, stats$baseline_rate))

  # Note: Top 20 breakdowns removed for concise memo format
  # Can be re-enabled if detailed analysis is needed
  
  # =============================================================================
  # GENERATE COMPARISON CHART (with 3 bars when using alt MFN source)
  # =============================================================================

  cat("3. Generating comparison chart...\n")

  # Create MFN source display name
  mfn_display <- switch(MFN_SUFFIX,
    "schedule24" = "HTS Schedule",
    "boe24" = "BoE 2024",
    "bilateral24" = "Bilateral 2024",
    "product24" = "Product 2024",
    "mixed24" = "Mixed 2024",
    MFN_SUFFIX
  )

  # Prepare data for chart (3 bars when using alt MFN, 2 bars otherwise)
  if (using_alt_mfn) {
    chart_data <- data.frame(
      Scenario = c("HTS Schedule", paste0("Baseline (", mfn_display, ")"), scenario_name),
      HTS = c(contrib$hts_hts, contrib$hts_base, contrib$hts_scen),
      IEEPA = c(contrib$ieepa_baseline_hts + contrib$ieepa_topup_hts,
                contrib$ieepa_baseline_base + contrib$ieepa_topup_base,
                contrib$ieepa_baseline_scen + contrib$ieepa_topup_scen),
      S232 = c(contrib$s232_hts, contrib$s232_base, contrib$s232_scen),
      Emergency = c(contrib$emergency_hts, contrib$emergency_base, contrib$emergency_scen),
      S301 = c(contrib$s301_hts, contrib$s301_base, contrib$s301_scen)
    )
    chart_levels <- c(scenario_name, paste0("Baseline (", mfn_display, ")"), "HTS Schedule")
    chart_subtitle <- sprintf("HTS: %.2f%% → %s: %.2f%% → Scenario: %.2f%%",
                              hts_stats$total_rate, mfn_display, stats$baseline_rate, stats$scenario_rate)
  } else {
    chart_data <- data.frame(
      Scenario = c("Baseline", scenario_name),
      HTS = c(contrib$hts_base, contrib$hts_scen),
      IEEPA = c(contrib$ieepa_baseline_base + contrib$ieepa_topup_base,
                contrib$ieepa_baseline_scen + contrib$ieepa_topup_scen),
      S232 = c(contrib$s232_base, contrib$s232_scen),
      Emergency = c(contrib$emergency_base, contrib$emergency_scen),
      S301 = c(contrib$s301_base, contrib$s301_scen)
    )
    chart_levels <- c(scenario_name, "Baseline")
    chart_subtitle <- sprintf("Change: %.2f pp (%.2f%% to %.2f%%)",
                              stats$rate_change, stats$baseline_rate, stats$scenario_rate)
  }

  chart_long <- chart_data %>%
    pivot_longer(cols = c(HTS, IEEPA, S232, Emergency, S301),
                 names_to = "Component", values_to = "Rate") %>%
    mutate(Component = factor(Component, levels = c("S301", "Emergency", "S232", "IEEPA", "HTS")),
           Scenario = factor(Scenario, levels = chart_levels))

  # Color scheme
  layer_colors <- c(
    "HTS" = "#A8D5E5",
    "IEEPA" = "#2171C2",
    "S232" = "#F5A623",
    "Emergency" = "#8B0000",
    "S301" = "#DC3545"
  )

  # Create chart (adjust height based on number of bars)
  chart_height <- ifelse(using_alt_mfn, 5, 4)

  p <- ggplot(chart_long, aes(x = Scenario, y = Rate, fill = Component)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", Rate)),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = layer_colors, guide = guide_legend(reverse = TRUE)) +
    labs(title = sprintf("Weighted Average Tariff Rate: %s", scenario_name),
         subtitle = chart_subtitle,
         x = NULL, y = "Weighted Average Rate (%)", fill = "Component") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )

  chart_file <- file.path(output_dir, sprintf("comparison_%s.png", scenario_name))
  ggsave(chart_file, p, width = 8, height = chart_height, dpi = 150)
  cat(sprintf("   Saved: %s\n", chart_file))
  
  # =============================================================================
  # DETECT CHANGED PARAMETERS
  # =============================================================================
  
  cat("4. Detecting changed parameters...\n")
  
  all_rates <- fread("data/scenarios/rates.csv")
  scenario_rates <- all_rates[scenario == scenario_name]
  baseline_rates <- all_rates[scenario == baseline_name]
  
  # Find rates that differ
  changed_params <- merge(
    scenario_rates[, .(rate_type, rate_scen = rate, desc_scen = description)],
    baseline_rates[, .(rate_type, rate_base = rate, desc_base = description)],
    by = "rate_type", all.x = TRUE
  )
  changed_params <- changed_params[is.na(rate_base) | rate_scen != rate_base]
  changed_params[is.na(rate_base), rate_base := "N/A"]
  
  cat(sprintf("   Changed parameters: %d\n", nrow(changed_params)))
  
  # =============================================================================
  # CALCULATE TRADE BREAKDOWN STATS (for all three datasets)
  # =============================================================================

  # Calculate trade by rate type for HTS baseline, MFN baseline and scenario
  # MFN only = no additional tariffs applied (s232 + ieepa + emergency + s301 all zero)
  hts_mfn_only <- sum(hts_baseline[s232_rate == 0 & ieepa_rate == 0 & emergency_rate == 0 & s301_rate == 0]$us_imports_bn)
  baseline_mfn_only <- sum(baseline[s232_rate == 0 & ieepa_rate == 0 & emergency_rate == 0 & s301_rate == 0]$us_imports_bn)
  scenario_mfn_only <- sum(scenario[s232_rate == 0 & ieepa_rate == 0 & emergency_rate == 0 & s301_rate == 0]$us_imports_bn)

  # S232 applied = has s232 rate > 0
  hts_s232 <- sum(hts_baseline[s232_rate > 0]$us_imports_bn)
  baseline_s232 <- sum(baseline[s232_rate > 0]$us_imports_bn)
  scenario_s232 <- sum(scenario[s232_rate > 0]$us_imports_bn)

  # IEEPA applied = has ieepa rate > 0
  hts_ieepa <- sum(hts_baseline[ieepa_rate > 0]$us_imports_bn)
  baseline_ieepa <- sum(baseline[ieepa_rate > 0]$us_imports_bn)
  scenario_ieepa <- sum(scenario[ieepa_rate > 0]$us_imports_bn)
  
  # =============================================================================
  # GENERATE MARKDOWN REPORT
  # =============================================================================
  
  cat("5. Generating markdown report...\n")
  
  # Author and date settings
  author <- "Johannes Fritz"
  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")
  report_date <- sprintf("%s %d, %s", 
                         month_names[as.integer(format(Sys.Date(), "%m"))],
                         as.integer(format(Sys.Date(), "%d")),
                         format(Sys.Date(), "%Y"))
  
  # Format scenario name for display (replace underscores with spaces, handle acronyms)
  scenario_display <- gsub("_", " ", scenario_name)
  scenario_display <- tools::toTitleCase(scenario_display)
  # Fix common acronyms that should be uppercase
  scenario_display <- gsub("\\bIeepa\\b", "IEEPA", scenario_display)
  scenario_display <- gsub("\\bS232\\b", "S232", scenario_display)
  scenario_display <- gsub("\\bS301\\b", "S301", scenario_display)
  scenario_display <- gsub("\\bUsmca\\b", "USMCA", scenario_display)
  scenario_display <- gsub("\\bMfn\\b", "MFN", scenario_display)
  
  # Build markdown using template format
  md <- c(
    sprintf("# Scenario: %s", scenario_display),
    "",
    sprintf("**Date:** %s", report_date),
    sprintf("**Author:** %s", author),
    sprintf("**Scenario ID:** `%s`", scenario_name),
    "",
    "## Key Assumptions",
    "",
    sprintf("This scenario modifies %d rate parameters relative to baseline.", nrow(changed_params)),
    ""
  )
  
  # Categorize changed parameters into human-readable groups
  if (nrow(changed_params) > 0) {
    # Count by category
    ieepa_baseline <- sum(grepl("^ieepa_baseline", changed_params$rate_type))
    ieepa_topups <- sum(grepl("^ieepa_topup", changed_params$rate_type))
    emergency <- sum(grepl("emergency|opioid", changed_params$rate_type))
    floors <- sum(grepl("floor", changed_params$rate_type))
    special <- sum(grepl("bra_topup|india_russia", changed_params$rate_type))
    
    # Determine if this is a "zero-out" scenario (all scenario values are 0)
    all_zeroed <- all(changed_params$rate_scen == 0)
    
    if (all_zeroed) {
      md <- c(md, "The scenario sets the following rate parameters to **zero**:", "")
    } else {
      md <- c(md, "The scenario modifies the following rate parameters:", "")
    }
    
    md <- c(md,
            "| Category | Description |",
            "|----------|-------------|")
    
    if (ieepa_baseline > 0) {
      md <- c(md, "| **IEEPA Baseline** | Universal baseline reciprocal rate |")
    }
    if (ieepa_topups > 0) {
      md <- c(md, sprintf("| **Country Top-ups** | %d country-specific reciprocal top-up rates |", ieepa_topups))
    }
    if (emergency > 0) {
      emergency_types <- unique(gsub("_rate$", "", changed_params$rate_type[grepl("emergency|opioid", changed_params$rate_type)]))
      emergency_desc <- paste(gsub("_", " ", emergency_types), collapse = ", ")
      # Clean up country abbreviations
      emergency_desc <- gsub("\\bcan\\b", "Canada", emergency_desc, ignore.case = TRUE)
      emergency_desc <- gsub("\\bmex\\b", "Mexico", emergency_desc, ignore.case = TRUE)
      emergency_desc <- gsub("\\bchina\\b", "China", emergency_desc, ignore.case = TRUE)
      emergency_desc <- tools::toTitleCase(emergency_desc)
      md <- c(md, sprintf("| **Emergency Orders** | %s |", emergency_desc))
    }
    if (floors > 0) {
      floor_countries <- unique(gsub("_.*", "", changed_params$rate_type[grepl("floor", changed_params$rate_type)]))
      floor_countries <- toupper(floor_countries)
      md <- c(md, sprintf("| **Country Floors** | %s IEEPA floor rates |", paste(floor_countries, collapse = ", ")))
    }
    if (special > 0) {
      md <- c(md, "| **Special Top-ups** | India/Russia top-up, Brazil top-up |")
    }
  }
  
  # Add "What Remains in Effect" section
  md <- c(md, "", "## What Remains in Effect", "",
          "- **Section 232 tariffs** on steel, aluminum, copper, lumber, and automobiles",
          "- **Section 232 floor rates** for EU/Japan (S232 auto/MHDV floors)",
          "- **Section 301 tariffs** on China (separate statutory authority)",
          "- **MFN tariff rates** from the US tariff schedule",
          "- **USMCA preferential treatment** for compliant Canada/Mexico trade")
  
  # Results Summary - format change values with sign, use "No change" for zero
  mfn_change <- scenario_mfn_only - baseline_mfn_only
  s232_change <- scenario_s232 - baseline_s232
  ieepa_change <- scenario_ieepa - baseline_ieepa

  format_change <- function(val) {
    if (abs(val) < 1) return("No change")
    if (val >= 0) sprintf("+$%.0fB", val) else sprintf("-$%.0fB", abs(val))
  }

  format_rate_change <- function(val) {
    if (abs(val) < 0.01) return("No change")
    sprintf("%+.2f pp", val)
  }

  # Build tables conditionally based on whether using alternative MFN source
  if (using_alt_mfn) {
    # THREE-COLUMN TABLES: HTS Schedule | MFN Baseline | Scenario | Change (vs HTS)
    mfn_baseline_label <- paste0("Baseline (", mfn_display, ")")

    md <- c(md, "", "\\newpage", "", "## Results Summary", "",
            sprintf("*Note: Using %s as 2024 MFN rate source. HTS Schedule column shows reference baseline.*", mfn_display),
            "",
            sprintf("| Metric | HTS Schedule | %s | %s | Δ vs HTS |", mfn_baseline_label, scenario_display),
            "|--------|--------------|----------|----------|----------|",
            sprintf("| **Weighted Average Rate** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    hts_stats$total_rate, stats$baseline_rate, stats$scenario_rate,
                    format_rate_change(stats$scenario_rate - hts_stats$total_rate)),
            sprintf("| **MFN Only Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    hts_mfn_only, 100 * hts_mfn_only / hts_total_imports,
                    baseline_mfn_only, 100 * baseline_mfn_only / stats$total_imports_bn,
                    scenario_mfn_only, 100 * scenario_mfn_only / stats$total_imports_bn,
                    format_change(scenario_mfn_only - hts_mfn_only)),
            sprintf("| **S232 Applied Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    hts_s232, 100 * hts_s232 / hts_total_imports,
                    baseline_s232, 100 * baseline_s232 / stats$total_imports_bn,
                    scenario_s232, 100 * scenario_s232 / stats$total_imports_bn,
                    format_change(scenario_s232 - hts_s232)),
            sprintf("| **IEEPA Applied Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    hts_ieepa, 100 * hts_ieepa / hts_total_imports,
                    baseline_ieepa, 100 * baseline_ieepa / stats$total_imports_bn,
                    scenario_ieepa, 100 * scenario_ieepa / stats$total_imports_bn,
                    format_change(scenario_ieepa - hts_ieepa)),
            "",
            "## Tariff Component Breakdown",
            "",
            "The following table shows trade-weighted tariff contributions by component (these SUM to total rate).",
            "",
            sprintf("| Component | HTS Schedule | %s | %s | Δ vs HTS |", mfn_baseline_label, scenario_display),
            "|-----------|--------------|----------|----------|----------|",
            sprintf("| **HTS (MFN)** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    contrib$hts_hts, contrib$hts_base, contrib$hts_scen,
                    format_rate_change(contrib$hts_scen - contrib$hts_hts)),
            sprintf("| **Section 232** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    contrib$s232_hts, contrib$s232_base, contrib$s232_scen,
                    format_rate_change(contrib$s232_scen - contrib$s232_hts)),
            sprintf("| &nbsp;&nbsp;Steel | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$steel_hts, s232_breakdown$steel_base, s232_breakdown$steel_scen,
                    format_rate_change(s232_breakdown$steel_scen - s232_breakdown$steel_hts)),
            sprintf("| &nbsp;&nbsp;Aluminum | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$alu_hts, s232_breakdown$alu_base, s232_breakdown$alu_scen,
                    format_rate_change(s232_breakdown$alu_scen - s232_breakdown$alu_hts)),
            sprintf("| &nbsp;&nbsp;Copper | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$copper_hts, s232_breakdown$copper_base, s232_breakdown$copper_scen,
                    format_rate_change(s232_breakdown$copper_scen - s232_breakdown$copper_hts)),
            sprintf("| &nbsp;&nbsp;Lumber | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$lumber_hts, s232_breakdown$lumber_base, s232_breakdown$lumber_scen,
                    format_rate_change(s232_breakdown$lumber_scen - s232_breakdown$lumber_hts)),
            sprintf("| &nbsp;&nbsp;Automobiles | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$auto_hts, s232_breakdown$auto_base, s232_breakdown$auto_scen,
                    format_rate_change(s232_breakdown$auto_scen - s232_breakdown$auto_hts)),
            sprintf("| &nbsp;&nbsp;MHDV/Parts | %.2f%% | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$mhdv_hts, s232_breakdown$mhdv_base, s232_breakdown$mhdv_scen,
                    format_rate_change(s232_breakdown$mhdv_scen - s232_breakdown$mhdv_hts)),
            sprintf("| **IEEPA** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    contrib$ieepa_baseline_hts + contrib$ieepa_topup_hts,
                    contrib$ieepa_baseline_base + contrib$ieepa_topup_base,
                    contrib$ieepa_baseline_scen + contrib$ieepa_topup_scen,
                    format_rate_change((contrib$ieepa_baseline_scen + contrib$ieepa_topup_scen) -
                                       (contrib$ieepa_baseline_hts + contrib$ieepa_topup_hts))),
            sprintf("| &nbsp;&nbsp;Baseline (10%%) | %.2f%% | %.2f%% | %.2f%% | %s |",
                    ieepa_breakdown$baseline_10_hts, ieepa_breakdown$baseline_10_base, ieepa_breakdown$baseline_10_scen,
                    format_rate_change(ieepa_breakdown$baseline_10_scen - ieepa_breakdown$baseline_10_hts)),
            sprintf("| &nbsp;&nbsp;Country Top-ups | %.2f%% | %.2f%% | %.2f%% | %s |",
                    ieepa_breakdown$topup_hts, ieepa_breakdown$topup_base, ieepa_breakdown$topup_scen,
                    format_rate_change(ieepa_breakdown$topup_scen - ieepa_breakdown$topup_hts)),
            sprintf("| **Emergency Orders** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    contrib$emergency_hts, contrib$emergency_base, contrib$emergency_scen,
                    format_rate_change(contrib$emergency_scen - contrib$emergency_hts)),
            sprintf("| **Section 301** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    contrib$s301_hts, contrib$s301_base, contrib$s301_scen,
                    format_rate_change(contrib$s301_scen - contrib$s301_hts)),
            sprintf("| **TOTAL** | %.2f%% | %.2f%% | %.2f%% | %s |",
                    hts_stats$total_rate, stats$baseline_rate, stats$scenario_rate,
                    format_rate_change(stats$scenario_rate - hts_stats$total_rate)),
            "")
  } else {
    # TWO-COLUMN TABLES: Baseline | Scenario | Change (original format)
    md <- c(md, "", "\\newpage", "", "## Results Summary", "",
            sprintf("| Metric | Baseline | %s | Change |", scenario_display),
            "|--------|----------|----------|--------|",
            sprintf("| **Weighted Average Rate** | %.2f%% | %.2f%% | %s |",
                    stats$baseline_rate, stats$scenario_rate, format_rate_change(stats$rate_change)),
            sprintf("| **MFN Only Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    baseline_mfn_only, 100 * baseline_mfn_only / stats$total_imports_bn,
                    scenario_mfn_only, 100 * scenario_mfn_only / stats$total_imports_bn,
                    format_change(mfn_change)),
            sprintf("| **S232 Applied Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    baseline_s232, 100 * baseline_s232 / stats$total_imports_bn,
                    scenario_s232, 100 * scenario_s232 / stats$total_imports_bn,
                    format_change(s232_change)),
            sprintf("| **IEEPA Applied Trade** | $%.0fB (%.0f%%) | $%.0fB (%.0f%%) | %s |",
                    baseline_ieepa, 100 * baseline_ieepa / stats$total_imports_bn,
                    scenario_ieepa, 100 * scenario_ieepa / stats$total_imports_bn,
                    format_change(ieepa_change)),
            "",
            "## Tariff Component Breakdown",
            "",
            "The following table shows trade-weighted tariff contributions by component for total US imports (these SUM to total rate).",
            "Components showing 'No change' verify scenario isolation (changes to one layer don't affect others).",
            "",
            sprintf("| Component | Baseline | %s | Change |", scenario_display),
            "|-----------|----------|----------|--------|",
            sprintf("| **HTS (MFN)** | %.2f%% | %.2f%% | %s |",
                    contrib$hts_base, contrib$hts_scen, format_rate_change(contrib$hts_scen - contrib$hts_base)),
            "| **Section 232** | | | |",
            sprintf("| &nbsp;&nbsp;Steel | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$steel_base, s232_breakdown$steel_scen,
                    format_rate_change(s232_breakdown$steel_scen - s232_breakdown$steel_base)),
            sprintf("| &nbsp;&nbsp;Aluminum | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$alu_base, s232_breakdown$alu_scen,
                    format_rate_change(s232_breakdown$alu_scen - s232_breakdown$alu_base)),
            sprintf("| &nbsp;&nbsp;Copper | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$copper_base, s232_breakdown$copper_scen,
                    format_rate_change(s232_breakdown$copper_scen - s232_breakdown$copper_base)),
            sprintf("| &nbsp;&nbsp;Lumber | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$lumber_base, s232_breakdown$lumber_scen,
                    format_rate_change(s232_breakdown$lumber_scen - s232_breakdown$lumber_base)),
            sprintf("| &nbsp;&nbsp;Automobiles | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$auto_base, s232_breakdown$auto_scen,
                    format_rate_change(s232_breakdown$auto_scen - s232_breakdown$auto_base)),
            sprintf("| &nbsp;&nbsp;MHDV/Parts | %.2f%% | %.2f%% | %s |",
                    s232_breakdown$mhdv_base, s232_breakdown$mhdv_scen,
                    format_rate_change(s232_breakdown$mhdv_scen - s232_breakdown$mhdv_base)),
            "| **IEEPA** | | | |",
            sprintf("| &nbsp;&nbsp;Baseline (10%%) | %.2f%% | %.2f%% | %s |",
                    ieepa_breakdown$baseline_10_base, ieepa_breakdown$baseline_10_scen,
                    format_rate_change(ieepa_breakdown$baseline_10_scen - ieepa_breakdown$baseline_10_base)),
            sprintf("| &nbsp;&nbsp;Country Top-ups | %.2f%% | %.2f%% | %s |",
                    ieepa_breakdown$topup_base, ieepa_breakdown$topup_scen,
                    format_rate_change(ieepa_breakdown$topup_scen - ieepa_breakdown$topup_base)),
            sprintf("| **Emergency Orders** | %.2f%% | %.2f%% | %s |",
                    contrib$emergency_base, contrib$emergency_scen,
                    format_rate_change(contrib$emergency_scen - contrib$emergency_base)),
            sprintf("| **Section 301** | %.2f%% | %.2f%% | %s |",
                    contrib$s301_base, contrib$s301_scen,
                    format_rate_change(contrib$s301_scen - contrib$s301_base)),
            sprintf("| **TOTAL** | %.2f%% | %.2f%% | %s |",
                    stats$baseline_rate, stats$scenario_rate, format_rate_change(stats$rate_change)),
            "")
  }
  
  # Write markdown file
  md_file <- file.path(output_dir, sprintf("scenario_report_%s.md", scenario_name))
  writeLines(md, md_file)
  cat(sprintf("   Saved: %s\n", md_file))
  
  # =============================================================================
  # GENERATE DOCX
  # =============================================================================
  
  cat("6. Generating DOCX...\n")
  
  docx_file <- file.path(output_dir, sprintf("scenario_report_%s.docx", scenario_name))
  template_file <- "documentation/SGEPT - US Tariff Barrier Estimates - Assumptions Summary.docx"
  
  # Get absolute path for pandoc (needs full path for image embedding)
  abs_output_dir <- normalizePath(output_dir)
  abs_md_file <- normalizePath(md_file)
  
  # Check if template exists
  if (file.exists(template_file)) {
    abs_template <- normalizePath(template_file)
    pandoc_cmd <- sprintf('pandoc "%s" -o "%s" --from markdown --to docx --standalone --reference-doc="%s" --resource-path="%s"',
                          abs_md_file, docx_file, abs_template, abs_output_dir)
  } else {
    pandoc_cmd <- sprintf('pandoc "%s" -o "%s" --from markdown --to docx --standalone --resource-path="%s"',
                          abs_md_file, docx_file, abs_output_dir)
  }
  
  result <- system(pandoc_cmd)
  if (result == 0) {
    cat(sprintf("   Saved: %s\n", docx_file))
  } else {
    warning("DOCX generation failed - pandoc may not be installed")
  }

  # Convenience copies in results/ with the repo's common naming convention
  # (Some workflows expect scenario_<id>.md/.docx at the results root.)
  root_md <- file.path("results", sprintf("scenario_%s.md", scenario_name))
  file.copy(md_file, root_md, overwrite = TRUE)
  if (file.exists(docx_file)) {
    root_docx <- file.path("results", sprintf("scenario_%s.docx", scenario_name))
    file.copy(docx_file, root_docx, overwrite = TRUE)
  }
  
  # =============================================================================
  # GENERATE PDF FROM DOCX (preserves template formatting)
  # =============================================================================
  
  cat("7. Generating PDF from DOCX...\n")
  
  pdf_file <- file.path(output_dir, sprintf("scenario_report_%s.pdf", scenario_name))
  abs_docx <- normalizePath(docx_file)
  abs_pdf <- file.path(abs_output_dir, sprintf("scenario_report_%s.pdf", scenario_name))
  
  pdf_generated <- FALSE
  
  # Method 1: Try Microsoft Word via AppleScript (macOS)
  if (!pdf_generated && Sys.info()["sysname"] == "Darwin" && 
      file.exists("/Applications/Microsoft Word.app")) {
    
    applescript_cmd <- sprintf('
      osascript -e "
        tell application \\"Microsoft Word\\"
          activate
          open POSIX file \\"%s\\"
          delay 1
          set theDoc to active document
          save as theDoc file name POSIX file \\"%s\\" file format format PDF
          close theDoc saving no
        end tell
      " 2>/dev/null', abs_docx, abs_pdf)
    
    result <- system(applescript_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if (file.exists(abs_pdf)) {
      pdf_generated <- TRUE
      cat(sprintf("   Saved: %s (via Microsoft Word)\n", pdf_file))
    }
  }
  
  # Method 2: Try LibreOffice headless
  if (!pdf_generated) {
    soffice_paths <- c(
      "/Applications/LibreOffice.app/Contents/MacOS/soffice",
      "/usr/bin/soffice",
      "soffice"
    )
    
    for (soffice_path in soffice_paths) {
      if (file.exists(soffice_path) || soffice_path == "soffice") {
        soffice_cmd <- sprintf('"%s" --headless --convert-to pdf --outdir "%s" "%s" 2>/dev/null',
                               soffice_path, abs_output_dir, abs_docx)
        
        result <- system(soffice_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        if (file.exists(abs_pdf)) {
          pdf_generated <- TRUE
          cat(sprintf("   Saved: %s (via LibreOffice)\n", pdf_file))
          break
        }
      }
    }
  }
  
  # Method 3: Try docx2pdf Python package
  if (!pdf_generated) {
    docx2pdf_cmd <- sprintf('docx2pdf "%s" "%s" 2>/dev/null', abs_docx, abs_pdf)
    result <- system(docx2pdf_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if (file.exists(abs_pdf)) {
      pdf_generated <- TRUE
      cat(sprintf("   Saved: %s (via docx2pdf)\n", pdf_file))
    }
  }
  
  if (!pdf_generated) {
    cat("   Note: PDF generation skipped (no converter available)\n")
    cat("   Options: Microsoft Word, LibreOffice, or 'pip install docx2pdf'\n")
    pdf_file <- NULL
  }
  
  # =============================================================================
  # DONE
  # =============================================================================
  
  cat("\n=============================================================================\n")
  cat("SCENARIO REPORT COMPLETE\n")
  cat("=============================================================================\n")
  cat(sprintf("Output directory: %s\n", output_dir))
  cat("Files generated:\n")
  cat(sprintf("  - %s\n", basename(md_file)))
  cat(sprintf("  - %s\n", basename(chart_file)))
  if (file.exists(docx_file)) {
    cat(sprintf("  - %s\n", basename(docx_file)))
  }
  if (!is.null(pdf_file) && file.exists(pdf_file)) {
    cat(sprintf("  - %s\n", basename(pdf_file)))
  }
  cat("\n")
  
  return(list(
    md_file = md_file,
    docx_file = docx_file,
    pdf_file = pdf_file,
    chart_file = chart_file,
    stats = stats,
    hts_stats = hts_stats,
    using_alt_mfn = using_alt_mfn
  ))
}

# =============================================================================
# STANDALONE EXECUTION
# =============================================================================

# If run directly (not sourced), generate report for specified scenario
if (!interactive() && !exists("SCENARIO_NAME")) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1) {
    generate_scenario_report(args[1])
  } else {
    cat("Usage: Rscript generate_scenario_report.R <scenario_name>\n")
  }
}
