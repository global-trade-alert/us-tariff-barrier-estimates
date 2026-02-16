# scenario_flow_comparison.R
# Compare scenario results to baseline at flow level (HS8 x country)
#
# This script provides:
#   1. Flow-level merge of baseline and scenario data
#   2. Rate change calculations (absolute and percentage)
#   3. Trade-weighted impact analysis
#   4. Country and sector summaries
#   5. Excel export with multiple worksheets

library(data.table)
library(openxlsx)

# -----------------------------------------------------------------------------
# Main Comparison Function
# -----------------------------------------------------------------------------

#' Compare scenario results to baseline at flow level
#' @param baseline_path Path to baseline .RData file
#' @param scenario_path Path to scenario .RData file
#' @param scenario_name Name of scenario for labeling
#' @param output_dir Directory for output files
#' @return data.table with flow-level comparison
compare_flows <- function(baseline_path,
                          scenario_path,
                          scenario_name,
                          output_dir = "results") {

  cat(sprintf("\nComparing '%s' to baseline...\n", scenario_name))

  # Load baseline
  cat("  Loading baseline data...\n")
  env_base <- new.env()
  load(baseline_path, envir = env_base)
  baseline <- copy(env_base$trade.data)
  setnames(baseline, "final_rate", "final_rate_baseline")
  setnames(baseline, "ieepa_rate", "ieepa_rate_baseline")
  if ("reciprocal_rate" %in% names(baseline)) {
    setnames(baseline, "reciprocal_rate", "reciprocal_rate_baseline")
  }

  # Load scenario
  cat("  Loading scenario data...\n")
  env_scen <- new.env()
  load(scenario_path, envir = env_scen)
  scenario <- copy(env_scen$trade.data)
  setnames(scenario, "final_rate", "final_rate_scenario")
  setnames(scenario, "ieepa_rate", "ieepa_rate_scenario")
  if ("reciprocal_rate" %in% names(scenario)) {
    setnames(scenario, "reciprocal_rate", "reciprocal_rate_scenario")
  }

  # Select columns for merge
  baseline_cols <- c("hs8", "i", "exporter", "trade_value_usd", "us_imports_bn",
                     "final_rate_baseline", "ieepa_rate_baseline")
  if ("reciprocal_rate_baseline" %in% names(baseline)) {
    baseline_cols <- c(baseline_cols, "reciprocal_rate_baseline")
  }
  baseline <- baseline[, .SD, .SDcols = intersect(baseline_cols, names(baseline))]

  scenario_cols <- c("hs8", "i", "final_rate_scenario", "ieepa_rate_scenario")
  if ("reciprocal_rate_scenario" %in% names(scenario)) {
    scenario_cols <- c(scenario_cols, "reciprocal_rate_scenario")
  }
  scenario <- scenario[, .SD, .SDcols = intersect(scenario_cols, names(scenario))]

  # Merge at flow level
  cat("  Merging at flow level (HS8 x country)...\n")
  comparison <- merge(baseline, scenario, by = c("hs8", "i"), all.x = TRUE)

  # Calculate changes
  cat("  Calculating rate changes...\n")
  comparison[, `:=`(
    rate_change = final_rate_scenario - final_rate_baseline,
    ieepa_change = ieepa_rate_scenario - ieepa_rate_baseline,
    rate_change_pct = fifelse(
      final_rate_baseline > 0,
      (final_rate_scenario - final_rate_baseline) / final_rate_baseline * 100,
      NA_real_
    ),
    trade_value_affected = us_imports_bn * abs(final_rate_scenario - final_rate_baseline) / 100
  )]

  # Summary statistics
  cat("\n  Summary Statistics:\n")
  cat(sprintf("    Total flows: %s\n", format(nrow(comparison), big.mark = ",")))
  cat(sprintf("    Flows with rate change: %s\n",
              format(sum(abs(comparison$rate_change) > 0.01, na.rm = TRUE), big.mark = ",")))

  # Trade-weighted changes
  tw_baseline <- comparison[, weighted.mean(final_rate_baseline, us_imports_bn, na.rm = TRUE)]
  tw_scenario <- comparison[, weighted.mean(final_rate_scenario, us_imports_bn, na.rm = TRUE)]
  cat(sprintf("    Trade-weighted avg rate (baseline): %.2f%%\n", tw_baseline))
  cat(sprintf("    Trade-weighted avg rate (scenario): %.2f%%\n", tw_scenario))
  cat(sprintf("    Trade-weighted change: %.2f pp\n", tw_scenario - tw_baseline))

  # Country summary
  cat("\n  Creating summaries...\n")
  country_summary <- comparison[, .(
    n_flows = .N,
    trade_value_bn = sum(us_imports_bn, na.rm = TRUE),
    avg_baseline_rate = weighted.mean(final_rate_baseline, us_imports_bn, na.rm = TRUE),
    avg_scenario_rate = weighted.mean(final_rate_scenario, us_imports_bn, na.rm = TRUE),
    avg_rate_change = weighted.mean(rate_change, us_imports_bn, na.rm = TRUE),
    total_trade_affected = sum(trade_value_affected, na.rm = TRUE)
  ), by = .(i, exporter)]
  country_summary <- country_summary[order(-trade_value_bn)]

  # Sector summary (by HS2)
  comparison[, hs2 := substr(hs8, 1, 2)]
  sector_summary <- comparison[, .(
    n_flows = .N,
    trade_value_bn = sum(us_imports_bn, na.rm = TRUE),
    avg_baseline_rate = weighted.mean(final_rate_baseline, us_imports_bn, na.rm = TRUE),
    avg_scenario_rate = weighted.mean(final_rate_scenario, us_imports_bn, na.rm = TRUE),
    avg_rate_change = weighted.mean(rate_change, us_imports_bn, na.rm = TRUE),
    total_trade_affected = sum(trade_value_affected, na.rm = TRUE)
  ), by = .(hs2)]
  sector_summary <- sector_summary[order(-trade_value_bn)]

  # Top changes (largest rate decreases/increases by trade value)
  top_decreases <- comparison[rate_change < -0.5][order(rate_change)][1:min(100, .N)]
  top_increases <- comparison[rate_change > 0.5][order(-rate_change)][1:min(100, .N)]

  # Save comparison data
  comparison_file <- file.path(output_dir,
                               sprintf("flow_comparison_%s_vs_baseline.RData", scenario_name))
  save(comparison, country_summary, sector_summary, file = comparison_file)
  cat(sprintf("    Saved RData: %s\n", comparison_file))

  # Excel export
  excel_file <- file.path(output_dir,
                          sprintf("flow_comparison_%s_vs_baseline.xlsx", scenario_name))

  wb <- createWorkbook()

  # Sheet 1: Summary
  addWorksheet(wb, "Summary")
  summary_data <- data.frame(
    Metric = c("Scenario", "Baseline File", "Scenario File", "Total Flows",
               "Flows with Rate Change", "Trade-Weighted Avg (Baseline)",
               "Trade-Weighted Avg (Scenario)", "Trade-Weighted Change"),
    Value = c(scenario_name, basename(baseline_path), basename(scenario_path),
              format(nrow(comparison), big.mark = ","),
              format(sum(abs(comparison$rate_change) > 0.01, na.rm = TRUE), big.mark = ","),
              sprintf("%.2f%%", tw_baseline),
              sprintf("%.2f%%", tw_scenario),
              sprintf("%.2f pp", tw_scenario - tw_baseline))
  )
  writeDataTable(wb, "Summary", summary_data, tableStyle = "TableStyleLight1")
  setColWidths(wb, "Summary", cols = 1:2, widths = c(35, 25))

  # Sheet 2: Country Summary
  addWorksheet(wb, "Country Summary")
  writeDataTable(wb, "Country Summary", country_summary, tableStyle = "TableStyleMedium2")
  freezePane(wb, "Country Summary", firstRow = TRUE)

  # Sheet 3: Sector Summary
  addWorksheet(wb, "Sector Summary")
  writeDataTable(wb, "Sector Summary", sector_summary, tableStyle = "TableStyleMedium2")
  freezePane(wb, "Sector Summary", firstRow = TRUE)

  # Sheet 4: Top Decreases
  if (nrow(top_decreases) > 0) {
    addWorksheet(wb, "Top Rate Decreases")
    writeDataTable(wb, "Top Rate Decreases", top_decreases, tableStyle = "TableStyleMedium3")
    freezePane(wb, "Top Rate Decreases", firstRow = TRUE)
  }

  # Sheet 5: Top Increases
  if (nrow(top_increases) > 0) {
    addWorksheet(wb, "Top Rate Increases")
    writeDataTable(wb, "Top Rate Increases", top_increases, tableStyle = "TableStyleMedium4")
    freezePane(wb, "Top Rate Increases", firstRow = TRUE)
  }

  saveWorkbook(wb, excel_file, overwrite = TRUE)
  cat(sprintf("    Saved Excel: %s\n", excel_file))

  cat("\n  Comparison complete!\n")

  return(comparison)
}

# -----------------------------------------------------------------------------
# Quick Comparison Utility
# -----------------------------------------------------------------------------

#' Quick comparison summary without full Excel export
#' @param baseline_path Path to baseline .RData file
#' @param scenario_path Path to scenario .RData file
quick_compare <- function(baseline_path, scenario_path) {
  env_base <- new.env()
  load(baseline_path, envir = env_base)
  baseline <- copy(env_base$trade.data)

  env_scen <- new.env()
  load(scenario_path, envir = env_scen)
  scenario <- copy(env_scen$trade.data)

  # Trade-weighted averages
  tw_base <- baseline[, weighted.mean(final_rate, us_imports_bn, na.rm = TRUE)]
  tw_scen <- scenario[, weighted.mean(final_rate, us_imports_bn, na.rm = TRUE)]

  cat(sprintf("\nQuick Comparison:\n"))
  cat(sprintf("  Baseline: %.2f%% (trade-weighted avg)\n", tw_base))
  cat(sprintf("  Scenario: %.2f%% (trade-weighted avg)\n", tw_scen))
  cat(sprintf("  Change:   %.2f pp\n", tw_scen - tw_base))

  return(list(baseline = tw_base, scenario = tw_scen, change = tw_scen - tw_base))
}
