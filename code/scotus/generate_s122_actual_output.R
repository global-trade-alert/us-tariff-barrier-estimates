# =============================================================================
# SECTION 122 ACTUAL TARIFF REGIME: OUTPUT GENERATOR
# =============================================================================
# Master orchestrator for the S122 Actual blog output package (Blog Post 3).
# Generates Excel files + blog HTML comparing 4 empirical scenarios:
#   1. Before SCOTUS (pre-ruling baseline, POLICY_DATE=2026-02-19)
#   2. IEEPA Strike Down (POLICY_DATE=2026-02-21)
#   3. S122 @ 10% (initial proclamation, POLICY_DATE=2026-02-24)
#   4. S122 @ 15% (raised surcharge, POLICY_DATE=2026-02-24, schedule24)
#
# Input:
#   - results/260221-260219/...schedule24_baseline.RData        (pre-SCOTUS)
#   - results/260222-260221-schedule24/...schedule24_baseline.RData  (IEEPA-struck)
#   - results/260221-260224/...schedule24_baseline.RData        (S122 @ 10%)
#   - results/260222-260224-schedule24/...schedule24_baseline.RData  (S122 @ 15%)
#
# Output:
#   - results/scotus/s122_actual/charts/   (5 charts + 2 mobile variants)
#   - results/scotus/s122_actual/data/     (2 Excel files)
#   - results/scotus/s122_actual/blog/     (blog HTML for review)
#
# Usage:
#   cd us-tariff-barrier-estimates
#   Rscript code/scotus/generate_s122_actual_output.R
# =============================================================================

cat("=== Section 122 Actual Tariff Regime: Output Generator ===\n")
cat("Generating blog charts, Excel downloads, and HTML draft.\n\n")

# =============================================================================
# 1. SETUP
# =============================================================================

cat("1. Loading dependencies...\n")

library(data.table)
library(ggplot2)
library(cowplot)
library(grid)
library(dplyr)
library(tidyr)
library(openxlsx)

# Source shared functions
source("code/date_config.R")
source("code/compute_contributions.R")
source("code/scotus/scotus_chart_functions.R")

# Create output directories
dirs <- c("results/scotus/s122_actual/charts",
          "results/scotus/s122_actual/data",
          "results/scotus/s122_actual/blog")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("   Output directories created.\n")

# =============================================================================
# 2. LOAD DATA (3 scenarios)
# =============================================================================

cat("\n2. Loading tariff data (3 scenarios)...\n")

# --- Scenario 1: Before SCOTUS (POLICY_DATE=2026-02-19) ---
baseline_file <- "results/260221-260219/processed_us_imports_with_rates_schedule24_baseline.RData"
stopifnot("Pre-SCOTUS RData not found. Run: POLICY_DATE=2026-02-19 Rscript code/run_scenario.R baseline" =
            file.exists(baseline_file))

load(baseline_file)
baseline_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   Before SCOTUS (2026-02-19): %s flows, $%.1f bn imports\n",
            format(nrow(baseline_data), big.mark = ","),
            sum(baseline_data$us_imports_bn)))

# --- Scenario 2: IEEPA Strike Down ---
ieepa_file <- "results/260222-260221-schedule24/processed_us_imports_with_rates_schedule24_baseline.RData"
stopifnot("IEEPA strike-down RData not found" = file.exists(ieepa_file))

load(ieepa_file)
ieepa_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   IEEPA Strike Down: %s flows, $%.1f bn imports\n",
            format(nrow(ieepa_data), big.mark = ","),
            sum(ieepa_data$us_imports_bn)))

# --- Scenario 3: Post-SCOTUS + S122 (POLICY_DATE=2026-02-24) ---
s122_file <- "results/260221-260224/processed_us_imports_with_rates_schedule24_baseline.RData"
stopifnot("Post-SCOTUS + S122 RData not found" = file.exists(s122_file))

load(s122_file)
s122_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   Post-SCOTUS + S122 (2026-02-24): %s flows, $%.1f bn imports\n",
            format(nrow(s122_data), big.mark = ","),
            sum(s122_data$us_imports_bn)))

# --- Scenario 4: S122 @ 15% (raised surcharge, POLICY_DATE=2026-02-24, schedule24) ---
s122_15_file <- "results/260222-260224-schedule24/processed_us_imports_with_rates_schedule24_baseline.RData"
stopifnot("S122 @ 15% RData not found" = file.exists(s122_15_file))

load(s122_15_file)
s122_15_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   S122 @ 15%% (schedule24): %s flows, $%.1f bn imports\n",
            format(nrow(s122_15_data), big.mark = ","),
            sum(s122_15_data$us_imports_bn)))

# =============================================================================
# 3. COMPUTE CONTRIBUTIONS (4 scenarios)
# =============================================================================

cat("\n3. Computing tariff contributions (4 scenarios)...\n")

cat("   Before SCOTUS:\n")
baseline_data <- compute_contributions(baseline_data, ieepa_baseline_rate = 10, verbose = TRUE)
validate_contributions(baseline_data, tolerance = 0.01, verbose = TRUE)

cat("   IEEPA Strike Down:\n")
ieepa_data <- compute_contributions(ieepa_data, ieepa_baseline_rate = 10, verbose = TRUE)
validate_contributions(ieepa_data, tolerance = 0.01, verbose = TRUE)

cat("   Post-SCOTUS + S122:\n")
s122_data <- compute_contributions(s122_data, ieepa_baseline_rate = 10, verbose = TRUE)
# S122 contribution: residual after all other contributions
# In this dataset, the rate engine stores the S122 surcharge alongside HTS/S232/S301.
# Capture residual as s122_contribution.
s122_data[, s122_contribution := rate - (hts_contribution + ieepa_contribution +
                                          s232_contribution + emergency_contribution +
                                          s301_contribution)]
s122_data[, s122_contribution := pmax(0, s122_contribution)]
cat(sprintf("   S122 residual: mean %.2f pp (captured as s122_contribution)\n",
            weighted.mean(s122_data$s122_contribution, s122_data$us_imports_bn)))

cat("   S122 @ 15%:\n")
s122_15_data <- compute_contributions(s122_15_data, ieepa_baseline_rate = 10, verbose = TRUE)
s122_15_data[, s122_contribution := rate - (hts_contribution + ieepa_contribution +
                                             s232_contribution + emergency_contribution +
                                             s301_contribution)]
s122_15_data[, s122_contribution := pmax(0, s122_contribution)]
cat(sprintf("   S122@15%% residual: mean %.2f pp (captured as s122_contribution)\n",
            weighted.mean(s122_15_data$s122_contribution, s122_15_data$us_imports_bn)))

# Add s122_contribution = 0 to non-S122 datasets for uniform structure
baseline_data[, s122_contribution := 0]
ieepa_data[, s122_contribution := 0]

# =============================================================================
# 4. AGGREGATE: Global
# =============================================================================

cat("\n4. Aggregating compositions...\n")

# Custom aggregation that includes s122_layer
aggregate_contributions_s122 <- function(data) {
  total_imports <- sum(data$us_imports_bn, na.rm = TRUE)
  list(
    hts_layer = sum(data$hts_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    s232_layer = sum(data$s232_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    ieepa_baseline_layer = sum(data$ieepa_baseline_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    ieepa_topup_layer = sum(data$ieepa_topup_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    emergency_layer = sum(data$emergency_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    s301_layer = sum(data$s301_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    s122_layer = sum(data$s122_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    total_rate = sum(data$rate * data$us_imports_bn, na.rm = TRUE) / total_imports,
    total_imports_bn = total_imports
  )
}

baseline_global  <- aggregate_contributions_s122(baseline_data)
ieepa_global     <- aggregate_contributions_s122(ieepa_data)
s122_global      <- aggregate_contributions_s122(s122_data)
s122_15_global   <- aggregate_contributions_s122(s122_15_data)

cat(sprintf("   Before SCOTUS:       %.2f%%\n", baseline_global$total_rate))
cat(sprintf("   IEEPA Strike Down:   %.2f%%\n", ieepa_global$total_rate))
cat(sprintf("   S122 @ 10%%:          %.2f%%\n", s122_global$total_rate))
cat(sprintf("   S122 @ 15%%:          %.2f%%\n", s122_15_global$total_rate))

# Sanity check ordering: pre-SCOTUS > S122@15% > S122@10% > IEEPA-struck
stopifnot("Ordering error: pre-SCOTUS should be > S122@15%" =
            baseline_global$total_rate > s122_15_global$total_rate)
stopifnot("Ordering error: S122@15% should be > S122@10%" =
            s122_15_global$total_rate > s122_global$total_rate)
stopifnot("Ordering error: S122@10% should be > IEEPA-struck" =
            s122_global$total_rate > ieepa_global$total_rate)
cat("   Sanity check PASSED: Before > S122@15% > S122@10% > IEEPA Strike Down\n")

# =============================================================================
# 5. AGGREGATE: Country-level
# =============================================================================

cat("   Aggregating country-level compositions...\n")

aggregate_country <- function(data) {
  global <- data[, .(
    geography = "Global", iso_code = "WLD",
    hts_layer            = weighted.mean(hts_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_baseline_layer = weighted.mean(ieepa_baseline_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_topup_layer    = weighted.mean(ieepa_topup_contribution, us_imports_bn, na.rm = TRUE),
    s232_layer           = weighted.mean(s232_contribution, us_imports_bn, na.rm = TRUE),
    emergency_layer      = weighted.mean(emergency_contribution, us_imports_bn, na.rm = TRUE),
    s301_layer           = weighted.mean(s301_contribution, us_imports_bn, na.rm = TRUE),
    s122_layer           = weighted.mean(s122_contribution, us_imports_bn, na.rm = TRUE),
    total_rate           = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn     = sum(us_imports_bn)
  )]

  country <- data[, .(
    hts_layer            = weighted.mean(hts_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_baseline_layer = weighted.mean(ieepa_baseline_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_topup_layer    = weighted.mean(ieepa_topup_contribution, us_imports_bn, na.rm = TRUE),
    s232_layer           = weighted.mean(s232_contribution, us_imports_bn, na.rm = TRUE),
    emergency_layer      = weighted.mean(emergency_contribution, us_imports_bn, na.rm = TRUE),
    s301_layer           = weighted.mean(s301_contribution, us_imports_bn, na.rm = TRUE),
    s122_layer           = weighted.mean(s122_contribution, us_imports_bn, na.rm = TRUE),
    total_rate           = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn     = sum(us_imports_bn)
  ), by = .(geography = exporter, iso_code)]

  country <- country[order(-total_imports_bn)]

  # EU aggregate
  eu <- aggregate_eu_composition(data)
  eu_s122 <- data[iso_code %in% EU_MEMBERS_ISO,
                   weighted.mean(s122_contribution, us_imports_bn, na.rm = TRUE)]
  eu[, s122_layer := eu_s122]

  rbind(global, country, eu, fill = TRUE)
}

baseline_country  <- aggregate_country(baseline_data)
ieepa_country     <- aggregate_country(ieepa_data)
s122_country      <- aggregate_country(s122_data)
s122_15_country   <- aggregate_country(s122_15_data)

cat(sprintf("   Baseline: %d  |  IEEPA: %d  |  S122@10%%: %d  |  S122@15%%: %d\n",
            nrow(baseline_country), nrow(ieepa_country), nrow(s122_country), nrow(s122_15_country)))

# =============================================================================
# 6. AGGREGATE: HS-2 sector-level
# =============================================================================

cat("   Aggregating HS-2 sector compositions...\n")

aggregate_hs2 <- function(data) {
  data[, hs2 := sprintf("%02d", as.integer(substr(sprintf("%08d", as.integer(hs_8digit)), 1, 2)))]

  hs2_agg <- data[, .(
    hts_layer            = weighted.mean(hts_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_baseline_layer = weighted.mean(ieepa_baseline_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_topup_layer    = weighted.mean(ieepa_topup_contribution, us_imports_bn, na.rm = TRUE),
    s232_layer           = weighted.mean(s232_contribution, us_imports_bn, na.rm = TRUE),
    emergency_layer      = weighted.mean(emergency_contribution, us_imports_bn, na.rm = TRUE),
    s301_layer           = weighted.mean(s301_contribution, us_imports_bn, na.rm = TRUE),
    total_rate           = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn     = sum(us_imports_bn)
  ), by = hs2]

  hs2_agg[, chapter_label := get_chapter_label(hs2)]
  hs2_agg <- hs2_agg[order(-total_imports_bn)]

  return(hs2_agg)
}

baseline_hs2  <- aggregate_hs2(baseline_data)
ieepa_hs2     <- aggregate_hs2(ieepa_data)
s122_hs2      <- aggregate_hs2(s122_data)
s122_15_hs2   <- aggregate_hs2(s122_15_data)

cat(sprintf("   Baseline: %d  |  IEEPA: %d  |  S122@10%%: %d  |  S122@15%%: %d\n",
            nrow(baseline_hs2), nrow(ieepa_hs2), nrow(s122_hs2), nrow(s122_15_hs2)))

# =============================================================================
# 7. CHARTS — now generated by enhance_s122_actual_charts.R
# =============================================================================
# Chart generation has been moved to enhance_s122_actual_charts.R which
# produces all 7 charts (desktop + mobile) with 4 scenarios and larger fonts.
# Run: Rscript code/scotus/enhance_s122_actual_charts.R

cat("\n5. Skipping chart generation (handled by enhance_s122_actual_charts.R)...\n")

# =============================================================================
# 8. GENERATE EXCEL 1: Country Comparison (4 scenarios)
# =============================================================================

cat("\n6. Generating Excel files...\n")
cat("   Excel 1: Country-level comparison (4 scenarios)...\n")

# Helper: extract slim country table for one scenario
slim_country <- function(country_dt, suffix) {
  out <- country_dt[, .(
    iso_code,
    hts = round(hts_layer, 2),
    ieepa_baseline = round(ieepa_baseline_layer, 2),
    ieepa_topup = round(ieepa_topup_layer, 2),
    s232 = round(s232_layer, 2),
    emergency = round(emergency_layer, 2),
    s301 = round(s301_layer, 2),
    s122 = round(s122_layer, 2),
    total = round(total_rate, 2)
  )]
  setnames(out, c("hts", "ieepa_baseline", "ieepa_topup", "s232", "emergency", "s301", "s122", "total"),
           paste0(c("hts", "ieepa_baseline", "ieepa_topup", "s232", "emergency", "s301", "s122", "total"), "_", suffix))
  return(out)
}

base_slim <- baseline_country[, .(geography, iso_code, total_imports_bn)]
s1 <- slim_country(baseline_country, "before")
s2 <- slim_country(ieepa_country, "ieepa")
s3 <- slim_country(s122_country, "s122")
s4 <- slim_country(s122_15_country, "s122_15")

country_comp <- Reduce(function(x, y) merge(x, y, by = "iso_code", all.x = TRUE),
                       list(base_slim, s1, s2, s3, s4))
country_comp[, `:=`(
  change_strike_vs_before = round(total_ieepa - total_before, 2),
  change_s122_vs_ieepa = round(total_s122 - total_ieepa, 2),
  change_s122_vs_before = round(total_s122 - total_before, 2),
  change_s122_15_vs_before = round(total_s122_15 - total_before, 2),
  change_s122_15_vs_s122 = round(total_s122_15 - total_s122, 2)
)]
country_comp <- country_comp[order(-total_imports_bn)]

# Pretty column names for Excel
country_excel <- country_comp[, .(
  Country = geography, `ISO Code` = iso_code,
  `Import Value ($bn)` = round(total_imports_bn, 2),
  `HTS (Before)` = hts_before,
  `IEEPA Baseline (Before)` = ieepa_baseline_before,
  `IEEPA Top-up (Before)` = ieepa_topup_before,
  `S232 (Before)` = s232_before,
  `Emergency (Before)` = emergency_before,
  `S301 (Before)` = s301_before,
  `S122 (Before)` = s122_before,
  `Total (Before)` = total_before,
  `HTS (Strike Down)` = hts_ieepa,
  `IEEPA Baseline (Strike Down)` = ieepa_baseline_ieepa,
  `IEEPA Top-up (Strike Down)` = ieepa_topup_ieepa,
  `S232 (Strike Down)` = s232_ieepa,
  `Emergency (Strike Down)` = emergency_ieepa,
  `S301 (Strike Down)` = s301_ieepa,
  `S122 (Strike Down)` = s122_ieepa,
  `Total (Strike Down)` = total_ieepa,
  `HTS (S122 @ 10%)` = hts_s122,
  `IEEPA Baseline (S122 @ 10%)` = ieepa_baseline_s122,
  `IEEPA Top-up (S122 @ 10%)` = ieepa_topup_s122,
  `S232 (S122 @ 10%)` = s232_s122,
  `Emergency (S122 @ 10%)` = emergency_s122,
  `S301 (S122 @ 10%)` = s301_s122,
  `S122 (S122 @ 10%)` = s122_s122,
  `Total (S122 @ 10%)` = total_s122,
  `HTS (S122 @ 15%)` = hts_s122_15,
  `IEEPA Baseline (S122 @ 15%)` = ieepa_baseline_s122_15,
  `IEEPA Top-up (S122 @ 15%)` = ieepa_topup_s122_15,
  `S232 (S122 @ 15%)` = s232_s122_15,
  `Emergency (S122 @ 15%)` = emergency_s122_15,
  `S301 (S122 @ 15%)` = s301_s122_15,
  `S122 (S122 @ 15%)` = s122_s122_15,
  `Total (S122 @ 15%)` = total_s122_15,
  `Change: Strike Down vs Before (pp)` = change_strike_vs_before,
  `Change: S122@10% vs Before (pp)` = change_s122_vs_before,
  `Change: S122@15% vs Before (pp)` = change_s122_15_vs_before,
  `Change: S122@15% vs S122@10% (pp)` = change_s122_15_vs_s122
)]

# Sheet 2: Sector summary (HS-2)
slim_hs2 <- function(hs2_dt, suffix) {
  out <- hs2_dt[, .(hs2, total = round(total_rate, 2))]
  setnames(out, "total", paste0("total_", suffix))
  return(out)
}

hs2_base <- baseline_hs2[, .(hs2, chapter_label, total_imports_bn)]
h1 <- slim_hs2(baseline_hs2, "before")
h2 <- slim_hs2(ieepa_hs2, "ieepa")
h3 <- slim_hs2(s122_hs2, "s122")
h4 <- slim_hs2(s122_15_hs2, "s122_15")

sector_comp <- Reduce(function(x, y) merge(x, y, by = "hs2", all.x = TRUE),
                      list(hs2_base, h1, h2, h3, h4))
sector_comp[, `:=`(
  change_strike_vs_before = round(total_ieepa - total_before, 2),
  change_s122_vs_ieepa = round(total_s122 - total_ieepa, 2),
  change_s122_vs_before = round(total_s122 - total_before, 2),
  change_s122_15_vs_before = round(total_s122_15 - total_before, 2),
  change_s122_15_vs_s122 = round(total_s122_15 - total_s122, 2)
)]
sector_comp <- sector_comp[order(-total_imports_bn)]

sector_excel <- sector_comp[, .(
  `HS-2 Chapter` = hs2, `Chapter Name` = chapter_label,
  `Import Value ($bn)` = round(total_imports_bn, 2),
  `Total (Before)` = total_before,
  `Total (Strike Down)` = total_ieepa,
  `Total (S122 @ 10%)` = total_s122,
  `Total (S122 @ 15%)` = total_s122_15,
  `Change: Strike Down vs Before (pp)` = change_strike_vs_before,
  `Change: S122@10% vs Before (pp)` = change_s122_vs_before,
  `Change: S122@15% vs Before (pp)` = change_s122_15_vs_before,
  `Change: S122@15% vs S122@10% (pp)` = change_s122_15_vs_s122
)]

# Sheet 3: Methodology
methodology_text <- data.table(
  Section = c("Data source", "Analysis date",
              "Scenario 1: Before SCOTUS",
              "Scenario 2: IEEPA Strike Down",
              "Scenario 3: S122 @ 10%",
              "Scenario 4: S122 @ 15%",
              "MFN rates", "Import values", "Methodology", "Contact"),
  Description = c(
    "Global Trade Alert US Tariff Barrier Estimates",
    "22 February 2026",
    paste0("The tariff regime as of 19 February 2026 (the day before the Supreme Court ruling), ",
           "including all IEEPA, S232, S301, and emergency tariffs."),
    paste0("All tariffs imposed under IEEPA (baseline 10% + country-specific top-ups) and ",
           "emergency executive orders set to zero following the Supreme Court ruling on 20 February 2026. ",
           "S232, S301, and HTS rates unchanged."),
    paste0("The initial Section 122 tariff regime effective 24 February 2026. ",
           "A flat 10% surcharge imposed on most imports, with exceptions for Section 232 products, ",
           "USMCA-qualifying goods, CAFTA-DR textiles, and approximately 1,100 products listed in Annex II. ",
           "The surcharge is temporary (150 days, expiring 24 July 2026)."),
    paste0("The current Section 122 tariff regime following the 22 February increase from 10% to 15%. ",
           "Same exception structure as S122 @ 10%. The higher surcharge is fully simulated at the ",
           "HS 8-digit product level."),
    "HTS Schedule rates (2024 published tariff schedule)",
    "USITC DataWeb, 2024 annual customs value by HS 8-digit product and exporter",
    paste0("Trade-weighted average tariff rates computed at flow level ",
           "(exporter x HS-8) and aggregated. Tariff contributions decomposed ",
           "using formula-based approach ensuring layers sum to total rate. ",
           "EU aggregate includes 27 member states."),
    "Global Trade Alert, University of St. Gallen. https://www.globaltradealert.org"
  )
)

# Write workbook
wb1 <- createWorkbook()
addWorksheet(wb1, "Country Summary")
addWorksheet(wb1, "Sector Summary (HS-2)")
addWorksheet(wb1, "Methodology")

writeDataTable(wb1, "Country Summary", country_excel,
               tableStyle = "TableStyleMedium2")
freezePane(wb1, "Country Summary", firstRow = TRUE)
setColWidths(wb1, "Country Summary", cols = 1:ncol(country_excel), widths = "auto")

writeDataTable(wb1, "Sector Summary (HS-2)", sector_excel,
               tableStyle = "TableStyleMedium2")
freezePane(wb1, "Sector Summary (HS-2)", firstRow = TRUE)
setColWidths(wb1, "Sector Summary (HS-2)", cols = 1:ncol(sector_excel), widths = "auto")

writeDataTable(wb1, "Methodology", methodology_text,
               tableStyle = "TableStyleLight1")
setColWidths(wb1, "Methodology", cols = 1:2, widths = c(25, 80))

saveWorkbook(wb1, "results/scotus/s122_actual/data/s122_actual_country_comparison.xlsx", overwrite = TRUE)
cat("   Saved: s122_actual_country_comparison.xlsx\n")

# =============================================================================
# 9. GENERATE EXCEL 2: Flow-Level Public Dataset (4 scenarios)
# =============================================================================

cat("   Excel 2: Flow-level public dataset (4 scenarios)...\n")

# Load product names
product_names <- fread("data/us_hts-8_digit_sectioned.csv", sep = ";",
                       select = c("hs_8digit", "hs_8name"))
setnames(product_names, c("hs_8digit", "product_name"))

# Prepare flow-level data (merge all 4 by exporter x hs_8digit)
flow_base <- baseline_data[, .(
  exporter, iso_code, hs_8digit, us_imports_bn,
  rate_before = rate,
  hts_rate, s232_rate, s301_rate
)]

flow_ieepa   <- ieepa_data[, .(iso_code, hs_8digit, rate_ieepa = rate)]
flow_s122    <- s122_data[, .(iso_code, hs_8digit, rate_s122 = rate)]
flow_s122_15 <- s122_15_data[, .(iso_code, hs_8digit, rate_s122_15 = rate)]

flow_merged <- merge(flow_base, flow_ieepa, by = c("iso_code", "hs_8digit"), all.x = TRUE)
flow_merged <- merge(flow_merged, flow_s122, by = c("iso_code", "hs_8digit"), all.x = TRUE)
flow_merged <- merge(flow_merged, flow_s122_15, by = c("iso_code", "hs_8digit"), all.x = TRUE)

# Merge product names
flow_merged[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit := NULL]
flow_merged <- merge(flow_merged, product_names, by = "hs_8digit_int", all.x = TRUE)

flow_merged <- flow_merged[order(exporter, hs_8digit)]

# Sheet 1: Flow-level data with clean date-based column names
flow_excel <- flow_merged[, .(
  `Import Origin` = exporter,
  `ISO Code` = iso_code,
  `HS 8-digit` = hs_8digit,
  `Product Name` = product_name,
  `US Imports ($bn)` = round(us_imports_bn, 6),
  `Before SCOTUS (%)` = round(rate_before, 2),
  `IEEPA Strike Down (%)` = round(rate_ieepa, 2),
  `S122 @ 10% (%)` = round(rate_s122, 2),
  `S122 @ 15% (%)` = round(rate_s122_15, 2),
  `HTS/MFN Rate (%)` = round(hts_rate, 2),
  `S232 Rate (%)` = round(s232_rate, 2),
  `S301 Rate (%)` = round(s301_rate, 2)
)]

# Sheet 2: Aggregate by import origin (trade-weighted average rates)
origin_agg <- flow_merged[, .(
  `US Imports ($bn)` = round(sum(us_imports_bn), 2),
  `Before SCOTUS (%)` = round(weighted.mean(rate_before, us_imports_bn, na.rm = TRUE), 2),
  `IEEPA Strike Down (%)` = round(weighted.mean(rate_ieepa, us_imports_bn, na.rm = TRUE), 2),
  `S122 @ 10% (%)` = round(weighted.mean(rate_s122, us_imports_bn, na.rm = TRUE), 2),
  `S122 @ 15% (%)` = round(weighted.mean(rate_s122_15, us_imports_bn, na.rm = TRUE), 2)
), by = .(
  `Import Origin` = exporter,
  `ISO Code` = iso_code
)]
origin_agg <- origin_agg[order(-`US Imports ($bn)`)]

# Add EU aggregate row
eu_flows <- flow_merged[iso_code %in% EU_MEMBERS_ISO]
if (nrow(eu_flows) > 0) {
  eu_row <- data.table(
    `Import Origin` = "European Union",
    `ISO Code` = "EU",
    `US Imports ($bn)` = round(sum(eu_flows$us_imports_bn), 2),
    `Before SCOTUS (%)` = round(weighted.mean(eu_flows$rate_before, eu_flows$us_imports_bn, na.rm = TRUE), 2),
    `IEEPA Strike Down (%)` = round(weighted.mean(eu_flows$rate_ieepa, eu_flows$us_imports_bn, na.rm = TRUE), 2),
    `S122 @ 10% (%)` = round(weighted.mean(eu_flows$rate_s122, eu_flows$us_imports_bn, na.rm = TRUE), 2),
    `S122 @ 15% (%)` = round(weighted.mean(eu_flows$rate_s122_15, eu_flows$us_imports_bn, na.rm = TRUE), 2)
  )
  origin_agg <- rbind(origin_agg, eu_row)
  origin_agg <- origin_agg[order(-`US Imports ($bn)`)]
}

# Add Global row at top
global_row <- data.table(
  `Import Origin` = "Global",
  `ISO Code` = "WLD",
  `US Imports ($bn)` = round(sum(flow_merged$us_imports_bn), 2),
  `Before SCOTUS (%)` = round(weighted.mean(flow_merged$rate_before, flow_merged$us_imports_bn, na.rm = TRUE), 2),
  `IEEPA Strike Down (%)` = round(weighted.mean(flow_merged$rate_ieepa, flow_merged$us_imports_bn, na.rm = TRUE), 2),
  `S122 @ 10% (%)` = round(weighted.mean(flow_merged$rate_s122, flow_merged$us_imports_bn, na.rm = TRUE), 2),
  `S122 @ 15% (%)` = round(weighted.mean(flow_merged$rate_s122_15, flow_merged$us_imports_bn, na.rm = TRUE), 2)
)
origin_agg <- rbind(global_row, origin_agg)

wb2 <- createWorkbook()
addWorksheet(wb2, "Flow-Level Tariff Rates")
addWorksheet(wb2, "Aggregate by Import Origin")

writeDataTable(wb2, "Flow-Level Tariff Rates", flow_excel,
               tableStyle = "TableStyleMedium2")
freezePane(wb2, "Flow-Level Tariff Rates", firstRow = TRUE)
setColWidths(wb2, "Flow-Level Tariff Rates",
             cols = 1:ncol(flow_excel), widths = "auto")

writeDataTable(wb2, "Aggregate by Import Origin", origin_agg,
               tableStyle = "TableStyleMedium2")
freezePane(wb2, "Aggregate by Import Origin", firstRow = TRUE)
setColWidths(wb2, "Aggregate by Import Origin",
             cols = 1:ncol(origin_agg), widths = "auto")

saveWorkbook(wb2, "results/scotus/s122_actual/data/s122_actual_public_dataset.xlsx", overwrite = TRUE)
cat("   Saved: s122_actual_public_dataset.xlsx (2 sheets: flow-level + aggregate by origin)\n")

# =============================================================================
# 10. GENERATE BLOG HTML DRAFT
# =============================================================================

cat("\n7. Generating blog HTML draft...\n")

# S3 base URL for images
S3_BASE <- "https://ricardo-dashboard.s3.eu-west-1.amazonaws.com/reports/scotus/s122_actual"

# Blog Post 2 link
BLOG_POST_2_URL <- "https://www.globaltradealert.org/reports/S122-Tariff-Scenario-Analysis"

blog_html <- sprintf('
<div class="title"><h3>Section 122 in effect: the US tariff average is now %.1f%%</h3></div><div class="paragraph"><p>The Supreme Court struck down IEEPA tariffs on 20 February 2026. Within hours, the White House invoked Section 122 of the Trade Act of 1974. The proclamation imposes a flat 10%% surcharge on most US imports, effective 24 February 2026, for 150 days. We now know the actual tariff landscape.</p>

<p>The trade-weighted average US tariff rate under the Section 122 regime is <strong>%.1f%%</strong>, compared with %.1f%% before the ruling and %.1f%% if no replacement had been enacted.</p>

<p><img src="%s/s122_actual_aggregate_comparison.png" alt="Section 122 actual tariff regime: aggregate US tariff composition" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>What the proclamation does</h3></div><div class="paragraph"><p>The Section 122 proclamation imposes a flat 10 percentage point surcharge on all imports, with the following exceptions:</p>

<ul>
<li><strong>Section 232 primacy</strong>: Products already subject to Section 232 tariffs (steel, aluminium, copper, lumber, automobiles) are excluded from the surcharge to the extent the 232 tariff applies.</li>
<li><strong>USMCA preferences</strong>: Articles entering duty-free under USMCA remain exempt.</li>
<li><strong>CAFTA-DR textiles</strong>: Textile and apparel articles entering duty-free under the Dominican Republic-Central America Free Trade Agreement are exempt.</li>
<li><strong>Annex II exceptions</strong>: Approximately 1,100 product codes are exempted from the surcharge.</li>
</ul>

<p>The surcharge is explicitly temporary: 150 days from 24 February 2026, expiring 24 July 2026 unless Congress extends it. For a detailed analysis of the proclamation, including the exception list changes and the fate of bilateral trade deals, see our <a href="https://www.globaltradealert.org/reports/S122-Explainer" target="_blank" rel="noopener">Section 122 explainer</a>.</p></div>

<div class="title"><h3>By country: top 20 import sources</h3></div><div class="paragraph"><p>The following chart compares all three scenarios for the top 20 US import sources. The grey bars show the pre-ruling tariff rate, the navy bars show what remains after IEEPA is struck down, and the teal bars show the actual Section 122 regime.</p>

<p><img src="%s/s122_actual_country_comparison.png" alt="Section 122 actual: tariff rates by country" style="width:100%%;max-width:900px;" /></p>

<p>Countries that faced high IEEPA rates (India, Thailand, Vietnam) see substantially lower tariffs under the Section 122 replacement than they did before the ruling. Countries with low pre-existing tariffs see a more modest change, with the 10%% surcharge providing the floor.</p></div>

<div class="title"><h3>By sector: HS-2 chapters</h3></div><div class="paragraph"><p>The sector-level view reveals where the Section 122 regime differs from the pre-ruling tariff stack. Industries dominated by Section 232 products (iron and steel, aluminium) show less movement because the surcharge does not apply where 232 tariffs already bite. Sectors where IEEPA was the primary tariff instrument show the largest gaps.</p>

<p><img src="%s/s122_actual_sector_comparison.png" alt="Section 122 actual: tariff rates by sector" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>S122 contribution decomposition</h3></div><div class="paragraph"><p>The following chart decomposes the Section 122 tariff regime into two layers for each of the top 20 import sources: the post-ruling base rate (tariffs that survived the SCOTUS decision) and the S122 surcharge increment (the additional 10 percentage points where applicable).</p>

<p><img src="%s/s122_actual_contribution.png" alt="S122 contribution decomposition by country" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>Comparison to hypothetical analysis</h3></div><div class="paragraph"><p>Yesterday, we modelled two hypothetical Section 122 interpretations before the proclamation was issued: a flat surcharge (18.3%%) and an IEEPA-like stacking (13.7%%). The actual proclamation lands at %.1f%%, below both hypothetical scenarios. This is because the actual proclamation uses a simpler structure than either hypothetical, with the flat 10%% surcharge and broad exceptions for Section 232, USMCA, and Annex II products.</p>

<p>For the hypothetical analysis, see <a href="%s" target="_blank" rel="noopener">Section 122 invoked: two interpretations of the new US tariff regime</a>.</p></div>

<div class="title"><h3>Downloads</h3></div><div class="paragraph"><p>The following datasets are available for download:</p>

<ul>
<li><a href="%s/s122_actual_country_comparison.xlsx" target="_blank" rel="noopener"><strong>Country-level comparison</strong></a>: Tariff rates for all US import sources under all three scenarios, including EU aggregate and HS-2 sector breakdown</li>
<li><a href="%s/s122_actual_public_dataset.xlsx" target="_blank" rel="noopener"><strong>Flow-level public dataset</strong></a>: Complete tariff rates at the exporter x HS 8-digit product level for all scenarios (274,000+ flows)</li>
</ul></div>

<div class="title"><h3>Methodology</h3></div><div class="paragraph"><p>Tariff rates are computed at the HS 8-digit product level for each exporting country and aggregated using 2024 US import values as weights. The analysis covers all US merchandise imports.</p>

<p>Three reference points are modelled:</p>
<ol>
<li><strong>Before SCOTUS</strong>: The tariff regime as of 19 February 2026, including all IEEPA, S232, S301, and emergency tariffs.</li>
<li><strong>IEEPA Strike Down</strong>: All IEEPA-authorised tariffs and emergency orders removed following the Supreme Court ruling on 20 February 2026.</li>
<li><strong>Post-SCOTUS + S122</strong>: The actual tariff regime effective 24 February 2026 under the Section 122 proclamation, including the 10%% surcharge with exceptions for S232, USMCA, CAFTA-DR, and Annex II products.</li>
</ol>

<p>For the full methodology, see the <a href="https://www.globaltradealert.org/reports/US-Tariff-Barrier-Estimates" target="_blank" rel="noopener">US Tariff Barrier Estimates</a> documentation.</p></div>',
  s122_global$total_rate,
  s122_global$total_rate, baseline_global$total_rate, ieepa_global$total_rate,
  S3_BASE,
  S3_BASE, S3_BASE, S3_BASE,
  s122_global$total_rate,
  BLOG_POST_2_URL,
  S3_BASE, S3_BASE
)

writeLines(blog_html, "results/scotus/s122_actual/blog/s122_actual_blog_body.html")
cat("   Saved: results/scotus/s122_actual/blog/s122_actual_blog_body.html\n")

# =============================================================================
# 11. SUMMARY
# =============================================================================

cat("\n=== OUTPUT GENERATION COMPLETE ===\n\n")
cat("Charts:\n")
cat("  results/scotus/s122_actual/charts/s122_actual_aggregate_comparison.png\n")
cat("  results/scotus/s122_actual/charts/s122_actual_aggregate_comparison_mobile.png\n")
cat("  results/scotus/s122_actual/charts/s122_actual_country_comparison.png\n")
cat("  results/scotus/s122_actual/charts/s122_actual_sector_comparison.png\n")
cat("  results/scotus/s122_actual/charts/s122_actual_sector_comparison_mobile.png\n")
cat("  results/scotus/s122_actual/charts/s122_actual_contribution.png\n")
cat("\nExcel:\n")
cat("  results/scotus/s122_actual/data/s122_actual_country_comparison.xlsx\n")
cat("  results/scotus/s122_actual/data/s122_actual_public_dataset.xlsx\n")
cat("\nBlog:\n")
cat("  results/scotus/s122_actual/blog/s122_actual_blog_body.html\n")
cat(sprintf("\nKey numbers:\n"))
cat(sprintf("  Before SCOTUS:       %.2f%%\n", baseline_global$total_rate))
cat(sprintf("  IEEPA Strike Down:   %.2f%%\n", ieepa_global$total_rate))
cat(sprintf("  Post-SCOTUS + S122:  %.2f%%\n", s122_global$total_rate))
cat(sprintf("  S122 surcharge contribution: %.2f pp\n",
            s122_global$s122_layer))
cat("\nNext steps:\n")
cat("  1. Review charts and blog HTML\n")
cat("  2. Upload to S3: aws s3 cp results/scotus/s122_actual/ s3://ricardo-dashboard/reports/scotus/s122_actual/ --recursive\n")
cat("  3. Build blog post: python3 code/scotus/build_s122_actual_blog_post.py --dry-run\n")
