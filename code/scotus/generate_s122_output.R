# =============================================================================
# SECTION 122 SCENARIO ANALYSIS: OUTPUT GENERATOR
# =============================================================================
# Master orchestrator for the S122 blog output package (Blog Post 2).
# Generates 4 charts + 2 Excel files comparing 4 scenarios:
#   1. Before SCOTUS (baseline)
#   2. IEEPA Strike Down
#   3. S122 Flat Surcharge
#   4. S122 IEEPA Stack
#
# Input:
#   - results/260220-260220/...baseline.RData
#   - results/scenarios/ieepa_full_strike_down/...RData
#   - results/scenarios/s122_flat_surcharge/...RData
#   - results/scenarios/s122_ieepa_stack/...RData
#
# Output:
#   - results/scotus/s122/charts/   (4 charts + 2 mobile variants)
#   - results/scotus/s122/data/     (2 Excel files)
#   - results/scotus/s122/blog/     (blog HTML for review)
#
# Usage:
#   cd us-tariff-barrier-estimates
#   Rscript code/scotus/generate_s122_output.R
# =============================================================================

cat("=== Section 122 Scenario Analysis: Output Generator ===\n")
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
dirs <- c("results/scotus/s122/charts", "results/scotus/s122/data", "results/scotus/s122/blog")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("   Output directories created.\n")

# =============================================================================
# 2. LOAD DATA (4 scenarios)
# =============================================================================

cat("\n2. Loading tariff data (4 scenarios)...\n")

# --- Scenario 1: Before SCOTUS (baseline) ---
baseline_file <- "results/260220-260220/processed_us_imports_with_rates_schedule24_baseline.RData"
if (!file.exists(baseline_file)) {
  baseline_dirs <- list.dirs("results", recursive = FALSE)
  baseline_dirs <- baseline_dirs[grepl("260220", basename(baseline_dirs))]
  if (length(baseline_dirs) > 0) {
    candidate <- file.path(baseline_dirs[1], "processed_us_imports_with_rates_schedule24_baseline.RData")
    if (file.exists(candidate)) baseline_file <- candidate
  }
}
stopifnot("Baseline RData not found" = file.exists(baseline_file))

load(baseline_file)
baseline_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   Baseline: %s flows, $%.1f bn imports\n",
            format(nrow(baseline_data), big.mark = ","),
            sum(baseline_data$us_imports_bn)))

# --- Scenario 2: IEEPA Strike Down ---
ieepa_file <- "results/scenarios/ieepa_full_strike_down/processed_us_imports_with_rates_schedule24_ieepa_full_strike_down.RData"
stopifnot("IEEPA strike-down RData not found" = file.exists(ieepa_file))

load(ieepa_file)
ieepa_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   IEEPA Strike Down: %s flows, $%.1f bn imports\n",
            format(nrow(ieepa_data), big.mark = ","),
            sum(ieepa_data$us_imports_bn)))

# --- Scenario 3: S122 Flat Surcharge ---
surcharge_file <- "results/scenarios/s122_flat_surcharge/processed_us_imports_with_rates_schedule24_s122_flat_surcharge.RData"
stopifnot("S122 flat surcharge RData not found" = file.exists(surcharge_file))

load(surcharge_file)
surcharge_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   S122 Flat Surcharge: %s flows, $%.1f bn imports\n",
            format(nrow(surcharge_data), big.mark = ","),
            sum(surcharge_data$us_imports_bn)))

# --- Scenario 4: S122 IEEPA Stack ---
stack_file <- "results/scenarios/s122_ieepa_stack/processed_us_imports_with_rates_schedule24_s122_ieepa_stack.RData"
stopifnot("S122 IEEPA stack RData not found" = file.exists(stack_file))

load(stack_file)
stack_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   S122 IEEPA Stack: %s flows, $%.1f bn imports\n",
            format(nrow(stack_data), big.mark = ","),
            sum(stack_data$us_imports_bn)))

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

cat("   S122 Flat Surcharge:\n")
surcharge_data <- compute_contributions(surcharge_data, ieepa_baseline_rate = 10, verbose = TRUE)
# S122 surcharge adds a flat rate on top — capture residual as s122_contribution
surcharge_data[, s122_contribution := rate - (hts_contribution + ieepa_contribution +
                                               s232_contribution + emergency_contribution +
                                               s301_contribution)]
surcharge_data[, s122_contribution := pmax(0, s122_contribution)]
cat(sprintf("   S122 surcharge residual: mean %.2f pp (captured as s122_contribution)\n",
            weighted.mean(surcharge_data$s122_contribution, surcharge_data$us_imports_bn)))

cat("   S122 IEEPA Stack:\n")
# The rate engine stores S122 baseline in the ieepa_rate column.
# compute_contributions() will attribute it to IEEPA — we relabel afterwards.
stack_data <- compute_contributions(stack_data, ieepa_baseline_rate = 10, verbose = TRUE)
validate_contributions(stack_data, tolerance = 0.01, verbose = TRUE)
# Relabel: what compute_contributions calls "IEEPA" is actually S122 in this scenario
stack_data[, s122_contribution := ieepa_contribution]
stack_data[, ieepa_baseline_contribution := 0]
stack_data[, ieepa_topup_contribution := 0]
stack_data[, ieepa_contribution := 0]
cat(sprintf("   S122 stack contribution: mean %.2f pp (relabeled from IEEPA)\n",
            weighted.mean(stack_data$s122_contribution, stack_data$us_imports_bn)))

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
surcharge_global <- aggregate_contributions_s122(surcharge_data)
stack_global     <- aggregate_contributions_s122(stack_data)

cat(sprintf("   Before SCOTUS:    %.2f%%\n", baseline_global$total_rate))
cat(sprintf("   IEEPA Strike Down: %.2f%%\n", ieepa_global$total_rate))
cat(sprintf("   S122 Surcharge:   %.2f%%\n", surcharge_global$total_rate))
cat(sprintf("   S122 Stack:       %.2f%%\n", stack_global$total_rate))

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

  # EU aggregate (s122_contribution handled via fill = TRUE, will be NA -> filled below)
  eu <- aggregate_eu_composition(data)
  # Add s122 EU aggregate manually
  eu_s122 <- data[iso_code %in% EU_MEMBERS_ISO,
                   weighted.mean(s122_contribution, us_imports_bn, na.rm = TRUE)]
  eu[, s122_layer := eu_s122]

  rbind(global, country, eu, fill = TRUE)
}

baseline_country  <- aggregate_country(baseline_data)
ieepa_country     <- aggregate_country(ieepa_data)
surcharge_country <- aggregate_country(surcharge_data)
stack_country     <- aggregate_country(stack_data)

cat(sprintf("   Baseline: %d geographies  |  IEEPA: %d  |  Surcharge: %d  |  Stack: %d\n",
            nrow(baseline_country), nrow(ieepa_country),
            nrow(surcharge_country), nrow(stack_country)))

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
surcharge_hs2 <- aggregate_hs2(surcharge_data)
stack_hs2     <- aggregate_hs2(stack_data)

cat(sprintf("   Baseline: %d HS-2 chapters  |  IEEPA: %d  |  Surcharge: %d  |  Stack: %d\n",
            nrow(baseline_hs2), nrow(ieepa_hs2),
            nrow(surcharge_hs2), nrow(stack_hs2)))

# =============================================================================
# 7. GENERATE CHARTS
# =============================================================================

cat("\n5. Generating charts...\n")

# --- Chart 1: Hero 4-scenario composition ---
cat("   Chart 1: 4-scenario aggregate composition...\n")

p1 <- create_four_scenario_chart(baseline_global, ieepa_global, surcharge_global, stack_global)
ggsave("results/scotus/s122/charts/s122_aggregate_comparison.png",
       plot = p1, width = 10, height = 6, dpi = 300, bg = GTA_BG)

p1m <- create_four_scenario_chart_mobile(baseline_global, ieepa_global, surcharge_global, stack_global)
ggsave("results/scotus/s122/charts/s122_aggregate_comparison_mobile.png",
       plot = p1m, width = 5, height = 8, dpi = 300, bg = GTA_BG)

cat("   Saved: s122_aggregate_comparison.png + mobile\n")

# --- Chart 2: 4-scenario country comparison ---
cat("   Chart 2: 4-scenario country comparison...\n")

p2 <- create_four_scenario_country_chart(baseline_country, ieepa_country,
                                          surcharge_country, stack_country)
ggsave("results/scotus/s122/charts/s122_country_comparison.png",
       plot = p2, width = 14, height = 12, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_country_comparison.png\n")

# --- Chart 3: 4-scenario sector comparison ---
cat("   Chart 3: 4-scenario sector comparison...\n")

p3 <- create_four_scenario_sector_chart(baseline_hs2, ieepa_hs2, surcharge_hs2, stack_hs2)
ggsave("results/scotus/s122/charts/s122_sector_comparison.png",
       plot = p3, width = 14, height = 12, dpi = 300, bg = GTA_BG)

p3m <- create_four_scenario_sector_chart_mobile(baseline_hs2, ieepa_hs2, surcharge_hs2, stack_hs2)
ggsave("results/scotus/s122/charts/s122_sector_comparison_mobile.png",
       plot = p3m, width = 6, height = 12, dpi = 300, bg = GTA_BG)

cat("   Saved: s122_sector_comparison.png + mobile\n")

# --- Chart 4: Surcharge vs stack gap ---
cat("   Chart 4: Surcharge vs stack gap...\n")

p4 <- create_surcharge_vs_stack_chart(ieepa_country, stack_country, surcharge_country)
ggsave("results/scotus/s122/charts/s122_surcharge_vs_stack.png",
       plot = p4, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_surcharge_vs_stack.png\n")

# =============================================================================
# 8. GENERATE EXCEL 1: Country Comparison (4 scenarios)
# =============================================================================

cat("\n6. Generating Excel files...\n")
cat("   Excel 1: Country-level comparison (4 scenarios)...\n")

# Helper: extract slim country table for one scenario
slim_country <- function(country_dt, suffix) {
  cols <- c("iso_code",
            paste0("hts_", suffix), paste0("ieepa_baseline_", suffix),
            paste0("ieepa_topup_", suffix), paste0("s232_", suffix),
            paste0("emergency_", suffix), paste0("s301_", suffix),
            paste0("total_", suffix))
  out <- country_dt[, .(
    iso_code,
    hts = round(hts_layer, 2),
    ieepa_baseline = round(ieepa_baseline_layer, 2),
    ieepa_topup = round(ieepa_topup_layer, 2),
    s232 = round(s232_layer, 2),
    emergency = round(emergency_layer, 2),
    s301 = round(s301_layer, 2),
    total = round(total_rate, 2)
  )]
  setnames(out, c("hts", "ieepa_baseline", "ieepa_topup", "s232", "emergency", "s301", "total"),
           paste0(c("hts", "ieepa_baseline", "ieepa_topup", "s232", "emergency", "s301", "total"), "_", suffix))
  return(out)
}

base_slim  <- baseline_country[, .(geography, iso_code, total_imports_bn)]
s1 <- slim_country(baseline_country, "before")
s2 <- slim_country(ieepa_country, "ieepa")
s3 <- slim_country(surcharge_country, "surcharge")
s4 <- slim_country(stack_country, "stack")

country_comp <- Reduce(function(x, y) merge(x, y, by = "iso_code", all.x = TRUE),
                       list(base_slim, s1, s2, s3, s4))
country_comp[, `:=`(
  change_strike_vs_before = round(total_ieepa - total_before, 2),
  change_surcharge_vs_strike = round(total_surcharge - total_ieepa, 2),
  change_stack_vs_strike = round(total_stack - total_ieepa, 2)
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
  `Total (Before)` = total_before,
  `HTS (Strike Down)` = hts_ieepa,
  `IEEPA Baseline (Strike Down)` = ieepa_baseline_ieepa,
  `IEEPA Top-up (Strike Down)` = ieepa_topup_ieepa,
  `S232 (Strike Down)` = s232_ieepa,
  `Emergency (Strike Down)` = emergency_ieepa,
  `S301 (Strike Down)` = s301_ieepa,
  `Total (Strike Down)` = total_ieepa,
  `HTS (Surcharge)` = hts_surcharge,
  `IEEPA Baseline (Surcharge)` = ieepa_baseline_surcharge,
  `IEEPA Top-up (Surcharge)` = ieepa_topup_surcharge,
  `S232 (Surcharge)` = s232_surcharge,
  `Emergency (Surcharge)` = emergency_surcharge,
  `S301 (Surcharge)` = s301_surcharge,
  `Total (Surcharge)` = total_surcharge,
  `HTS (Stack)` = hts_stack,
  `IEEPA Baseline (Stack)` = ieepa_baseline_stack,
  `IEEPA Top-up (Stack)` = ieepa_topup_stack,
  `S232 (Stack)` = s232_stack,
  `Emergency (Stack)` = emergency_stack,
  `S301 (Stack)` = s301_stack,
  `Total (Stack)` = total_stack,
  `Change: Strike Down vs Before (pp)` = change_strike_vs_before,
  `Change: Surcharge vs Strike Down (pp)` = change_surcharge_vs_strike,
  `Change: Stack vs Strike Down (pp)` = change_stack_vs_strike
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
h3 <- slim_hs2(surcharge_hs2, "surcharge")
h4 <- slim_hs2(stack_hs2, "stack")

sector_comp <- Reduce(function(x, y) merge(x, y, by = "hs2", all.x = TRUE),
                      list(hs2_base, h1, h2, h3, h4))
sector_comp[, `:=`(
  change_strike_vs_before = round(total_ieepa - total_before, 2),
  change_surcharge_vs_strike = round(total_surcharge - total_ieepa, 2),
  change_stack_vs_strike = round(total_stack - total_ieepa, 2)
)]
sector_comp <- sector_comp[order(-total_imports_bn)]

sector_excel <- sector_comp[, .(
  `HS-2 Chapter` = hs2, `Chapter Name` = chapter_label,
  `Import Value ($bn)` = round(total_imports_bn, 2),
  `Total (Before)` = total_before,
  `Total (Strike Down)` = total_ieepa,
  `Total (Surcharge)` = total_surcharge,
  `Total (Stack)` = total_stack,
  `Change: Strike Down vs Before (pp)` = change_strike_vs_before,
  `Change: Surcharge vs Strike Down (pp)` = change_surcharge_vs_strike,
  `Change: Stack vs Strike Down (pp)` = change_stack_vs_strike
)]

# Sheet 3: Methodology
methodology_text <- data.table(
  Section = c("Data source", "Baseline date",
              "Scenario 1: Before SCOTUS",
              "Scenario 2: IEEPA Strike Down",
              "Scenario 3: S122 Flat Surcharge",
              "Scenario 4: S122 IEEPA Stack",
              "MFN rates", "Import values", "Methodology", "Contact"),
  Description = c(
    "Global Trade Alert US Tariff Barrier Estimates",
    sprintf("Policy effective date: %s", POLICY_DATE_LONG),
    "Current tariff regime as of the policy date, including all IEEPA, S232, S301, and emergency tariffs.",
    paste0("All tariffs imposed under IEEPA (baseline 10% + country-specific top-ups) and ",
           "emergency executive orders set to zero. S232, S301, and HTS rates unchanged."),
    paste0("S122 Flat Surcharge: After IEEPA strike-down, a flat 10% surcharge is imposed ",
           "on top of all remaining tariff rates under Section 122 authority (which permits up to 15%). ",
           "The surcharge is additive to existing HTS, S232, S301 rates."),
    paste0("S122 IEEPA Stack: After IEEPA strike-down, Congress uses Section 122 to reimpose ",
           "rates equivalent to the IEEPA structure but as a congressional tariff. ",
           "The S122 rate replaces IEEPA baseline and top-up but does not include emergency orders. ",
           "Effective rate = max(S122 rate, existing tariff)."),
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

saveWorkbook(wb1, "results/scotus/s122/data/s122_country_comparison.xlsx", overwrite = TRUE)
cat("   Saved: s122_country_comparison.xlsx\n")

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

flow_ieepa <- ieepa_data[, .(iso_code, hs_8digit, rate_ieepa = rate)]
flow_surcharge <- surcharge_data[, .(iso_code, hs_8digit, rate_surcharge = rate)]
flow_stack <- stack_data[, .(iso_code, hs_8digit, rate_stack = rate)]

flow_merged <- merge(flow_base, flow_ieepa, by = c("iso_code", "hs_8digit"), all.x = TRUE)
flow_merged <- merge(flow_merged, flow_surcharge, by = c("iso_code", "hs_8digit"), all.x = TRUE)
flow_merged <- merge(flow_merged, flow_stack, by = c("iso_code", "hs_8digit"), all.x = TRUE)

# Merge product names
flow_merged[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit := NULL]
flow_merged <- merge(flow_merged, product_names, by = "hs_8digit_int", all.x = TRUE)

flow_merged <- flow_merged[order(exporter, hs_8digit)]

flow_excel <- flow_merged[, .(
  Exporter = exporter,
  `ISO Code` = iso_code,
  `HS 8-digit` = hs_8digit,
  `Product Name` = product_name,
  `US Imports ($bn)` = round(us_imports_bn, 6),
  `Rate Before SCOTUS (%)` = round(rate_before, 2),
  `Rate IEEPA Strike Down (%)` = round(rate_ieepa, 2),
  `Rate S122 Surcharge (%)` = round(rate_surcharge, 2),
  `Rate S122 Stack (%)` = round(rate_stack, 2),
  `HTS/MFN Rate (%)` = round(hts_rate, 2),
  `S232 Rate (%)` = round(s232_rate, 2),
  `S301 Rate (%)` = round(s301_rate, 2)
)]

wb2 <- createWorkbook()
addWorksheet(wb2, "US Imports with Tariff Rates")
writeDataTable(wb2, "US Imports with Tariff Rates", flow_excel,
               tableStyle = "TableStyleMedium2")
freezePane(wb2, "US Imports with Tariff Rates", firstRow = TRUE)
setColWidths(wb2, "US Imports with Tariff Rates",
             cols = 1:ncol(flow_excel), widths = "auto")

saveWorkbook(wb2, "results/scotus/s122/data/s122_public_dataset.xlsx", overwrite = TRUE)
cat("   Saved: s122_public_dataset.xlsx\n")

# =============================================================================
# 10. GENERATE BLOG HTML DRAFT
# =============================================================================

cat("\n7. Generating blog HTML draft...\n")

# S3 base URL for images
S3_BASE <- "https://ricardo-dashboard.s3.eu-west-1.amazonaws.com/reports/scotus/s122"

blog_html <- sprintf('
<div class="title"><h3>Section 122 invoked: two interpretations of the new US tariff regime</h3></div><div class="paragraph"><p>The Supreme Court ruled in <em>Learning Resources v. Trump</em> that IEEPA does not authorise tariffs. In response, President Trump has invoked Section 122 of the Trade Act of 1974, which gives the President temporary authority to impose surcharges of up to 15%% on imports for balance-of-payments purposes. The IEEPA tariffs are gone. The question now is how the Section 122 surcharge interacts with the remaining tariff stack.</p>

<p>We model two interpretations. The trade-weighted average US tariff rate lands at either %.1f%% (flat surcharge) or %.1f%% (IEEPA-like stacking), compared with %.1f%% before the Supreme Court ruling and %.1f%% if no replacement had been enacted.</p>

<p><img src="%s/s122_aggregate_comparison.png" alt="Section 122 scenarios: aggregate US tariff composition" style="width:100%%%%;max-width:900px;" /></p></div>

<div class="title"><h3>Two interpretations of Section 122</h3></div><div class="paragraph"><p>There is genuine ambiguity about how the Section 122 surcharge interacts with tariffs that survived the ruling (HTS duties, Section 232 steel and aluminium tariffs, Section 301 China tariffs). We model two interpretations:</p>

<ul>
<li><strong>S122 Flat Surcharge (%.1f%%)</strong>: A flat 10%% surcharge is imposed on top of all remaining tariffs. This is the simplest interpretation: every import faces its existing rate plus 10 percentage points. The result is a trade-weighted average of %.1f%%, higher than even the pre-SCOTUS baseline.</li>
<li><strong>S122 IEEPA Stack (%.1f%%)</strong>: The Section 122 surcharge reimposes rates equivalent to the IEEPA structure, but the emergency orders are not restored. The S122 rate replaces the IEEPA baseline and top-ups where they exceed other rates. The result is %.1f%%, higher than the post-ruling rate but below the pre-SCOTUS level.</li>
</ul></div>

<div class="title"><h3>By country: top 20 import sources</h3></div><div class="paragraph"><p>The following chart compares all scenarios for the top 20 US import sources. The grey bars show the pre-ruling baseline and the navy bars show what remains after IEEPA is struck down. The orange and teal bars show the two Section 122 interpretations.</p>

<p><img src="%s/s122_country_comparison.png" alt="Section 122 scenarios: tariff rates by country" style="width:100%%%%;max-width:900px;" /></p>

<p>The impact varies substantially by trading partner. Countries that faced high IEEPA rates see the largest gaps between the two interpretations.</p></div>

<div class="title"><h3>By sector: HS-2 chapters</h3></div><div class="paragraph"><p>The sector-level view reveals where the two interpretations diverge most. Industries dominated by Section 232 products (steel, aluminium) see smaller variation, while sectors where IEEPA was the primary tariff instrument show the widest gaps.</p>

<p><img src="%s/s122_sector_comparison.png" alt="Section 122 scenarios: tariff rates by sector" style="width:100%%%%;max-width:900px;" /></p></div>

<div class="title"><h3>How the surcharge builds on the post-ruling base</h3></div><div class="paragraph"><p>The following chart decomposes the flat surcharge scenario into three layers for each of the top 20 import sources: the post-ruling base rate, the additional S122 stack increment, and the further surcharge top-up. Together, these three components sum to the S122 Flat Surcharge rate.</p>

<p><img src="%s/s122_surcharge_vs_stack.png" alt="Surcharge decomposition by country" style="width:100%%%%;max-width:900px;" /></p></div>

<div class="title"><h3>Downloads</h3></div><div class="paragraph"><p>The following datasets are available for download:</p>

<ul>
<li><a href="%s/s122_country_comparison.xlsx" target="_blank" rel="noopener"><strong>Country-level comparison</strong></a>: Tariff rates for all US import sources under both S122 interpretations, including EU aggregate and HS-2 sector breakdown</li>
<li><a href="%s/s122_public_dataset.xlsx" target="_blank" rel="noopener"><strong>Flow-level public dataset</strong></a>: Complete tariff rates at the exporter x HS 8-digit product level for all scenarios (274,000+ flows)</li>
</ul></div>

<div class="title"><h3>Methodology</h3></div><div class="paragraph"><p>Tariff rates are computed at the HS 8-digit product level for each exporting country and aggregated using 2024 US import values as weights. The analysis covers all US merchandise imports.</p>

<p>Four reference points are modelled:</p>
<ol>
<li><strong>Before SCOTUS</strong>: The tariff regime as of %s, including all IEEPA, S232, S301, and emergency tariffs.</li>
<li><strong>Post-ruling baseline</strong>: All IEEPA-authorised tariffs and emergency orders removed following the Supreme Court ruling.</li>
<li><strong>S122 Flat Surcharge</strong>: Post-ruling rates plus a flat 10%% surcharge under Section 122.</li>
<li><strong>S122 IEEPA Stack</strong>: Post-ruling rates with S122 reimposing the IEEPA rate structure (excluding emergency orders).</li>
</ol>

<p>For the full methodology, see the <a href="https://www.globaltradealert.org/reports/US-Tariff-Barrier-Estimates" target="_blank" rel="noopener">US Tariff Barrier Estimates</a> documentation.</p></div>',
  surcharge_global$total_rate, stack_global$total_rate,
  baseline_global$total_rate, ieepa_global$total_rate,
  S3_BASE,
  surcharge_global$total_rate, surcharge_global$total_rate,
  stack_global$total_rate, stack_global$total_rate,
  S3_BASE, S3_BASE, S3_BASE,
  S3_BASE, S3_BASE,
  POLICY_DATE_LONG
)

writeLines(blog_html, "results/scotus/s122/blog/s122_blog_body.html")
cat("   Saved: results/scotus/s122/blog/s122_blog_body.html\n")

# =============================================================================
# 11. SUMMARY
# =============================================================================

cat("\n=== OUTPUT GENERATION COMPLETE ===\n\n")
cat("Charts:\n")
cat("  results/scotus/s122/charts/s122_aggregate_comparison.png\n")
cat("  results/scotus/s122/charts/s122_aggregate_comparison_mobile.png\n")
cat("  results/scotus/s122/charts/s122_country_comparison.png\n")
cat("  results/scotus/s122/charts/s122_sector_comparison.png\n")
cat("  results/scotus/s122/charts/s122_sector_comparison_mobile.png\n")
cat("  results/scotus/s122/charts/s122_surcharge_vs_stack.png\n")
cat("\nExcel:\n")
cat("  results/scotus/s122/data/s122_country_comparison.xlsx\n")
cat("  results/scotus/s122/data/s122_public_dataset.xlsx\n")
cat("\nBlog:\n")
cat("  results/scotus/s122/blog/s122_blog_body.html\n")
cat(sprintf("\nKey numbers:\n"))
cat(sprintf("  Before SCOTUS:     %.2f%%\n", baseline_global$total_rate))
cat(sprintf("  IEEPA Strike Down: %.2f%%\n", ieepa_global$total_rate))
cat(sprintf("  S122 Surcharge:    %.2f%%\n", surcharge_global$total_rate))
cat(sprintf("  S122 Stack:        %.2f%%\n", stack_global$total_rate))
cat("\nNext steps:\n")
cat("  1. Review charts and blog HTML\n")
cat("  2. Upload to S3: aws s3 cp results/scotus/s122/ s3://ricardo-dashboard/reports/scotus/s122/ --recursive\n")
cat("  3. Build blog post: python3 code/scotus/build_s122_blog_post.py --dry-run\n")
