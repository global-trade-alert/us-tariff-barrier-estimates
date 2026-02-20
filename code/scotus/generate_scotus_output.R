# =============================================================================
# SCOTUS IEEPA STRIKE-DOWN: OUTPUT GENERATOR
# =============================================================================
# Master orchestrator for the SCOTUS blog output package.
# Generates 4 charts + 2 Excel files comparing before/after SCOTUS scenarios.
#
# Input:
#   - results/260220-260220/processed_us_imports_with_rates_schedule24_baseline.RData
#   - results/scenarios/ieepa_full_strike_down/processed_us_imports_with_rates_schedule24_ieepa_full_strike_down.RData
#
# Output:
#   - results/scotus/charts/   (4 charts, landscape + mobile variants)
#   - results/scotus/data/     (2 Excel files)
#   - results/scotus/blog/     (blog HTML for review)
#
# Usage:
#   cd us-tariff-barrier-estimates
#   Rscript code/scotus/generate_scotus_output.R
# =============================================================================

cat("=== SCOTUS IEEPA Strike-Down: Output Generator ===\n")
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
dirs <- c("results/scotus/charts", "results/scotus/data", "results/scotus/blog")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("   Output directories created.\n")

# =============================================================================
# 2. LOAD DATA
# =============================================================================

cat("\n2. Loading tariff data (both scenarios)...\n")

# Pre-SCOTUS baseline
baseline_file <- "results/260220-260220/processed_us_imports_with_rates_schedule24_baseline.RData"
if (!file.exists(baseline_file)) {
  # Try date-stamped directory pattern
  baseline_dirs <- list.dirs("results", recursive = FALSE)
  baseline_dirs <- baseline_dirs[grepl("260220", basename(baseline_dirs))]
  if (length(baseline_dirs) > 0) {
    candidate <- file.path(baseline_dirs[1], "processed_us_imports_with_rates_schedule24_baseline.RData")
    if (file.exists(candidate)) baseline_file <- candidate
  }
}
stopifnot("Baseline RData not found" = file.exists(baseline_file))

load(baseline_file)
before_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   Baseline: %s flows, $%.1f bn imports\n",
            format(nrow(before_data), big.mark = ","),
            sum(before_data$us_imports_bn)))

# Post-SCOTUS scenario
scenario_file <- "results/scenarios/ieepa_full_strike_down/processed_us_imports_with_rates_schedule24_ieepa_full_strike_down.RData"
stopifnot("IEEPA strike-down RData not found" = file.exists(scenario_file))

load(scenario_file)
after_data <- copy(us_imports)
rm(us_imports)
cat(sprintf("   IEEPA strike-down: %s flows, $%.1f bn imports\n",
            format(nrow(after_data), big.mark = ","),
            sum(after_data$us_imports_bn)))

# =============================================================================
# 3. COMPUTE CONTRIBUTIONS
# =============================================================================

cat("\n3. Computing tariff contributions (both scenarios)...\n")

cat("   Before SCOTUS:\n")
before_data <- compute_contributions(before_data, ieepa_baseline_rate = 10, verbose = TRUE)
validate_contributions(before_data, tolerance = 0.01, verbose = TRUE)

cat("   After SCOTUS:\n")
after_data <- compute_contributions(after_data, ieepa_baseline_rate = 10, verbose = TRUE)
validate_contributions(after_data, tolerance = 0.01, verbose = TRUE)

# =============================================================================
# 4. AGGREGATE: Global
# =============================================================================

cat("\n4. Aggregating compositions...\n")

# Global aggregates
before_global <- aggregate_contributions(before_data)
after_global  <- aggregate_contributions(after_data)

cat(sprintf("   Before: %.2f%%  |  After: %.2f%%  |  Change: %+.2f pp\n",
            before_global$total_rate, after_global$total_rate,
            after_global$total_rate - before_global$total_rate))

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
    total_rate           = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn     = sum(us_imports_bn)
  ), by = .(geography = exporter, iso_code)]

  country <- country[order(-total_imports_bn)]

  # EU aggregate
  eu <- aggregate_eu_composition(data)

  rbind(global, country, eu, fill = TRUE)
}

before_country <- aggregate_country(before_data)
after_country  <- aggregate_country(after_data)

cat(sprintf("   Before: %d geographies  |  After: %d geographies\n",
            nrow(before_country), nrow(after_country)))

# =============================================================================
# 6. AGGREGATE: HS-2 sector-level
# =============================================================================

cat("   Aggregating HS-2 sector compositions...\n")

aggregate_hs2 <- function(data) {
  # Extract HS-2 chapter from 8-digit code
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

  # Add chapter labels
  hs2_agg[, chapter_label := get_chapter_label(hs2)]
  hs2_agg <- hs2_agg[order(-total_imports_bn)]

  return(hs2_agg)
}

before_hs2 <- aggregate_hs2(before_data)
after_hs2  <- aggregate_hs2(after_data)

cat(sprintf("   Before: %d HS-2 chapters  |  After: %d HS-2 chapters\n",
            nrow(before_hs2), nrow(after_hs2)))

# =============================================================================
# 7. GENERATE CHARTS
# =============================================================================

cat("\n5. Generating charts...\n")

# --- Chart 1: Hero before/after ---
cat("   Chart 1: Aggregate before/after composition...\n")

p1 <- create_before_after_chart(before_global, after_global)
ggsave("results/scotus/charts/aggregate_before_after.png",
       plot = p1, width = 10, height = 5, dpi = 300, bg = GTA_BG)

p1m <- create_before_after_chart_mobile(before_global, after_global)
ggsave("results/scotus/charts/aggregate_before_after_mobile.png",
       plot = p1m, width = 5, height = 7, dpi = 300, bg = GTA_BG)

cat("   Saved: aggregate_before_after.png + mobile\n")

# --- Chart 2: Before vs after by country (paired bars) ---
cat("   Chart 2: Country before/after comparison...\n")

p2 <- create_country_comparison_chart(before_country, after_country)
ggsave("results/scotus/charts/country_before_after.png",
       plot = p2, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: country_before_after.png\n")

# --- Chart 2b: Before SCOTUS top 20 composition (supplementary) ---
cat("   Chart 2b: Before SCOTUS top 20 composition (supplementary)...\n")

p2b <- create_composition_chart_scotus(
  before_country,
  title    = "Before SCOTUS: US tariff composition for top 20 import sources",
  subtitle = sprintf("Trade-weighted average by tariff layer, effective %s", POLICY_DATE_LONG)
)
ggsave("results/scotus/charts/composition_top20_before.png",
       plot = p2b, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: composition_top20_before.png\n")

# --- Chart 3: After SCOTUS top 20 composition ---
cat("   Chart 3: After SCOTUS top 20 composition...\n")

p3 <- create_composition_chart_scotus(
  after_country,
  title    = "After SCOTUS: Section 232 becomes the dominant US tariff instrument",
  subtitle = sprintf("Trade-weighted average by tariff layer, after IEEPA + emergency orders struck down")
)
ggsave("results/scotus/charts/composition_top20_after.png",
       plot = p3, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: composition_top20_after.png\n")

# --- Chart 4: Sector before vs after ---
cat("   Chart 4: Sector comparison (HS-2)...\n")

p4 <- create_sector_comparison_chart(before_hs2, after_hs2)
ggsave("results/scotus/charts/sector_before_after.png",
       plot = p4, width = 12, height = 10, dpi = 300, bg = GTA_BG)

p4m <- create_sector_comparison_chart_mobile(before_hs2, after_hs2)
ggsave("results/scotus/charts/sector_before_after_mobile.png",
       plot = p4m, width = 6, height = 12, dpi = 300, bg = GTA_BG)

cat("   Saved: sector_before_after.png + mobile\n")

# --- Chart 5a: Top winners by tariff reduction ---
cat("   Chart 5a: Top winners by tariff reduction...\n")

p5a <- create_top_winners_chart(before_country, after_country, top_by = "reduction")
ggsave("results/scotus/charts/top_winners.png",
       plot = p5a, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: top_winners.png\n")

# --- Chart 5b: Top importers tariff reduction ---
cat("   Chart 5b: Top 20 importers tariff reduction...\n")

p5b <- create_top_winners_chart(before_country, after_country, top_by = "imports")
ggsave("results/scotus/charts/top_importers_reduction.png",
       plot = p5b, width = 12, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: top_importers_reduction.png\n")

# =============================================================================
# 8. GENERATE EXCEL 1: Country Comparison
# =============================================================================

cat("\n6. Generating Excel files...\n")
cat("   Excel 1: Country-level comparison...\n")

# Merge before and after country data
country_before_slim <- before_country[, .(
  geography, iso_code, total_imports_bn,
  hts_before = round(hts_layer, 2),
  ieepa_baseline_before = round(ieepa_baseline_layer, 2),
  ieepa_topup_before = round(ieepa_topup_layer, 2),
  s232_before = round(s232_layer, 2),
  emergency_before = round(emergency_layer, 2),
  s301_before = round(s301_layer, 2),
  total_before = round(total_rate, 2)
)]

country_after_slim <- after_country[, .(
  iso_code,
  hts_after = round(hts_layer, 2),
  ieepa_baseline_after = round(ieepa_baseline_layer, 2),
  ieepa_topup_after = round(ieepa_topup_layer, 2),
  s232_after = round(s232_layer, 2),
  emergency_after = round(emergency_layer, 2),
  s301_after = round(s301_layer, 2),
  total_after = round(total_rate, 2)
)]

country_comparison <- merge(country_before_slim, country_after_slim, by = "iso_code", all.x = TRUE)
country_comparison[, change_pp := round(total_after - total_before, 2)]
country_comparison <- country_comparison[order(-total_imports_bn)]

# Rename columns for Excel
country_excel <- country_comparison[, .(
  Country = geography,
  `ISO Code` = iso_code,
  `Import Value ($bn)` = round(total_imports_bn, 2),
  `HTS Rate (before)` = hts_before,
  `IEEPA Baseline (before)` = ieepa_baseline_before,
  `IEEPA Top-up (before)` = ieepa_topup_before,
  `S232 (before)` = s232_before,
  `Emergency (before)` = emergency_before,
  `S301 (before)` = s301_before,
  `Total Rate (before)` = total_before,
  `HTS Rate (after)` = hts_after,
  `IEEPA Baseline (after)` = ieepa_baseline_after,
  `IEEPA Top-up (after)` = ieepa_topup_after,
  `S232 (after)` = s232_after,
  `Emergency (after)` = emergency_after,
  `S301 (after)` = s301_after,
  `Total Rate (after)` = total_after,
  `Change (pp)` = change_pp
)]

# Sheet 2: Sector summary (HS-2)
sector_before_slim <- before_hs2[, .(
  hs2, chapter_label, total_imports_bn,
  total_before = round(total_rate, 2)
)]
sector_after_slim <- after_hs2[, .(hs2, total_after = round(total_rate, 2))]
sector_comparison <- merge(sector_before_slim, sector_after_slim, by = "hs2")
sector_comparison[, change_pp := round(total_after - total_before, 2)]
sector_comparison <- sector_comparison[order(-total_imports_bn)]

sector_excel <- sector_comparison[, .(
  `HS-2 Chapter` = hs2,
  `Chapter Name` = chapter_label,
  `Import Value ($bn)` = round(total_imports_bn, 2),
  `Total Rate (before)` = total_before,
  `Total Rate (after)` = total_after,
  `Change (pp)` = change_pp
)]

# Sheet 3: Methodology text
methodology_text <- data.table(
  Section = c("Data source", "Baseline date", "Scenario", "MFN rates",
              "Import values", "Methodology", "Contact"),
  Description = c(
    "Global Trade Alert US Tariff Barrier Estimates",
    sprintf("Policy effective date: %s", POLICY_DATE_LONG),
    paste0("IEEPA Full Strike-Down: All tariffs imposed under IEEPA (baseline 10% + ",
           "country-specific top-ups) and emergency executive orders are set to zero. ",
           "Section 232, Section 301, and HTS/MFN rates remain unchanged."),
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
setColWidths(wb1, "Methodology", cols = 1:2, widths = c(20, 80))

saveWorkbook(wb1, "results/scotus/data/scotus_country_comparison.xlsx", overwrite = TRUE)
cat("   Saved: scotus_country_comparison.xlsx\n")

# =============================================================================
# 9. GENERATE EXCEL 2: Flow-Level Public Dataset
# =============================================================================

cat("   Excel 2: Flow-level public dataset...\n")

# Load product names
product_names <- fread("data/us_hts-8_digit_sectioned.csv", sep = ";",
                       select = c("hs_8digit", "hs_8name"))
setnames(product_names, c("hs_8digit", "product_name"))

# Prepare flow-level data (merge before + after by exporter x hs_8digit)
flow_before <- before_data[, .(
  exporter, iso_code, hs_8digit, us_imports_bn,
  rate_before = rate,
  hts_rate, s232_rate,
  ieepa_rate_before = ieepa_rate,
  emergency_rate_before = emergency_rate,
  s301_rate
)]

flow_after <- after_data[, .(
  iso_code, hs_8digit,
  rate_after = rate
)]

flow_merged <- merge(flow_before, flow_after,
                     by = c("iso_code", "hs_8digit"),
                     all.x = TRUE)
flow_merged[, change_pp := round(rate_after - rate_before, 2)]

# Merge product names (join on integer hs_8digit, drop duplicate column to avoid .x/.y)
flow_merged[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit_int := as.integer(hs_8digit)]
product_names[, hs_8digit := NULL]
flow_merged <- merge(flow_merged, product_names, by = "hs_8digit_int", all.x = TRUE)

# Order by exporter then HS code
flow_merged <- flow_merged[order(exporter, hs_8digit)]

# Select and rename columns for Excel
flow_excel <- flow_merged[, .(
  Exporter = exporter,
  `ISO Code` = iso_code,
  `HS 8-digit` = hs_8digit,
  `Product Name` = product_name,
  `US Imports ($bn)` = round(us_imports_bn, 6),
  `Rate Before SCOTUS (%)` = round(rate_before, 2),
  `Rate After SCOTUS (%)` = round(rate_after, 2),
  `Change (pp)` = change_pp,
  `HTS/MFN Rate (%)` = round(hts_rate, 2),
  `S232 Rate (%)` = round(s232_rate, 2),
  `IEEPA Rate Before (%)` = round(ieepa_rate_before, 2),
  `Emergency Rate Before (%)` = round(emergency_rate_before, 2),
  `S301 Rate (%)` = round(s301_rate, 2)
)]

wb2 <- createWorkbook()
addWorksheet(wb2, "US Imports with Tariff Rates")
writeDataTable(wb2, "US Imports with Tariff Rates", flow_excel,
               tableStyle = "TableStyleMedium2")
freezePane(wb2, "US Imports with Tariff Rates", firstRow = TRUE)
setColWidths(wb2, "US Imports with Tariff Rates",
             cols = 1:ncol(flow_excel), widths = "auto")

saveWorkbook(wb2, "results/scotus/data/scotus_public_dataset.xlsx", overwrite = TRUE)
cat("   Saved: scotus_public_dataset.xlsx\n")

# =============================================================================
# 10. GENERATE BLOG HTML DRAFT
# =============================================================================

cat("\n7. Generating blog HTML draft...\n")

# S3 base URL for images (stable keys, not timestamped)
S3_BASE <- "https://ricardo-dashboard.s3.eu-west-1.amazonaws.com/reports/scotus"

blog_html <- sprintf('
<div class="title"><h3>The trade-weighted average US tariff drops from %.1f%% to %.1f%%</h3></div><div class="paragraph"><p>On 20 February 2026, the US Supreme Court ruled in <em>Learning Resources v. Trump</em> that the International Emergency Economic Powers Act (IEEPA) does not authorise the President to impose tariffs. This ruling strikes down the single largest source of tariff increases imposed since January 2025.</p>

<p>The trade-weighted average tariff on all US imports falls by %.1f percentage points, from %.1f%% to %.1f%%.</p>

<p><img src="%s/aggregate_before_after.png" alt="Before and after SCOTUS: aggregate US tariff composition" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>What was struck down</h3></div><div class="paragraph"><p>The ruling invalidates all tariffs imposed under IEEPA authority:</p>

<ul>
<li><strong>IEEPA baseline tariff (10%%)</strong> applied to imports from all countries except Canada, Mexico, and USMCA-compliant goods</li>
<li><strong>IEEPA country-specific top-ups</strong> including the additional tariffs on China (currently at 145%% total IEEPA rate), the EU (20%%), and other trading partners</li>
<li><strong>Emergency executive orders</strong> including the China fentanyl emergency surcharge and the Canada/Mexico border emergency tariffs</li>
<li><strong>IEEPA tariff floors</strong> that prevented other tariff instruments from reducing the effective rate below the IEEPA level</li>
</ul></div>

<div class="title"><h3>What remains in force</h3></div><div class="paragraph"><p>The following tariff instruments were imposed under separate legal authorities and are unaffected by the ruling:</p>

<ul>
<li><strong>Section 232 tariffs</strong> on steel (25%%), aluminium (25%%), copper (25%%), lumber (25%%), and automobiles/parts (25%%)</li>
<li><strong>Section 301 tariffs</strong> on Chinese goods (various rates, originally imposed 2018-2019)</li>
<li><strong>HTS/MFN baseline rates</strong> including USMCA and KORUS preferential rates</li>
</ul></div>

<div class="title"><h3>Before vs after: by country</h3></div><div class="paragraph"><p>The tariff reduction is visible across all trading partners. The following chart compares before and after rates for the top 20 US import sources.</p>

<p><img src="%s/country_before_after.png" alt="Before vs after SCOTUS: tariff rates by country" style="width:100%%;max-width:900px;" /></p>

<p>China remains the most heavily tariffed origin due to the remaining Section 301 duties. The chart below shows the post-SCOTUS tariff composition, where Section 232 (orange) has become the dominant instrument for most trading partners.</p>

<p><img src="%s/composition_top20_after.png" alt="After SCOTUS: tariff composition by country" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>Biggest tariff cuts: by country</h3></div><div class="paragraph"><p>Among the largest US import sources, the tariff cuts range widely. The following chart shows the reduction for the top 20 import origins.</p>

<p><img src="%s/top_importers_reduction.png" alt="Tariff cuts for the top 20 US import sources" style="width:100%%;max-width:900px;" /></p>

<p>Looking across all trading partners, the largest tariff reductions occur for countries that faced the highest IEEPA top-ups.</p>

<p><img src="%s/top_winners.png" alt="Which US import sources see the largest tariff cuts" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>Before vs after: by sector</h3></div><div class="paragraph"><p>The tariff reduction varies considerably across sectors. Industries dominated by Section 232 products (steel, aluminium, motor vehicles) see smaller reductions, while sectors where IEEPA was the primary tariff instrument see the largest drops.</p>

<p><img src="%s/sector_before_after.png" alt="Sector comparison: before vs after SCOTUS" style="width:100%%;max-width:900px;" /></p></div>

<div class="title"><h3>Downloads</h3></div><div class="paragraph"><p>The following datasets are available for download:</p>

<ul>
<li><a href="%s/scotus_country_comparison.xlsx" target="_blank" rel="noopener"><strong>Country-level comparison</strong></a>: Before and after tariff rates for all US import sources, including EU aggregate and HS-2 sector breakdown</li>
<li><a href="%s/scotus_public_dataset.xlsx" target="_blank" rel="noopener"><strong>Flow-level public dataset</strong></a>: Complete tariff rates at the exporter x HS 8-digit product level for both scenarios (274,000+ flows)</li>
</ul></div>

<div class="title"><h3>Methodology</h3></div><div class="paragraph"><p>Tariff rates are computed at the HS 8-digit product level for each exporting country and aggregated using 2024 US import values as weights. The analysis covers all US merchandise imports.</p>

<p>The IEEPA full strike-down scenario sets all IEEPA-authorised tariff rates (baseline, top-ups, and floors) and emergency executive order rates to zero, while preserving all other tariff instruments at their current levels.</p>

<p>For the full methodology, see the <a href="https://www.globaltradealert.org/reports/US-Tariff-Barrier-Estimates" target="_blank" rel="noopener">US Tariff Barrier Estimates</a> documentation.</p></div>',
  before_global$total_rate, after_global$total_rate,
  before_global$total_rate - after_global$total_rate,
  before_global$total_rate, after_global$total_rate,
  S3_BASE, S3_BASE, S3_BASE, S3_BASE, S3_BASE, S3_BASE, S3_BASE, S3_BASE
)

# Write blog HTML for review
writeLines(blog_html, "results/scotus/blog/scotus_blog_body.html")
cat("   Saved: results/scotus/blog/scotus_blog_body.html\n")

# =============================================================================
# 11. SUMMARY
# =============================================================================

cat("\n=== OUTPUT GENERATION COMPLETE ===\n\n")
cat("Charts:\n")
cat("  results/scotus/charts/aggregate_before_after.png\n")
cat("  results/scotus/charts/aggregate_before_after_mobile.png\n")
cat("  results/scotus/charts/country_before_after.png\n")
cat("  results/scotus/charts/composition_top20_after.png\n")
cat("  results/scotus/charts/sector_before_after.png\n")
cat("  results/scotus/charts/sector_before_after_mobile.png\n")
cat("\nExcel:\n")
cat("  results/scotus/data/scotus_country_comparison.xlsx\n")
cat("  results/scotus/data/scotus_public_dataset.xlsx\n")
cat("\nBlog:\n")
cat("  results/scotus/blog/scotus_blog_body.html\n")
cat(sprintf("\nKey numbers: %.2f%% -> %.2f%% (-%0.2f pp)\n",
            before_global$total_rate, after_global$total_rate,
            before_global$total_rate - after_global$total_rate))
cat("\nNext steps:\n")
cat("  1. Review charts and blog HTML\n")
cat("  2. Upload to S3: python3 code/upload_results.py --run-dir results/scotus\n")
cat("  3. Build blog post: python3 code/scotus/build_scotus_blog_post.py --dry-run\n")
