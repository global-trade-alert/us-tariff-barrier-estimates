#!/usr/bin/env Rscript
# =============================================================================
# ENHANCE S122 ACTUAL BLOG POST CHARTS (Blog Post 3)
# =============================================================================
# Generates all 7 charts (desktop + mobile) for the S122 actual blog post.
# 4 scenarios — all from actual simulation data:
#   1. Pre-ruling (light grey #AAAAAA) — results/260221-260219
#   2. Post-ruling / IEEPA Strike Down (dark grey #666666) — results/260222-260221-schedule24
#   3. S122 @ 10% (navy #003366) — results/260221-260224
#   4. S122 @ 15% (teal #2CA58D) — results/260222-260224-schedule24
#
# Charts:
#   A. Country comparison dot plot (4 dots)
#   B. Sector comparison dot plot (4 dots)
#   C. Relative tariff advantage (3 dots: pre-ruling, S122@10%, S122@15%)
#   D. Aggregate composition (4 stacked bars)
#   E. S122 contribution decomposition (2-layer stacked bars, S122@15%)
#   F. Top importers reduction (Before vs S122@15%)
#   G. Top winners by tariff cut (Before vs S122@15%)
#
# Each chart has a desktop and mobile variant + Excel companion.
# =============================================================================

library(data.table)
library(ggplot2)
library(cowplot)
library(openxlsx)

source("code/scotus/scotus_chart_functions.R")
source("code/compute_contributions.R")

cat("=== ENHANCE S122 ACTUAL CHARTS ===\n\n")

# Output directories
CHART_DIR <- "results/scotus/s122_actual/charts"
DATA_DIR  <- "results/scotus/s122_actual/data"
dir.create(CHART_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

S3_BASE <- "https://ricardo-dashboard.s3.eu-west-1.amazonaws.com/reports/scotus/s122_actual"

# =============================================================================
# LOAD DATA — 4 scenarios, all from actual simulations
# =============================================================================

cat("1. Loading tariff data (4 scenarios)...\n")

# Scenario 1: Before SCOTUS (pre-ruling tariff regime as of 19 Feb 2026)
load("results/260221-260219/processed_us_imports_with_rates_schedule24_baseline.RData")
baseline_data <- as.data.table(us_imports)
baseline_data <- compute_contributions(baseline_data)
baseline_data[, s122_contribution := 0]
cat("   Before SCOTUS: loaded\n")

# Scenario 2: IEEPA Strike Down (all IEEPA tariffs removed)
load("results/260222-260221-schedule24/processed_us_imports_with_rates_schedule24_baseline.RData")
ieepa_data <- as.data.table(us_imports)
ieepa_data <- compute_contributions(ieepa_data)
ieepa_data[, s122_contribution := 0]
cat("   IEEPA Strike Down: loaded\n")

# Scenario 3: S122 @ 10% (initial Section 122 proclamation)
load("results/260221-260224/processed_us_imports_with_rates_schedule24_baseline.RData")
s122_data <- as.data.table(us_imports)
s122_data <- compute_contributions(s122_data)
s122_data[, s122_contribution := rate - (hts_contribution + ieepa_contribution +
                                          s232_contribution + emergency_contribution +
                                          s301_contribution)]
s122_data[, s122_contribution := pmax(0, s122_contribution)]
cat("   S122 @ 10%: loaded\n")

# Scenario 4: S122 @ 15% (actual simulation — full RData with decomposition)
load("results/260222-260224-schedule24/processed_us_imports_with_rates_schedule24_baseline.RData")
s122_15_data <- as.data.table(us_imports)
s122_15_data <- compute_contributions(s122_15_data)
s122_15_data[, s122_contribution := rate - (hts_contribution + ieepa_contribution +
                                             s232_contribution + emergency_contribution +
                                             s301_contribution)]
s122_15_data[, s122_contribution := pmax(0, s122_contribution)]
cat("   S122 @ 15%: loaded (full RData)\n")

cat("   All 4 scenarios loaded from actual simulations.\n\n")

# Country aggregation
aggregate_country_simple <- function(data) {
  country <- data[, .(
    total_rate      = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn = sum(us_imports_bn)
  ), by = .(geography = exporter, iso_code)]
  country <- country[order(-total_imports_bn)]

  # EU aggregate
  eu_data <- data[iso_code %in% EU_MEMBERS_ISO]
  if (nrow(eu_data) > 0) {
    eu <- data.table(
      geography = "European Union", iso_code = "EU",
      total_rate = weighted.mean(eu_data$rate, eu_data$us_imports_bn, na.rm = TRUE),
      total_imports_bn = sum(eu_data$us_imports_bn)
    )
    country <- rbind(country, eu)
  }

  # Global
  global <- data.table(
    geography = "Global", iso_code = "WLD",
    total_rate = weighted.mean(data$rate, data$us_imports_bn, na.rm = TRUE),
    total_imports_bn = sum(data$us_imports_bn)
  )
  rbind(global, country[order(-total_imports_bn)])
}

baseline_country <- aggregate_country_simple(baseline_data)
ieepa_country    <- aggregate_country_simple(ieepa_data)
s122_country     <- aggregate_country_simple(s122_data)
s122_15_country  <- aggregate_country_simple(s122_15_data)

# HS-2 aggregation
aggregate_hs2_simple <- function(data) {
  data[, hs2 := sprintf("%02d", as.integer(substr(sprintf("%08d", as.integer(hs_8digit)), 1, 2)))]
  hs2_agg <- data[, .(
    total_rate       = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn = sum(us_imports_bn)
  ), by = hs2]
  hs2_agg[, chapter_label := get_chapter_label(hs2)]
  hs2_agg[order(-total_imports_bn)]
}

baseline_hs2 <- aggregate_hs2_simple(baseline_data)
ieepa_hs2    <- aggregate_hs2_simple(ieepa_data)
s122_hs2     <- aggregate_hs2_simple(s122_data)
s122_15_hs2  <- aggregate_hs2_simple(s122_15_data)

cat(sprintf("   S122 @ 15%% simulation: global rate = %.2f%%\n",
            s122_15_country[geography == "Global"]$total_rate))
cat("   Aggregated country and sector data (4 scenarios).\n\n")


# =============================================================================
# GLOBAL AGGREGATES (for Charts D & E)
# =============================================================================

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

baseline_global <- aggregate_contributions_s122(baseline_data)
ieepa_global    <- aggregate_contributions_s122(ieepa_data)
s122_global     <- aggregate_contributions_s122(s122_data)
s122_15_global  <- aggregate_contributions_s122(s122_15_data)

cat(sprintf("   Global rates: Before=%.2f%%, IEEPA=%.2f%%, S122@10%%=%.2f%%, S122@15%%=%.2f%%\n",
            baseline_global$total_rate, ieepa_global$total_rate,
            s122_global$total_rate, s122_15_global$total_rate))


# =============================================================================
# CHART A: COUNTRY COMPARISON — CONNECTED DOT PLOT (4 dots)
# =============================================================================

cat("\n2. Creating country comparison dot plot (4 scenarios)...\n")

# Merge 4 scenarios for top 20 + EU + Global
country_merged <- Reduce(function(x, y) merge(x, y, by = c("geography", "iso_code"), all = FALSE),
  list(
    baseline_country[, .(geography, iso_code, rate_before = total_rate, imports_bn = total_imports_bn)],
    ieepa_country[, .(geography, iso_code, rate_ieepa = total_rate)],
    s122_country[, .(geography, iso_code, rate_s122 = total_rate)],
    s122_15_country[, .(geography, iso_code, rate_s122_15 = total_rate)]
  ))
country_merged <- country_merged[order(-imports_bn)]

# Top 20 by imports (exclude Global, keep EU)
top20 <- country_merged[geography != "Global"][1:min(21, nrow(country_merged) - 1)]
global_row <- country_merged[geography == "Global"]
dot_data <- rbind(global_row, top20)

# Display: ISO codes for compactness — except Global and EU
dot_data[, display_label := ifelse(
  geography %in% c("Global", "European Union"),
  geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]

# Order: Global at top, then by before-SCOTUS rate descending
dot_data[geography != "Global", rank := rank(-rate_before)]
dot_data[geography == "Global", rank := 0]
dot_data <- dot_data[order(-rank)]
dot_data[, display_label := factor(display_label, levels = display_label)]

# Pivot to long for 4 dots
dot_long <- melt(dot_data,
  id.vars = c("geography", "iso_code", "imports_bn", "display_label", "rank"),
  measure.vars = c("rate_before", "rate_ieepa", "rate_s122", "rate_s122_15"),
  variable.name = "scenario", value.name = "rate")

dot_long[, scenario_label := factor(scenario,
  levels = SCENARIO_LEVELS_BLOG3,
  labels = SCENARIO_LABELS_BLOG3)]

# Build inner plot — labels at same horizontal level as dots (vjust=0.5)
p_inner <- ggplot() +
  # Connecting segments (range line)
  geom_segment(data = dot_data,
    aes(x = display_label, xend = display_label,
        y = pmin(rate_ieepa, rate_s122_15), yend = pmax(rate_before, rate_s122_15)),
    color = "grey70", linewidth = 0.6) +
  # Dots (4 scenarios)
  geom_point(data = dot_long,
    aes(x = display_label, y = rate, color = scenario_label),
    size = 4) +
  # Grey dot labels: same level, left/right of dot
  geom_text(data = dot_long[scenario == "rate_before"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.0, color = "#AAAAAA") +
  geom_text(data = dot_long[scenario == "rate_ieepa"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.0, color = "#999999") +
  # S122 labels: same level, bold
  geom_text(data = dot_long[scenario == "rate_s122"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.5, color = "#003366", fontface = "bold") +
  geom_text(data = dot_long[scenario == "rate_s122_15"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.5, color = "#2CA58D", fontface = "bold") +
  scale_color_manual(values = SCENARIO_COLORS_BLOG3, name = NULL) +
  scale_y_continuous(position = "right", labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.10))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

# Highlight Global row with subtle shading
p_inner <- p_inner +
  annotate("rect",
    xmin = nrow(dot_data) - 0.5, xmax = nrow(dot_data) + 0.5,
    ymin = -Inf, ymax = Inf,
    fill = "grey90", alpha = 0.3)

p_country_dot <- gta_wrap(
  p_inner,
  title    = "Four scenarios: tariff rates for top 20 US import sources",
  subtitle = "Connected dots show the range. Top 20 by 2024 US import value."
)

ggsave(file.path(CHART_DIR, "s122_actual_country_comparison.png"),
       plot = p_country_dot, width = 12, height = 14, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_country_comparison.png\n")

# --- Mobile variant: top 10 + Global ---
dot_data_m <- rbind(global_row, top20[1:min(10, nrow(top20))])
dot_data_m[geography != "Global", rank := rank(-rate_before)]
dot_data_m[geography == "Global", rank := 0]
dot_data_m <- dot_data_m[order(-rank)]
dot_data_m[, display_label := ifelse(
  geography %in% c("Global", "European Union"), geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
dot_data_m[, display_label := factor(display_label, levels = display_label)]

dot_long_m <- melt(dot_data_m,
  id.vars = c("geography", "iso_code", "imports_bn", "display_label", "rank"),
  measure.vars = c("rate_before", "rate_ieepa", "rate_s122", "rate_s122_15"),
  variable.name = "scenario", value.name = "rate")
dot_long_m[, scenario_label := factor(scenario,
  levels = SCENARIO_LEVELS_BLOG3, labels = SCENARIO_LABELS_BLOG3)]

p_inner_m <- ggplot() +
  geom_segment(data = dot_data_m,
    aes(x = display_label, xend = display_label,
        y = pmin(rate_ieepa, rate_s122_15), yend = pmax(rate_before, rate_s122_15)),
    color = "grey70", linewidth = 0.6) +
  geom_point(data = dot_long_m,
    aes(x = display_label, y = rate, color = scenario_label),
    size = 4) +
  geom_text(data = dot_long_m[scenario == "rate_before"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.0, color = "#AAAAAA") +
  geom_text(data = dot_long_m[scenario == "rate_ieepa"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.0, color = "#999999") +
  geom_text(data = dot_long_m[scenario == "rate_s122"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.5, color = "#003366", fontface = "bold") +
  geom_text(data = dot_long_m[scenario == "rate_s122_15"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.5, color = "#2CA58D", fontface = "bold") +
  scale_color_manual(values = SCENARIO_COLORS_BLOG3, name = NULL) +
  scale_y_continuous(position = "right", labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.10))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4)))

p_country_dot_m <- gta_wrap(p_inner_m,
  title    = "Four scenarios: tariff rates for top US import sources",
  subtitle = "Top 10 by 2024 US import value")

ggsave(file.path(CHART_DIR, "s122_actual_country_comparison_mobile.png"),
       plot = p_country_dot_m, width = 7, height = 12, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_country_comparison_mobile.png\n")

# Country comparison Excel companion
country_excel <- dot_data[, .(
  `Import Origin` = geography,
  `ISO Code` = iso_code,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Before SCOTUS (%)` = round(rate_before, 2),
  `IEEPA Strike Down (%)` = round(rate_ieepa, 2),
  `S122 @ 10% (%)` = round(rate_s122, 2),
  `S122 @ 15% (%)` = round(rate_s122_15, 2),
  `Change: Before to S122@15% (pp)` = round(rate_before - rate_s122_15, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "Country Comparison")
writeDataTable(wb, 1, country_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(country_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_country_comparison.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_country_comparison.xlsx\n")


# =============================================================================
# CHART B: SECTOR COMPARISON — CONNECTED DOT PLOT (4 dots)
# =============================================================================

cat("\n3. Creating sector comparison dot plot (4 scenarios)...\n")

sector_merged <- Reduce(function(x, y) merge(x, y, by = c("hs2", "chapter_label"), all = FALSE),
  list(
    baseline_hs2[, .(hs2, chapter_label, rate_before = total_rate, imports_bn = total_imports_bn)],
    ieepa_hs2[, .(hs2, chapter_label, rate_ieepa = total_rate)],
    s122_hs2[, .(hs2, chapter_label, rate_s122 = total_rate)],
    s122_15_hs2[, .(hs2, chapter_label, rate_s122_15 = total_rate)]
  ))
sector_merged <- sector_merged[order(-imports_bn)]

# Top 15 by import value
top_sectors <- sector_merged[1:min(15, nrow(sector_merged))]

top_sectors[, display_label := chapter_label]
top_sectors[, rank := rank(-rate_before)]
top_sectors <- top_sectors[order(-rank)]
top_sectors[, display_label := factor(display_label, levels = display_label)]

sector_long <- melt(top_sectors,
  id.vars = c("hs2", "chapter_label", "imports_bn", "display_label", "rank"),
  measure.vars = c("rate_before", "rate_ieepa", "rate_s122", "rate_s122_15"),
  variable.name = "scenario", value.name = "rate")

sector_long[, scenario_label := factor(scenario,
  levels = SCENARIO_LEVELS_BLOG3,
  labels = SCENARIO_LABELS_BLOG3)]

p_sector_inner <- ggplot() +
  geom_segment(data = top_sectors,
    aes(x = display_label, xend = display_label,
        y = pmin(rate_ieepa, rate_s122_15), yend = pmax(rate_before, rate_s122_15)),
    color = "grey70", linewidth = 0.6) +
  geom_point(data = sector_long,
    aes(x = display_label, y = rate, color = scenario_label),
    size = 4) +
  geom_text(data = sector_long[scenario == "rate_before"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.0, color = "#AAAAAA") +
  geom_text(data = sector_long[scenario == "rate_ieepa"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.0, color = "#999999") +
  geom_text(data = sector_long[scenario == "rate_s122"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.5, color = "#003366", fontface = "bold") +
  geom_text(data = sector_long[scenario == "rate_s122_15"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.5, color = "#2CA58D", fontface = "bold") +
  scale_color_manual(values = SCENARIO_COLORS_BLOG3, name = NULL) +
  scale_y_continuous(position = "right", labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.10))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

p_sector_dot <- gta_wrap(
  p_sector_inner,
  title    = "Four scenarios: tariff rates by sector",
  subtitle = "Top 15 HS-2 chapters by 2024 US import value."
)

ggsave(file.path(CHART_DIR, "s122_actual_sector_comparison.png"),
       plot = p_sector_dot, width = 12, height = 11, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_sector_comparison.png\n")

# --- Mobile variant: top 8 sectors ---
top_sectors_m <- sector_merged[1:min(8, nrow(sector_merged))]
top_sectors_m[, display_label := chapter_label]
top_sectors_m[, rank := rank(-rate_before)]
top_sectors_m <- top_sectors_m[order(-rank)]
top_sectors_m[, display_label := factor(display_label, levels = display_label)]

sector_long_m <- melt(top_sectors_m,
  id.vars = c("hs2", "chapter_label", "imports_bn", "display_label", "rank"),
  measure.vars = c("rate_before", "rate_ieepa", "rate_s122", "rate_s122_15"),
  variable.name = "scenario", value.name = "rate")
sector_long_m[, scenario_label := factor(scenario,
  levels = SCENARIO_LEVELS_BLOG3, labels = SCENARIO_LABELS_BLOG3)]

p_sector_inner_m <- ggplot() +
  geom_segment(data = top_sectors_m,
    aes(x = display_label, xend = display_label,
        y = pmin(rate_ieepa, rate_s122_15), yend = pmax(rate_before, rate_s122_15)),
    color = "grey70", linewidth = 0.6) +
  geom_point(data = sector_long_m,
    aes(x = display_label, y = rate, color = scenario_label),
    size = 4) +
  geom_text(data = sector_long_m[scenario == "rate_s122"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = 1.3, vjust = 0.5, size = 3.5, color = "#003366", fontface = "bold") +
  geom_text(data = sector_long_m[scenario == "rate_s122_15"],
    aes(x = display_label, y = rate, label = sprintf("%.1f", rate)),
    hjust = -0.3, vjust = 0.5, size = 3.5, color = "#2CA58D", fontface = "bold") +
  scale_color_manual(values = SCENARIO_COLORS_BLOG3, name = NULL) +
  scale_y_continuous(position = "right", labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.10))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4)))

p_sector_dot_m <- gta_wrap(p_sector_inner_m,
  title    = "Four scenarios: tariff rates by sector",
  subtitle = "Top 8 HS-2 chapters by import value")

ggsave(file.path(CHART_DIR, "s122_actual_sector_comparison_mobile.png"),
       plot = p_sector_dot_m, width = 7, height = 10, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_sector_comparison_mobile.png\n")

# Sector comparison Excel
sector_excel <- top_sectors[order(-imports_bn), .(
  `HS-2 Chapter` = hs2,
  `Sector` = chapter_label,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Before SCOTUS (%)` = round(rate_before, 2),
  `IEEPA Strike Down (%)` = round(rate_ieepa, 2),
  `S122 @ 10% (%)` = round(rate_s122, 2),
  `S122 @ 15% (%)` = round(rate_s122_15, 2),
  `Change: Before to S122@15% (pp)` = round(rate_before - rate_s122_15, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "Sector Comparison")
writeDataTable(wb, 1, sector_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(sector_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_sector_comparison.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_sector_comparison.xlsx\n")


# =============================================================================
# CHART C: RELATIVE TARIFF ADVANTAGE (3 dots: pre-ruling, S122@10%, S122@15%)
# =============================================================================

cat("\n4. Computing relative tariff advantage...\n")

compute_relative_advantage <- function(data) {
  data[, effective_rate := rate]

  # Total imports and tariff-weighted totals by HS code
  hs_totals <- data[, .(
    total_imports_by_hs     = sum(us_imports_bn),
    total_tariff_weighted   = sum(effective_rate * us_imports_bn)
  ), by = hs_8digit]

  dt <- merge(data, hs_totals, by = "hs_8digit", all.x = TRUE)

  # Competitor weighted average (excluding own country)
  dt[, competitor_rate := ifelse(
    total_imports_by_hs - us_imports_bn > 0,
    (total_tariff_weighted - effective_rate * us_imports_bn) / (total_imports_by_hs - us_imports_bn),
    NA_real_
  )]

  # Advantage = competitor rate - own rate (positive = advantage)
  dt[, tariff_advantage := competitor_rate - effective_rate]

  # Country summary
  country_adv <- dt[!is.na(competitor_rate), .(
    total_imports_bn     = sum(us_imports_bn),
    avg_own_rate         = weighted.mean(effective_rate, us_imports_bn),
    avg_competitor_rate  = weighted.mean(competitor_rate, us_imports_bn),
    avg_tariff_advantage = weighted.mean(tariff_advantage, us_imports_bn)
  ), by = .(geography = exporter, iso_code)]

  country_adv[order(-total_imports_bn)]
}

baseline_adv <- compute_relative_advantage(copy(baseline_data))
s122_adv     <- compute_relative_advantage(copy(s122_data))
s122_15_adv  <- compute_relative_advantage(copy(s122_15_data))

cat(sprintf("   Pre-ruling: %d countries with advantage data\n", nrow(baseline_adv)))
cat(sprintf("   S122 @ 10%%: %d countries\n", nrow(s122_adv)))
cat(sprintf("   S122 @ 15%%: %d countries\n", nrow(s122_15_adv)))

# Merge for 3-dot chart
adv_merged <- merge(
  baseline_adv[, .(geography, iso_code, imports_bn = total_imports_bn,
                   adv_before = avg_tariff_advantage, own_before = avg_own_rate)],
  s122_adv[, .(geography, iso_code,
               adv_s122 = avg_tariff_advantage, own_s122 = avg_own_rate)],
  by = c("geography", "iso_code")
)
adv_merged[, adv_s122_15 := s122_15_adv[match(adv_merged$iso_code, iso_code)]$avg_tariff_advantage]

# Top 20 by import value
adv_top20 <- adv_merged[order(-imports_bn)][1:20]

# Order by before-SCOTUS advantage (most advantaged at top)
adv_top20 <- adv_top20[order(adv_before)]
adv_top20[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
adv_top20[, display_label := factor(display_label, levels = display_label)]

ADV_COLORS <- c(
  "Pre-ruling" = "#AAAAAA",
  "S122 10%"   = "#003366",
  "S122 15%"   = "#2CA58D"
)

p_adv_inner <- ggplot() +
  # Connecting segments
  geom_segment(data = adv_top20,
    aes(x = display_label, xend = display_label,
        y = pmin(adv_s122, adv_before, adv_s122_15, na.rm = TRUE),
        yend = pmax(adv_s122, adv_before, adv_s122_15, na.rm = TRUE)),
    color = "grey70", linewidth = 0.6) +
  # Zero reference line
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "solid") +
  # Dots: grey = pre-ruling, navy = S122@10%, teal = S122@15%
  geom_point(data = adv_top20,
    aes(x = display_label, y = adv_before), color = "#AAAAAA", size = 4) +
  geom_point(data = adv_top20,
    aes(x = display_label, y = adv_s122), color = "#003366", size = 4) +
  geom_point(data = adv_top20,
    aes(x = display_label, y = adv_s122_15), color = "#2CA58D", size = 4) +
  # Labels — all at same horizontal level
  geom_text(data = adv_top20,
    aes(x = display_label, y = adv_before,
        label = sprintf("%+.1f", adv_before),
        hjust = ifelse(adv_before > adv_s122, -0.3, 1.3)),
    size = 3.0, color = "#AAAAAA", vjust = 0.5) +
  geom_text(data = adv_top20,
    aes(x = display_label, y = adv_s122,
        label = sprintf("%+.1f", adv_s122),
        hjust = ifelse(adv_s122 < adv_before, 1.3, -0.3)),
    size = 3.5, color = "#003366", fontface = "bold", vjust = 0.5) +
  # Manual legend via dummy aesthetics
  geom_point(data = data.table(x = rep(adv_top20$display_label[1], 3),
                                y = rep(NA_real_, 3),
                                sc = factor(c("Pre-ruling", "S122 10%", "S122 15%"),
                                            levels = names(ADV_COLORS))),
    aes(x = x, y = y, colour = sc), size = 4, na.rm = TRUE) +
  scale_colour_manual(values = ADV_COLORS, name = NULL) +
  scale_y_continuous(
    labels = function(x) paste0(x, " pp"),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

p_adv <- gta_wrap(
  p_adv_inner,
  title    = "The tariff advantage flattened under Section 122",
  subtitle = "Relative tariff advantage vs competitors (pp). Positive = lower tariff than competitors."
)

ggsave(file.path(CHART_DIR, "s122_actual_relative_advantage.png"),
       plot = p_adv, width = 12, height = 13, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_relative_advantage.png\n")

# --- Mobile variant: top 10 ---
adv_top10 <- adv_merged[order(-imports_bn)][1:10]
adv_top10 <- adv_top10[order(adv_before)]
adv_top10[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
adv_top10[, display_label := factor(display_label, levels = display_label)]

p_adv_inner_m <- ggplot() +
  geom_segment(data = adv_top10,
    aes(x = display_label, xend = display_label,
        y = pmin(adv_s122, adv_before, adv_s122_15, na.rm = TRUE),
        yend = pmax(adv_s122, adv_before, adv_s122_15, na.rm = TRUE)),
    color = "grey70", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
  geom_point(data = adv_top10,
    aes(x = display_label, y = adv_before), color = "#AAAAAA", size = 4) +
  geom_point(data = adv_top10,
    aes(x = display_label, y = adv_s122), color = "#003366", size = 4) +
  geom_point(data = adv_top10,
    aes(x = display_label, y = adv_s122_15), color = "#2CA58D", size = 4) +
  geom_point(data = data.table(x = rep(adv_top10$display_label[1], 3),
                                y = rep(NA_real_, 3),
                                sc = factor(c("Pre-ruling", "S122 10%", "S122 15%"),
                                            levels = names(ADV_COLORS))),
    aes(x = x, y = y, colour = sc), size = 4, na.rm = TRUE) +
  scale_colour_manual(values = ADV_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, " pp"),
                     expand = expansion(mult = c(0.12, 0.12))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 5, 0),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

p_adv_m <- gta_wrap(p_adv_inner_m,
  title    = "Tariff advantage flattened under S122",
  subtitle = "Relative advantage vs competitors (pp)")

ggsave(file.path(CHART_DIR, "s122_actual_relative_advantage_mobile.png"),
       plot = p_adv_m, width = 7, height = 12, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_relative_advantage_mobile.png\n")

# Relative advantage Excel
adv_excel <- adv_top20[order(-imports_bn), .(
  `Import Origin` = geography,
  `ISO Code` = iso_code,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Own Rate Pre-ruling (%)` = round(own_before, 2),
  `Own Rate S122@10% (%)` = round(own_s122, 2),
  `Advantage Pre-ruling (pp)` = round(adv_before, 2),
  `Advantage S122@10% (pp)` = round(adv_s122, 2),
  `Advantage S122@15% (pp)` = round(adv_s122_15, 2),
  `Change in Advantage (pp)` = round(adv_s122_15 - adv_before, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "Relative Advantage")
writeDataTable(wb, 1, adv_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(adv_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_relative_advantage.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_relative_advantage.xlsx\n")


# =============================================================================
# CHART D: AGGREGATE COMPOSITION (4 stacked bars)
# =============================================================================

cat("\n5. Creating aggregate composition chart (4 scenarios)...\n")

# Build 4-row data.table for stacked bar chart
agg_data <- data.table(
  geography  = c("S122 15%", "S122 10%", "Post-ruling", "Pre-ruling"),
  iso_code   = rep("WLD", 4),
  hts_layer              = c(s122_15_global$hts_layer, s122_global$hts_layer,
                             ieepa_global$hts_layer, baseline_global$hts_layer),
  ieepa_baseline_layer   = c(s122_15_global$ieepa_baseline_layer, s122_global$ieepa_baseline_layer,
                             ieepa_global$ieepa_baseline_layer, baseline_global$ieepa_baseline_layer),
  ieepa_topup_layer      = c(s122_15_global$ieepa_topup_layer, s122_global$ieepa_topup_layer,
                             ieepa_global$ieepa_topup_layer, baseline_global$ieepa_topup_layer),
  s232_layer             = c(s122_15_global$s232_layer, s122_global$s232_layer,
                             ieepa_global$s232_layer, baseline_global$s232_layer),
  emergency_layer        = c(s122_15_global$emergency_layer, s122_global$emergency_layer,
                             ieepa_global$emergency_layer, baseline_global$emergency_layer),
  s301_layer             = c(s122_15_global$s301_layer, s122_global$s301_layer,
                             ieepa_global$s301_layer, baseline_global$s301_layer),
  s122_layer             = c(s122_15_global$s122_layer, s122_global$s122_layer,
                             ieepa_global$s122_layer, baseline_global$s122_layer),
  total_rate             = c(s122_15_global$total_rate, s122_global$total_rate,
                             ieepa_global$total_rate, baseline_global$total_rate)
)

agg_data[, display_label := sprintf("%s (%.1f%%)", geography, total_rate)]
agg_data[, display_label := factor(display_label, levels = display_label)]

agg_long <- prepare_composition_long_s122(agg_data)

p_agg <- ggplot(agg_long, aes(x = display_label, y = rate, fill = layer)) +
  geom_bar(stat = "identity", width = 0.55) +
  geom_text(aes(y = label_pos, label = rate_label),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = LAYER_COLORS_S122, name = NULL,
                    breaks = rev(LAYER_LABELS_S122),
                    drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(x = NULL, y = "Effective Tariff Rate (%)") +
  theme_gta_inner +
  theme(
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

p_agg_wrapped <- gta_wrap(
  p_agg,
  title    = sprintf("Four scenarios: US tariffs range from %.1f%% to %.1f%%",
                     ieepa_global$total_rate, baseline_global$total_rate),
  subtitle = "Trade-weighted average tariff rate on all US imports, by tariff instrument"
)

ggsave(file.path(CHART_DIR, "s122_actual_aggregate_comparison.png"),
       plot = p_agg_wrapped, width = 12, height = 7, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_actual_aggregate_comparison.png\n")

# --- Mobile variant (same 4 bars, portrait) ---
agg_data_m <- data.table(
  geography  = c("Pre-\nruling", "Post-\nruling", "S122\n10%", "S122\n15%"),
  iso_code   = rep("WLD", 4),
  hts_layer              = c(baseline_global$hts_layer, ieepa_global$hts_layer,
                             s122_global$hts_layer, s122_15_global$hts_layer),
  ieepa_baseline_layer   = c(baseline_global$ieepa_baseline_layer, ieepa_global$ieepa_baseline_layer,
                             s122_global$ieepa_baseline_layer, s122_15_global$ieepa_baseline_layer),
  ieepa_topup_layer      = c(baseline_global$ieepa_topup_layer, ieepa_global$ieepa_topup_layer,
                             s122_global$ieepa_topup_layer, s122_15_global$ieepa_topup_layer),
  s232_layer             = c(baseline_global$s232_layer, ieepa_global$s232_layer,
                             s122_global$s232_layer, s122_15_global$s232_layer),
  emergency_layer        = c(baseline_global$emergency_layer, ieepa_global$emergency_layer,
                             s122_global$emergency_layer, s122_15_global$emergency_layer),
  s301_layer             = c(baseline_global$s301_layer, ieepa_global$s301_layer,
                             s122_global$s301_layer, s122_15_global$s301_layer),
  s122_layer             = c(baseline_global$s122_layer, ieepa_global$s122_layer,
                             s122_global$s122_layer, s122_15_global$s122_layer),
  total_rate             = c(baseline_global$total_rate, ieepa_global$total_rate,
                             s122_global$total_rate, s122_15_global$total_rate)
)

agg_data_m[, display_label := sprintf("%s\n(%.1f%%)", geography, total_rate)]
agg_data_m[, display_label := factor(display_label, levels = display_label)]

agg_long_m <- prepare_composition_long_s122(agg_data_m)

p_agg_m <- ggplot(agg_long_m, aes(x = display_label, y = rate, fill = layer)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = label_pos, label = rate_label),
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = LAYER_COLORS_S122, name = NULL, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Effective Tariff Rate (%)") +
  theme_gta_inner +
  theme(
    axis.text.x = element_text(lineheight = 0.9),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2))

p_agg_m_wrapped <- gta_wrap(p_agg_m,
  title    = sprintf("Four scenarios: US tariffs\nrange from %.1f%% to %.1f%%",
                     ieepa_global$total_rate, baseline_global$total_rate),
  subtitle = "Trade-weighted average, by tariff instrument",
  chart_top = 0.82)

ggsave(file.path(CHART_DIR, "s122_actual_aggregate_comparison_mobile.png"),
       plot = p_agg_m_wrapped, width = 6, height = 9, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_actual_aggregate_comparison_mobile.png\n")

# Aggregate Excel
agg_excel <- data.table(
  Scenario = c("Before SCOTUS", "IEEPA Strike Down", "S122 @ 10%", "S122 @ 15%"),
  `Total Rate (%)` = round(c(baseline_global$total_rate, ieepa_global$total_rate,
                              s122_global$total_rate, s122_15_global$total_rate), 2),
  `HTS/MFN (%)` = round(c(baseline_global$hts_layer, ieepa_global$hts_layer,
                            s122_global$hts_layer, s122_15_global$hts_layer), 2),
  `S232 (%)` = round(c(baseline_global$s232_layer, ieepa_global$s232_layer,
                        s122_global$s232_layer, s122_15_global$s232_layer), 2),
  `S301 (%)` = round(c(baseline_global$s301_layer, ieepa_global$s301_layer,
                        s122_global$s301_layer, s122_15_global$s301_layer), 2),
  `S122 Surcharge (%)` = round(c(baseline_global$s122_layer, ieepa_global$s122_layer,
                                  s122_global$s122_layer, s122_15_global$s122_layer), 2)
)
wb <- createWorkbook()
addWorksheet(wb, "Aggregate Comparison")
writeDataTable(wb, 1, agg_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(agg_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_aggregate_comparison.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_aggregate_comparison.xlsx\n")


# =============================================================================
# CHART E: S122 CONTRIBUTION DECOMPOSITION (2-layer stacked bars, S122@15%)
# =============================================================================

cat("\n6. Creating S122 contribution decomposition chart (S122@15%)...\n")

# Country aggregation for IEEPA (base rate) and S122@15% (total)
ieepa_simple <- aggregate_country_simple(ieepa_data)
ieepa_simple_slim <- ieepa_simple[, .(iso_code, base_rate = total_rate)]

# Use S122@15% country rates for the decomposition
contrib_merged <- merge(
  s122_15_country[, .(geography, iso_code, total_rate = total_rate, imports_bn = total_imports_bn)],
  ieepa_simple_slim,
  by = "iso_code", all.x = TRUE)
contrib_merged[, s122_increment := total_rate - base_rate]
contrib_merged[, s122_increment := pmax(0, s122_increment)]
contrib_top20 <- contrib_merged[order(-imports_bn)][1:20]

# Also get S122@10% total for Excel companion
s122_10_simple <- aggregate_country_simple(s122_data)
s122_10_slim <- s122_10_simple[, .(iso_code, rate_s122_10 = total_rate)]
contrib_top20 <- merge(contrib_top20, s122_10_slim, by = "iso_code", all.x = TRUE)

# Order by total rate
contrib_top20 <- contrib_top20[order(total_rate)]
contrib_top20[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
contrib_top20[, display_label := factor(display_label, levels = display_label)]

# Two-component stacked bar
contrib_long <- melt(contrib_top20,
  id.vars = c("geography", "iso_code", "display_label", "total_rate"),
  measure.vars = c("base_rate", "s122_increment"),
  variable.name = "component", value.name = "value")

component_levels <- c("s122_increment", "base_rate")
component_colors <- c("base_rate" = GTA_NAVY, "s122_increment" = "#2CA58D")
component_labels <- c("base_rate" = "Post-ruling base", "s122_increment" = "S122 surcharge (15%)")
contrib_long[, component := factor(component, levels = component_levels)]

# Midpoints for labels
contrib_top20[, mid_base := base_rate / 2]
contrib_top20[, mid_s122 := base_rate + s122_increment / 2]

MIN_WIDTH <- 1.2

p_contrib <- ggplot(contrib_long, aes(x = display_label, y = value, fill = component)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(data = contrib_top20[base_rate >= MIN_WIDTH],
    aes(x = display_label, y = mid_base, fill = NULL,
        label = sprintf("%.1f", base_rate)),
    size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = contrib_top20[s122_increment >= MIN_WIDTH],
    aes(x = display_label, y = mid_s122, fill = NULL,
        label = sprintf("%.1f", s122_increment)),
    size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = contrib_top20,
    aes(x = display_label, y = total_rate, fill = NULL,
        label = sprintf("%.1f%%", total_rate)),
    hjust = -0.12, size = 4.5, fontface = "bold", color = "grey30") +
  scale_fill_manual(values = component_colors, labels = component_labels, name = NULL,
                    breaks = c("base_rate", "s122_increment")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  coord_flip() +
  labs(x = NULL, y = "Trade-weighted tariff rate (%)") +
  theme_gta_inner +
  theme(legend.position = "bottom")

p_contrib_wrapped <- gta_wrap(
  p_contrib,
  title    = "How the S122 surcharge (15%) builds on the post-ruling base",
  subtitle = "Top 20 by 2024 US import value."
)

ggsave(file.path(CHART_DIR, "s122_actual_contribution.png"),
       plot = p_contrib_wrapped, width = 12, height = 12, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_actual_contribution.png\n")

# --- Mobile variant: top 10 ---
contrib_top10 <- contrib_merged[order(-imports_bn)][1:10]
contrib_top10 <- merge(contrib_top10, s122_10_slim, by = "iso_code", all.x = TRUE)
contrib_top10 <- contrib_top10[order(total_rate)]
contrib_top10[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
contrib_top10[, display_label := factor(display_label, levels = display_label)]

contrib_long_m <- melt(contrib_top10,
  id.vars = c("geography", "iso_code", "display_label", "total_rate"),
  measure.vars = c("base_rate", "s122_increment"),
  variable.name = "component", value.name = "value")
contrib_long_m[, component := factor(component, levels = component_levels)]

contrib_top10[, mid_base := base_rate / 2]
contrib_top10[, mid_s122 := base_rate + s122_increment / 2]

p_contrib_m <- ggplot(contrib_long_m, aes(x = display_label, y = value, fill = component)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(data = contrib_top10[base_rate >= MIN_WIDTH],
    aes(x = display_label, y = mid_base, fill = NULL,
        label = sprintf("%.1f", base_rate)),
    size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = contrib_top10[s122_increment >= MIN_WIDTH],
    aes(x = display_label, y = mid_s122, fill = NULL,
        label = sprintf("%.1f", s122_increment)),
    size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = contrib_top10,
    aes(x = display_label, y = total_rate, fill = NULL,
        label = sprintf("%.1f%%", total_rate)),
    hjust = -0.12, size = 4.5, fontface = "bold", color = "grey30") +
  scale_fill_manual(values = component_colors, labels = component_labels, name = NULL,
                    breaks = c("base_rate", "s122_increment")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  coord_flip() +
  labs(x = NULL, y = "Trade-weighted tariff rate (%)") +
  theme_gta_inner +
  theme(legend.position = "bottom")

p_contrib_m_wrapped <- gta_wrap(p_contrib_m,
  title    = "S122 surcharge (15%) on the post-ruling base",
  subtitle = "Top 10 by import value")

ggsave(file.path(CHART_DIR, "s122_actual_contribution_mobile.png"),
       plot = p_contrib_m_wrapped, width = 7, height = 10, dpi = 300, bg = GTA_BG)
cat("   Saved: s122_actual_contribution_mobile.png\n")

# Contribution Excel — includes both S122@10% and S122@15%
contrib_excel <- contrib_top20[order(-imports_bn), .(
  `Import Origin` = geography,
  `ISO Code` = iso_code,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Post-Ruling Base Rate (%)` = round(base_rate, 2),
  `S122@10% Total (%)` = round(rate_s122_10, 2),
  `S122@10% Increment (pp)` = round(rate_s122_10 - base_rate, 2),
  `S122@15% Total (%)` = round(total_rate, 2),
  `S122@15% Increment (pp)` = round(s122_increment, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "S122 Contribution")
writeDataTable(wb, 1, contrib_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(contrib_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_contribution.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_contribution.xlsx\n")


# =============================================================================
# CHART F: TOP IMPORTERS REDUCTION (Before SCOTUS → S122@15%)
# =============================================================================

cat("\n7. Creating top importers reduction chart (Before → S122@15%)...\n")

importers_merged <- merge(
  baseline_country[geography != "Global", .(geography, iso_code, imports_bn = total_imports_bn, rate_before = total_rate)],
  s122_15_country[geography != "Global", .(iso_code, rate_s122_15 = total_rate)],
  by = "iso_code"
)
# Also get S122@10% for Excel
importers_merged <- merge(importers_merged,
  s122_country[geography != "Global", .(iso_code, rate_s122 = total_rate)],
  by = "iso_code", all.x = TRUE)

# Deduplicate by iso_code (e.g. PSE appears as both Gaza Strip and Palestinian Territory)
importers_merged <- importers_merged[!duplicated(iso_code)]

importers_merged[, reduction_pp := rate_before - rate_s122_15]
top20_importers <- importers_merged[order(-imports_bn)][1:20]

# Order ascending so largest reduction at top after coord_flip
top20_importers <- top20_importers[order(reduction_pp)]
top20_importers[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
top20_importers[, display_label := factor(display_label, levels = display_label)]

top20_importers[, bar_label := ifelse(reduction_pp >= 0,
  sprintf("-%.1f pp", reduction_pp),
  sprintf("+%.1f pp", abs(reduction_pp)))]
top20_importers[, label_hjust := ifelse(reduction_pp >= 0, -0.15, 1.15)]

p_imp <- ggplot(top20_importers, aes(x = display_label, y = reduction_pp)) +
  geom_bar(stat = "identity", width = 0.65, fill = GTA_NAVY) +
  geom_text(
    aes(label = bar_label, hjust = label_hjust),
    size = 4.5, fontface = "bold", color = "grey30"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    axis.text.x  = element_blank(),
    legend.position = "none"
  )

p_imp_wrapped <- gta_wrap(
  p_imp,
  title    = "Tariff cuts for top 20 US import sources (Before → S122 15%)",
  subtitle = "Reduction in trade-weighted average tariff rate (pp). Brackets: 2024 imports ($bn)."
)

ggsave(file.path(CHART_DIR, "s122_actual_top_importers_reduction.png"),
       plot = p_imp_wrapped, width = 12, height = 12, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_top_importers_reduction.png\n")

# --- Mobile variant: top 10 ---
top10_importers <- importers_merged[order(-imports_bn)][1:10]
top10_importers <- top10_importers[order(reduction_pp)]
top10_importers[, display_label := ifelse(
  geography == "European Union", geography,
  sprintf("%s ($%.0fbn)", iso_code, imports_bn)
)]
top10_importers[, display_label := factor(display_label, levels = display_label)]

top10_importers[, bar_label := ifelse(reduction_pp >= 0,
  sprintf("-%.1f pp", reduction_pp),
  sprintf("+%.1f pp", abs(reduction_pp)))]
top10_importers[, label_hjust := ifelse(reduction_pp >= 0, -0.15, 1.15)]

p_imp_m <- ggplot(top10_importers, aes(x = display_label, y = reduction_pp)) +
  geom_bar(stat = "identity", width = 0.65, fill = GTA_NAVY) +
  geom_text(
    aes(label = bar_label, hjust = label_hjust),
    size = 4.5, fontface = "bold", color = "grey30"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    axis.text.x  = element_blank(),
    legend.position = "none"
  )

p_imp_m_wrapped <- gta_wrap(p_imp_m,
  title    = "Tariff cuts for top US import sources",
  subtitle = "Reduction in tariff rate (pp). Brackets: 2024 imports ($bn).")

ggsave(file.path(CHART_DIR, "s122_actual_top_importers_reduction_mobile.png"),
       plot = p_imp_m_wrapped, width = 7, height = 10, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_top_importers_reduction_mobile.png\n")

# Top importers Excel — includes both S122@10% and S122@15%
imp_excel <- top20_importers[order(-reduction_pp), .(
  `Import Origin` = geography,
  `ISO Code` = iso_code,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Before SCOTUS (%)` = round(rate_before, 2),
  `S122@10% (%)` = round(rate_s122, 2),
  `S122@15% (%)` = round(rate_s122_15, 2),
  `Reduction to S122@10% (pp)` = round(rate_before - rate_s122, 2),
  `Reduction to S122@15% (pp)` = round(reduction_pp, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "Top Importers Reduction")
writeDataTable(wb, 1, imp_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(imp_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_top_importers_reduction.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_top_importers_reduction.xlsx\n")


# =============================================================================
# CHART G: TOP WINNERS (bar chart, by largest tariff reduction Before → S122@15%)
# =============================================================================

cat("\n8. Creating top winners chart (Before → S122@15%)...\n")

top_winners <- importers_merged[iso_code != "EU"][order(-reduction_pp)][1:20]
top_winners <- top_winners[order(reduction_pp)]
top_winners[, display_label := sprintf("%s ($%.1fbn)", iso_code, imports_bn)]
top_winners[, display_label := factor(display_label, levels = display_label)]

top_winners[, bar_label := sprintf("-%.1f pp", reduction_pp)]

p_win <- ggplot(top_winners, aes(x = display_label, y = reduction_pp)) +
  geom_bar(stat = "identity", width = 0.65, fill = GTA_NAVY) +
  geom_text(
    aes(label = bar_label),
    hjust = -0.15, size = 4.5, fontface = "bold", color = "grey30"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    axis.text.x  = element_blank(),
    legend.position = "none"
  )

p_win_wrapped <- gta_wrap(
  p_win,
  title    = "Largest tariff cuts: Before SCOTUS to S122 15%",
  subtitle = "Reduction in trade-weighted tariff rate (pp). Brackets: 2024 imports ($bn)."
)

ggsave(file.path(CHART_DIR, "s122_actual_top_winners.png"),
       plot = p_win_wrapped, width = 12, height = 12, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_top_winners.png\n")

# --- Mobile variant: top 10 ---
top10_winners <- importers_merged[iso_code != "EU"][order(-reduction_pp)][1:10]
top10_winners <- top10_winners[order(reduction_pp)]
top10_winners[, display_label := sprintf("%s ($%.1fbn)", iso_code, imports_bn)]
top10_winners[, display_label := factor(display_label, levels = display_label)]

top10_winners[, bar_label := sprintf("-%.1f pp", reduction_pp)]

p_win_m <- ggplot(top10_winners, aes(x = display_label, y = reduction_pp)) +
  geom_bar(stat = "identity", width = 0.65, fill = GTA_NAVY) +
  geom_text(
    aes(label = bar_label),
    hjust = -0.15, size = 4.5, fontface = "bold", color = "grey30"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_gta_inner +
  theme(
    axis.text.x  = element_blank(),
    legend.position = "none"
  )

p_win_m_wrapped <- gta_wrap(p_win_m,
  title    = "Largest tariff cuts: Before to S122 15%",
  subtitle = "Reduction in tariff rate (pp). Brackets: 2024 imports ($bn).")

ggsave(file.path(CHART_DIR, "s122_actual_top_winners_mobile.png"),
       plot = p_win_m_wrapped, width = 7, height = 10, dpi = 200, bg = GTA_BG)
cat("   Saved: s122_actual_top_winners_mobile.png\n")

# Top winners Excel — includes both S122@10% and S122@15%
win_excel <- top_winners[order(-reduction_pp), .(
  `Import Origin` = geography,
  `ISO Code` = iso_code,
  `US Imports ($bn)` = round(imports_bn, 1),
  `Before SCOTUS (%)` = round(rate_before, 2),
  `S122@10% (%)` = round(rate_s122, 2),
  `S122@15% (%)` = round(rate_s122_15, 2),
  `Reduction to S122@10% (pp)` = round(rate_before - rate_s122, 2),
  `Reduction to S122@15% (pp)` = round(reduction_pp, 2)
)]
wb <- createWorkbook()
addWorksheet(wb, "Top Winners")
writeDataTable(wb, 1, win_excel, tableStyle = "TableStyleMedium2")
setColWidths(wb, 1, cols = 1:ncol(win_excel), widths = "auto")
saveWorkbook(wb, file.path(CHART_DIR, "s122_actual_top_winners.xlsx"), overwrite = TRUE)
cat("   Saved: s122_actual_top_winners.xlsx\n")


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== ENHANCEMENT COMPLETE ===\n\n")

# List all chart+excel pairs
chart_files <- list.files(CHART_DIR, pattern = "\\.png$", full.names = FALSE)
excel_files <- list.files(CHART_DIR, pattern = "\\.xlsx$", full.names = FALSE)

cat("Charts:\n")
for (f in sort(chart_files)) cat(sprintf("  %s\n", f))
cat("\nExcel companions:\n")
for (f in sort(excel_files)) cat(sprintf("  %s\n", f))

# Verify every chart has an Excel companion (excluding mobile variants)
base_charts <- gsub("_mobile\\.png$", ".png", chart_files)
base_charts <- unique(gsub("\\.png$", "", base_charts))
base_excels <- gsub("\\.xlsx$", "", excel_files)

missing <- setdiff(base_charts, base_excels)
if (length(missing) > 0) {
  cat("\nWARNING: Charts without Excel companions:\n")
  for (m in missing) cat(sprintf("  %s\n", m))
} else {
  cat("\nAll charts have matching Excel companions.\n")
}

# Key numbers
cat(sprintf("\nKey numbers:\n"))
cat(sprintf("  Before SCOTUS:       %.2f%%\n", baseline_global$total_rate))
cat(sprintf("  IEEPA Strike Down:   %.2f%%\n", ieepa_global$total_rate))
cat(sprintf("  S122 @ 10%%:          %.2f%%\n", s122_global$total_rate))
cat(sprintf("  S122 @ 15%%:          %.2f%%\n", s122_15_global$total_rate))

cat(sprintf("\nRelative advantage highlights:\n"))
cat(sprintf("  China: %.1f pp (before) -> %.1f pp (S122@10%%) -> %.1f pp (S122@15%%)\n",
            adv_top20[geography == "China"]$adv_before,
            adv_top20[geography == "China"]$adv_s122,
            adv_top20[geography == "China"]$adv_s122_15))
cat(sprintf("  India: %.1f pp (before) -> %.1f pp (S122@10%%) -> %.1f pp (S122@15%%)\n",
            adv_top20[geography == "India"]$adv_before,
            adv_top20[geography == "India"]$adv_s122,
            adv_top20[geography == "India"]$adv_s122_15))
