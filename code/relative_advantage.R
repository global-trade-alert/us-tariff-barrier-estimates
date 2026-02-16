# =============================================================================
# US Tariff Analysis - Relative Tariff Advantage Analysis
# Calculates competitive advantage/disadvantage for each country-product combination
# =============================================================================

# Load necessary libraries
library(data.table)
library(dplyr)
library(writexl)
library(ggplot2)
library(scales)
library(extrafont)
library(stringr)

# Clear environment
# rm(list = ls())  # Commented out - environment cleared by update_processing.R
options(scipen = 999)

# Output directory (set by update_processing.R, or standalone default)
if (!exists("RUN_OUTPUT_DIR")) {
  source("code/date_config.R")
  RUN_OUTPUT_DIR <- "results/standalone"
}

# Create output subdirectories
dir.create(file.path(RUN_OUTPUT_DIR, "analysis"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RUN_OUTPUT_DIR, "analysis", "charts"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RUN_OUTPUT_DIR, "countries"), recursive = TRUE, showWarnings = FALSE)

# Scenario description for chart subtitles
if (exists("SCENARIO_DESCRIPTION") && !is.null(SCENARIO_DESCRIPTION)) {
  scenario_desc <- SCENARIO_DESCRIPTION
  is_scenario <- TRUE
} else {
  scenario_desc <- NULL
  is_scenario <- FALSE
}

# Affected countries for dual-bar display
if (exists("SCENARIO_AFFECTED_COUNTRIES") && !is.null(SCENARIO_AFFECTED_COUNTRIES)) {
  affected_un_codes <- as.integer(SCENARIO_AFFECTED_COUNTRIES)
} else {
  affected_un_codes <- NULL
}

cat("=== US Tariff Analysis - Relative Tariff Advantage ===\n")
cat("Loading processed data and calculating competitive positions...\n\n")

# =============================================================================
# 1. LOAD PROCESSED DATA
# =============================================================================

cat("1. Loading processed tariff data...\n")

load(file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData")))

# Ensure us_imports is a data.table (may lose class when loaded from RData)
if (!is.data.table(us_imports)) {
  setDT(us_imports)
}

cat(sprintf("   Loaded %s import flows\n", format(nrow(us_imports), big.mark = ",")))
cat(sprintf("   Total import value: $%.1f billion\n", sum(us_imports$us_imports_bn)))
cat(sprintf("   Unique exporters: %d\n", length(unique(us_imports$exporter))))
cat(sprintf("   Unique HS codes: %d\n", length(unique(us_imports$hs_8digit))))

# =============================================================================
# 2. CALCULATE RELATIVE TARIFF ADVANTAGE/DISADVANTAGE
# =============================================================================

cat("\n2. Calculating relative tariff advantage...\n")

# Calculate effective rates
us_imports[, effective_rate := rate]

# Calculate total imports and tariff-weighted totals by HS code
hs_totals <- us_imports[, .(
  total_imports_by_hs = sum(us_imports_bn),
  total_tariff_weighted_by_hs = sum(effective_rate * us_imports_bn)
), by = hs_8digit]

cat(sprintf("   Calculated totals for %d unique HS codes\n", nrow(hs_totals)))

# Merge back to main data
us_imports <- merge(us_imports, hs_totals, by = "hs_8digit", all.x = TRUE)

# Calculate competitor weighted average rate (excluding own country)
# Handle cases where only one country imports a product (set competitor rate to NA)
us_imports[, competitor_rate := ifelse(
  total_imports_by_hs - us_imports_bn > 0,
  (total_tariff_weighted_by_hs - effective_rate * us_imports_bn) / (total_imports_by_hs - us_imports_bn),
  NA_real_
)]

# Calculate tariff advantage (negative = disadvantage, positive = advantage)
us_imports[, tariff_advantage := competitor_rate - effective_rate]

# Count products with competition
products_with_competition <- sum(!is.na(us_imports$competitor_rate))
products_single_supplier <- sum(is.na(us_imports$competitor_rate))

cat(sprintf("   Products with multiple suppliers: %s\n", format(products_with_competition, big.mark = ",")))
cat(sprintf("   Products with single supplier: %s\n", format(products_single_supplier, big.mark = ",")))

# =============================================================================
# 3. CREATE ANALYSIS DATASETS
# =============================================================================

cat("\n3. Creating relative tariff advantage datasets...\n")

# Create main relative advantage dataset
relative_advantage <- us_imports[!is.na(competitor_rate), .(
  exporter = exporter,
  hs_8digit = hs_8digit,
  hs_8digit_name = hs_8digit_name,
  own_rate = round(effective_rate, 2),
  own_rate_original = rate,  # Original rate for comparison
  competitor_rate = round(competitor_rate, 2),
  tariff_advantage = round(tariff_advantage, 2),
  import_value_bn = us_imports_bn,
  iso2 = iso2,
  iso_code = iso_code,
  un_code = un_code
)][order(-import_value_bn)]

cat(sprintf("   Relative advantage dataset contains %s country-product combinations\n", 
            format(nrow(relative_advantage), big.mark = ",")))

# Create country-level summary
country_advantage_summary <- relative_advantage[, .(
  total_import_value_bn = sum(import_value_bn),
  flow_count = .N,
  avg_own_rate = round(weighted.mean(own_rate, import_value_bn), 2),
  avg_own_rate_original = round(weighted.mean(own_rate_original, import_value_bn), 2),  # Original for comparison
  avg_competitor_rate = round(weighted.mean(competitor_rate, import_value_bn), 2),
  avg_tariff_advantage = round(weighted.mean(tariff_advantage, import_value_bn), 2),
  flows_with_advantage = sum(tariff_advantage > 0),
  flows_with_disadvantage = sum(tariff_advantage < 0),
  advantage_share = round(100 * sum(tariff_advantage > 0) / .N, 1)
), by = .(exporter, iso2, iso_code, un_code)][order(-total_import_value_bn)]

cat(sprintf("   Country summary contains %d countries\n", nrow(country_advantage_summary)))

# =============================================================================
# 4. SUMMARY STATISTICS
# =============================================================================

cat("\n4. Calculating summary statistics...\n")

# Overall statistics
advantage_stats <- relative_advantage[, .(
  flows_with_advantage = sum(tariff_advantage > 0),
  flows_with_disadvantage = sum(tariff_advantage < 0),
  flows_neutral = sum(tariff_advantage == 0),
  avg_advantage = round(mean(tariff_advantage), 2),
  weighted_avg_advantage = round(weighted.mean(tariff_advantage, import_value_bn), 2),
  median_advantage = round(median(tariff_advantage), 2),
  max_advantage = round(max(tariff_advantage), 2),
  max_disadvantage = round(min(tariff_advantage), 2)
)]

cat("\n=== TARIFF ADVANTAGE SUMMARY ===\n")
cat(sprintf("Total flows analyzed: %s\n", format(nrow(relative_advantage), big.mark = ",")))
cat(sprintf("Flows with tariff advantage (< competitors): %s (%.1f%%)\n", 
            format(advantage_stats$flows_with_advantage, big.mark = ","),
            100 * advantage_stats$flows_with_advantage / nrow(relative_advantage)))
cat(sprintf("Flows with tariff disadvantage (> competitors): %s (%.1f%%)\n", 
            format(advantage_stats$flows_with_disadvantage, big.mark = ","),
            100 * advantage_stats$flows_with_disadvantage / nrow(relative_advantage)))
cat(sprintf("Flows with neutral tariffs (= competitors): %s (%.1f%%)\n", 
            format(advantage_stats$flows_neutral, big.mark = ","),
            100 * advantage_stats$flows_neutral / nrow(relative_advantage)))
cat(sprintf("Average tariff advantage: %.2f%%\n", advantage_stats$avg_advantage))
cat(sprintf("Trade-weighted average advantage: %.2f%%\n", advantage_stats$weighted_avg_advantage))
cat(sprintf("Maximum advantage: %.2f%% | Maximum disadvantage: %.2f%%\n", 
            advantage_stats$max_advantage, advantage_stats$max_disadvantage))

# Top countries with advantages and disadvantages
cat("\nTop 10 countries by trade-weighted average advantage:\n")
print(head(country_advantage_summary[order(-avg_tariff_advantage)], 10))

cat("\nTop 10 countries by trade-weighted average disadvantage:\n")
print(head(country_advantage_summary[order(avg_tariff_advantage)], 10))

# =============================================================================
# 5. EXPORT RESULTS
# =============================================================================

cat("\n5. Exporting relative advantage analysis...\n")

# Save as RData
save(relative_advantage, country_advantage_summary,
     file = file.path(RUN_OUTPUT_DIR, "analysis", "relative_tariff_advantage.RData"))
cat(sprintf("Saved relative advantage data to: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "relative_tariff_advantage.RData")))

# Export to Excel
write_xlsx(
  list(
    "Relative Advantage by Flow" = relative_advantage %>%
      select(exporter, hs_8digit, hs_8digit_name, own_rate, own_rate_original, competitor_rate,
             tariff_advantage, import_value_bn) %>%
      rename(
        "Exporter" = exporter,
        "HS 8-Digit Code" = hs_8digit,
        "Product Description" = hs_8digit_name,
        "Own Rate (%)" = own_rate,
        "Own Rate Original (%)" = own_rate_original,
        "Competitor Avg Rate (%)" = competitor_rate,
        "Tariff Advantage (%)" = tariff_advantage,
        "Import Value (bn USD)" = import_value_bn
      ),
    "Country Summary" = country_advantage_summary %>%
      rename(
        "Exporter" = exporter,
        "ISO2 Code" = iso2,
        "ISO Code" = iso_code,
        "UN Code" = un_code,
        "Total Import Value (bn USD)" = total_import_value_bn,
        "Number of Products" = flow_count,
        "Avg Own Rate (%)" = avg_own_rate,
        "Avg Own Rate Original (%)" = avg_own_rate_original,
        "Avg Competitor Rate (%)" = avg_competitor_rate,
        "Avg Tariff Advantage (%)" = avg_tariff_advantage,
        "Products with Advantage" = flows_with_advantage,
        "Products with Disadvantage" = flows_with_disadvantage,
        "Advantage Share (%)" = advantage_share
      )
  ),
  path = file.path(RUN_OUTPUT_DIR, "analysis", "relative_tariff_advantage.xlsx")
)

cat(sprintf("Saved analysis to: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "relative_tariff_advantage.xlsx")))

# =============================================================================
# 6. VISUALIZATION - TOP 20 IMPORT ORIGINS CHARTS
# =============================================================================

cat("\n6. Creating visualizations for top 20 import origins...\n")

# Load baseline data for comparison if running scenario
baseline_country_summary <- NULL
if (is_scenario && file.exists("results/relative_tariff_advantage.RData")) {
  baseline_env <- new.env()
  load("results/relative_tariff_advantage.RData", envir = baseline_env)
  baseline_country_summary <- baseline_env$country_advantage_summary
  cat("   Loaded baseline data for comparison charts\n")
}

# Get top 20 countries by import value for charts
top20_countries <- head(country_advantage_summary, 20)

# Chart 1: Average Applied Rate - Highest at Top
# Build subtitle with scenario info
if (is_scenario && !is.null(scenario_desc)) {
  chart1_subtitle <- sprintf("Trade-weighted average tariff by country\nBaseline effective %s\nScenario: %s",
                              POLICY_DATE_LONG, scenario_desc)
} else {
  chart1_subtitle <- sprintf("Trade-weighted average tariff by country,\neffective %s", POLICY_DATE_LONG)
}

# Prepare chart data - add baseline values for affected countries if in scenario mode
chart1_data <- top20_countries %>%
  arrange(avg_own_rate) %>%
  mutate(exporter_ordered = factor(exporter, levels = exporter))

# Check if we need dual bars (scenario mode with baseline data and affected countries)
use_dual_bars_chart1 <- is_scenario && !is.null(baseline_country_summary) && !is.null(affected_un_codes)

if (use_dual_bars_chart1) {
  # Merge baseline rates for affected countries
  chart1_data <- chart1_data %>%
    left_join(
      baseline_country_summary %>% select(un_code, baseline_rate = avg_own_rate),
      by = "un_code"
    ) %>%
    mutate(
      is_affected = un_code %in% affected_un_codes,
      baseline_rate = ifelse(is_affected, baseline_rate, NA_real_)
    )

  # Create long format for dual bars
  chart1_long <- chart1_data %>%
    select(exporter, exporter_ordered, un_code, is_affected, scenario_rate = avg_own_rate, baseline_rate) %>%
    tidyr::pivot_longer(
      cols = c(scenario_rate, baseline_rate),
      names_to = "rate_type",
      values_to = "rate"
    ) %>%
    filter(!is.na(rate)) %>%
    mutate(
      rate_type = factor(rate_type, levels = c("baseline_rate", "scenario_rate")),
      bar_label = sprintf("%.1f%%", rate)
    )

  p1 <- ggplot(chart1_long, aes(x = exporter_ordered, y = rate, fill = rate_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = bar_label),
              position = position_dodge(width = 0.8),
              hjust = -0.1,
              color = "#333333",
              family = "sans",
              size = 5,
              fontface = "bold") +
    scale_fill_manual(
      values = c("baseline_rate" = "#AAAAAA", "scenario_rate" = "#2171C2"),
      labels = c("baseline_rate" = "Baseline", "scenario_rate" = "Scenario"),
      name = ""
    ) +
    labs(
      title = "The New US Tariff Environment",
      subtitle = chart1_subtitle,
      x = "",
      y = "",
      caption = "Gray = Baseline, Blue = Scenario\nSource: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 32, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 16, color = "#333333", face = "bold"),
      plot.margin = margin(t = 20, r = 50, b = 20, l = 20),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 14)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_flip()
} else {
  # Original single-bar chart
  p1 <- ggplot(chart1_data, aes(x = exporter_ordered, y = avg_own_rate)) +
    geom_bar(stat = "identity", fill = "#2171C2", width = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", avg_own_rate)),
              hjust = -0.1,
              color = "#333333",
              family = "sans",
              size = 6,
              fontface = "bold") +
    labs(
      title = "The New US Tariff Environment",
      subtitle = chart1_subtitle,
      x = "",
      y = "",
      caption = "Source: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 32, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 20, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 16, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 16, color = "#333333", face = "bold"),
      plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_flip()
}

# Save Chart 1 - Portrait for LinkedIn
ggsave(file.path(RUN_OUTPUT_DIR, "analysis", "charts", "effective_rates_top20.png"), plot = p1, width = 10, height = 12,
       dpi = 300, bg = "white", units = "in", limitsize = FALSE)

cat(sprintf("   Saved effective rates chart: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "charts", "effective_rates_top20.png")))

# Chart 2: Relative Tariff Advantage - Center Axis Chart
# Build subtitle with scenario info
if (is_scenario && !is.null(scenario_desc)) {
  chart2_subtitle <- sprintf("Trade-weighted average own tariff vs. competitors\nBaseline effective %s\nScenario: %s",
                              POLICY_DATE_LONG, scenario_desc)
} else {
  chart2_subtitle <- sprintf("Trade-weighted average own tariff vs. competitors,\neffective %s", POLICY_DATE_LONG)
}

chart2_data <- top20_countries %>%
  arrange(avg_tariff_advantage) %>%  # Keep ascending - most disadvantaged at bottom
  mutate(exporter_ordered = factor(exporter, levels = exporter))

# Check if we need dual bars (scenario mode with baseline data and affected countries)
use_dual_bars_chart2 <- is_scenario && !is.null(baseline_country_summary) && !is.null(affected_un_codes)

if (use_dual_bars_chart2) {
  # Merge baseline advantage for affected countries
  chart2_data <- chart2_data %>%
    left_join(
      baseline_country_summary %>% select(un_code, baseline_advantage = avg_tariff_advantage),
      by = "un_code"
    ) %>%
    mutate(
      is_affected = un_code %in% affected_un_codes,
      baseline_advantage = ifelse(is_affected, baseline_advantage, NA_real_)
    )

  # Create long format for dual bars
  chart2_long <- chart2_data %>%
    select(exporter, exporter_ordered, un_code, is_affected, scenario_advantage = avg_tariff_advantage, baseline_advantage) %>%
    tidyr::pivot_longer(
      cols = c(scenario_advantage, baseline_advantage),
      names_to = "advantage_type",
      values_to = "advantage"
    ) %>%
    filter(!is.na(advantage)) %>%
    mutate(
      advantage_type = factor(advantage_type, levels = c("baseline_advantage", "scenario_advantage")),
      bar_label = sprintf("%.1f%%", advantage),
      # For scenario bars: green if positive, red if negative; for baseline: gray
      bar_fill = case_when(
        advantage_type == "baseline_advantage" ~ "baseline",
        advantage >= 0 ~ "positive",
        TRUE ~ "negative"
      )
    )

  p2 <- ggplot(chart2_long, aes(x = exporter_ordered, y = advantage, fill = bar_fill, group = advantage_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = bar_label,
                  hjust = ifelse(advantage < 0, 1.1, -0.1)),
              position = position_dodge(width = 0.8),
              color = "#333333",
              family = "sans",
              size = 5,
              fontface = "bold") +
    geom_hline(yintercept = 0, color = "#333333", size = 1, linetype = "solid") +
    scale_fill_manual(
      values = c("baseline" = "#AAAAAA", "positive" = "#28A745", "negative" = "#DC3545"),
      labels = c("baseline" = "Baseline", "positive" = "Advantage (Scenario)", "negative" = "Disadvantage (Scenario)"),
      name = ""
    ) +
    labs(
      title = "Relative 'Trump Tariff Advantage'",
      subtitle = chart2_subtitle,
      x = "",
      y = "",
      caption = "Gray = Baseline | Green = Advantage, Red = Disadvantage (Scenario)\nSource: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 32, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 16, color = "#333333", face = "bold"),
      plot.margin = margin(t = 20, r = 60, b = 20, l = 60),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 14)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.15, 0.15))
    ) +
    coord_flip()
} else {
  # Original single-bar chart
  p2 <- ggplot(chart2_data, aes(x = exporter_ordered, y = avg_tariff_advantage)) +
    geom_bar(stat = "identity", width = 0.8,
             fill = ifelse(chart2_data$avg_tariff_advantage >= 0, "#28A745", "#DC3545")) +
    geom_text(aes(label = sprintf("%.1f%%", avg_tariff_advantage)),
              hjust = ifelse(chart2_data$avg_tariff_advantage < 0, 1.1, -0.1),
              color = "#333333",
              family = "sans",
              size = 6,
              fontface = "bold") +
    geom_hline(yintercept = 0, color = "#333333", size = 1, linetype = "solid") +
    labs(
      title = "Relative 'Trump Tariff Advantage'",
      subtitle = chart2_subtitle,
      x = "",
      y = "",
      caption = "Green = Advantage, Red = Disadvantage\nSource: Global Trade Alert,\nUSITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 32, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 20, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 16, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 16, color = "#333333", face = "bold"),
      plot.margin = margin(t = 20, r = 60, b = 20, l = 60),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_blank()
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.15, 0.15))
    ) +
    coord_flip()
}

# Save Chart 2 - Portrait for LinkedIn
ggsave(file.path(RUN_OUTPUT_DIR, "analysis", "charts", "relative_advantage_top20.png"), plot = p2, width = 10, height = 12,
       dpi = 300, bg = "white", units = "in", limitsize = FALSE)

cat(sprintf("   Saved relative advantage chart: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "charts", "relative_advantage_top20.png")))

# Export chart data to Excel (all countries, not just top 20)
write_xlsx(
  list(
    "All Countries Effective Rates" = country_advantage_summary %>%
      select(exporter, total_import_value_bn, avg_own_rate, avg_own_rate_original) %>%
      rename(
        "Exporter" = exporter,
        "Import Value 2024 (bn USD)" = total_import_value_bn,
        "Effective Rate (%)" = avg_own_rate
      ) %>%
      arrange(desc(`Import Value 2024 (bn USD)`)),
    "All Countries Relative Advantage" = country_advantage_summary %>%
      select(exporter, total_import_value_bn, avg_own_rate, avg_competitor_rate, avg_tariff_advantage) %>%
      rename(
        "Exporter" = exporter,
        "Import Value 2024 (bn USD)" = total_import_value_bn,
        "Own Rate Effective Rate (%)" = avg_own_rate,
        "Competitor Average Effective Rate (%)" = avg_competitor_rate,
        "Tariff Advantage (%)" = avg_tariff_advantage
      ) %>%
      arrange(desc(`Import Value 2024 (bn USD)`))
  ),
  path = file.path(RUN_OUTPUT_DIR, "analysis", "relative_advantage_data.xlsx")
)

cat(sprintf("   Saved chart data: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "relative_advantage_data.xlsx")))

# Clean up
us_imports[, `:=`(total_imports_by_hs = NULL, total_tariff_weighted_by_hs = NULL)]

cat("\n=== RELATIVE TARIFF ADVANTAGE ANALYSIS COMPLETE ===\n")