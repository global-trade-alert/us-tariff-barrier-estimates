# =============================================================================
# US Tariff Analysis - Analytical Functions
# Analyzes processed tariff data and generates insights
# =============================================================================

# Load necessary libraries
library(data.table)
library(dplyr)
library(writexl)
library(ggplot2)

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

cat("=== US Tariff Analysis - Analytical Functions ===\n")
cat("Loading processed data and generating analysis...\n\n")

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

# =============================================================================
# 2. TRADE-WEIGHTED AVERAGE TARIFF RATES BY COUNTRY
# =============================================================================

cat("\n2. Calculating trade-weighted average tariff rates by country...\n")

# Calculate effective rates
us_imports[, effective_rate := rate]

# Calculate trade-weighted average rates by exporter
country_analysis <- us_imports[, .(
  total_imports_bn = sum(us_imports_bn),
  weighted_tariff_revenue = sum(effective_rate * us_imports_bn / 100),
  total_tariff_points = sum(effective_rate * us_imports_bn),
  total_value_for_weighting = sum(us_imports_bn)
), by = .(exporter, iso2, un_code)]

# Calculate trade-weighted average rates
country_analysis[, avg_rate := total_tariff_points / total_value_for_weighting]

# Create final output
country_summary <- country_analysis[, .(
  exporter = exporter,
  total_value = total_imports_bn,
  avg_rate = round(avg_rate, 2)
)][order(-total_value)]

cat(sprintf("   Calculated rates for %d countries\n", nrow(country_summary)))

# Display top 20 countries by import value
cat("\nTop 20 countries by import value:\n")
print(head(country_summary, 20))

# =============================================================================
# 3. EXPORT RESULTS
# =============================================================================

cat("\n3. Exporting country analysis results...\n")

# Export to Excel
write_xlsx(
  list(
    "Country Tariff Analysis" = country_summary %>%
      rename(
        "Exporter" = exporter,
        "Total Imports 2024 (bn USD)" = total_value,
        "Avg Effective Rate (%)" = avg_rate
      ),
    "Detailed Country Data" = country_analysis %>%
      select(exporter, iso2, un_code, total_imports_bn, avg_rate, weighted_tariff_revenue) %>%
      rename(
        "Exporter" = exporter,
        "ISO2 Code" = iso2,
        "UN Code" = un_code,
        "Total Imports 2024 (bn USD)" = total_imports_bn,
        "Avg Effective Rate (%)" = avg_rate,
        "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue
      ) %>%
      arrange(desc(`Total Imports 2024 (bn USD)`))
  ),
  path = file.path(RUN_OUTPUT_DIR, "analysis", "country_tariff_analysis.xlsx")
)

cat(sprintf("Saved country analysis to: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "country_tariff_analysis.xlsx")))

# =============================================================================
# 4. SUMMARY STATISTICS
# =============================================================================

cat("\n=== COUNTRY ANALYSIS SUMMARY ===\n")
cat(sprintf("Total countries analyzed: %d\n", nrow(country_summary)))
cat(sprintf("Countries with 0%% average rate: %d\n", sum(country_summary$avg_rate == 0)))
cat(sprintf("Countries with >0%% average rate: %d\n", sum(country_summary$avg_rate > 0)))

# Summary of highest rates
cat(sprintf("Highest average rate: %.1f%% (%s)\n",
            max(country_summary$avg_rate),
            country_summary[which.max(avg_rate)]$exporter))

# Trade-weighted global average
cat("\nTrade-weighted global average rate:\n")
cat(sprintf("  %.2f%%\n",
            sum(country_analysis$total_tariff_points) / sum(country_analysis$total_value_for_weighting)))

# Show rate distribution
rate_ranges <- country_summary[, .(
  count = .N,
  total_value = sum(total_value)
), by = .(
  rate_range = cut(avg_rate,
                   breaks = c(-0.1, 0, 10, 20, 30, 50, 100),
                   labels = c("0%", "0.1-10%", "10.1-20%", "20.1-30%", "30.1-50%", "50%+"),
                   include.lowest = TRUE)
)]

cat("\nTariff rate distribution by country:\n")
print(rate_ranges)

# =============================================================================
# 5. VISUALIZATION - HYPOTHETICAL TARIFF REVENUE
# =============================================================================

cat("\n5. Creating hypothetical tariff revenue visualization...\n")

# Get top 20 countries by tariff revenue for chart
top20_revenue <- head(country_analysis[order(-weighted_tariff_revenue)], 20)

# Chart: Hypothetical Tariff Revenue - Highest at Top
revenue_chart_data <- top20_revenue %>%
  arrange(weighted_tariff_revenue) %>%  # Ascending order for coord_flip to put highest at top
  mutate(exporter_ordered = factor(exporter, levels = exporter))

p_revenue <- ggplot(revenue_chart_data, aes(x = exporter_ordered, y = weighted_tariff_revenue)) +
  geom_bar(stat = "identity", fill = "#2171C2", width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", weighted_tariff_revenue)), 
            hjust = -0.1,
            color = "#333333",
            family = "sans", 
            size = 6,
            fontface = "bold") +
  labs(
    title = "Hypothetical New U.S. Tariff Revenue",
    subtitle = "Top 20 Import Origins by Tariff Revenue (in USD bn)\n(ignoring supply/demand adjustments)",
    x = "",
    y = "",
    caption = "Source: Global Trade Alert, author's calculations using USITC DataWeb import statistics.\nRevenue calculated using additional tariff x 2024 imports."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(size = 28, face = "bold", color = "#2171C2", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
    plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16, color = "#333333", face = "bold"),
    plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  coord_flip()

# Save chart
ggsave(file.path(RUN_OUTPUT_DIR, "analysis", "charts", "hypothetical_tariff_revenue_top20.png"), plot = p_revenue, width = 10, height = 12,
       dpi = 300, bg = "white", units = "in", limitsize = FALSE)

cat(sprintf("   Saved tariff revenue chart: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "charts", "hypothetical_tariff_revenue_top20.png")))

# Export all countries revenue data to Excel
write_xlsx(
  list(
    "All Countries Tariff Revenue" = country_analysis %>%
      select(exporter, total_imports_bn, weighted_tariff_revenue, avg_rate) %>%
      rename(
        "Exporter" = exporter,
        "Total Imports 2024 (bn USD)" = total_imports_bn,
        "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue,
        "Avg Effective Rate (%)" = avg_rate
      ) %>%
      arrange(desc(`Hypothetical Tariff Revenue (bn USD)`)),
    "Top 20 Chart Data" = revenue_chart_data %>%
      select(exporter, total_imports_bn, weighted_tariff_revenue, avg_rate) %>%
      rename(
        "Exporter" = exporter,
        "Total Imports 2024 (bn USD)" = total_imports_bn,
        "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue,
        "Avg Effective Rate (%)" = avg_rate
      )
  ),
  path = file.path(RUN_OUTPUT_DIR, "analysis", "hypothetical_tariff_revenue_data.xlsx")
)

cat(sprintf("   Saved revenue data: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "hypothetical_tariff_revenue_data.xlsx")))

# =============================================================================
# 6. PRODUCT-LEVEL TARIFF REVENUE ANALYSIS
# =============================================================================

cat("\n6. Creating product-level tariff revenue analysis...\n")

# Calculate tariff revenue by HS 8-digit product
product_analysis <- us_imports[, .(
  total_imports_bn = sum(us_imports_bn),
  weighted_tariff_revenue = sum(effective_rate * us_imports_bn / 100),
  avg_rate = round(weighted.mean(effective_rate, us_imports_bn), 2),
  exporter_count = length(unique(exporter)),
  product_name = first(hs_8digit_name)
), by = hs_8digit][order(-weighted_tariff_revenue)]

cat(sprintf("   Calculated revenue for %d unique HS 8-digit products\n", nrow(product_analysis)))

# Get top 20 products by tariff revenue for chart
top20_products <- head(product_analysis, 10)

# Create multi-line product names for chart display (allow 2-3 lines)
wrap_text <- function(text, width = 40) {
  # Split long text into multiple lines
  words <- strsplit(text, " ")[[1]]
  lines <- character()
  current_line <- ""
  
  for (word in words) {
    test_line <- if (current_line == "") word else paste(current_line, word)
    if (nchar(test_line) <= width) {
      current_line <- test_line
    } else {
      if (current_line != "") {
        lines <- c(lines, current_line)
        current_line <- word
      } else {
        # Word is too long, just add it
        lines <- c(lines, word)
        current_line <- ""
      }
    }
  }
  if (current_line != "") {
    lines <- c(lines, current_line)
  }
  
  # Limit to 3 lines max
  if (length(lines) > 3) {
    lines <- lines[1:3]
    lines[3] <- paste0(lines[3], "...")
  }
  
  return(paste(lines, collapse = "\n"))
}

top20_products[, short_name := sapply(product_name, wrap_text)]

# Ensure unique names by adding HS code suffix if duplicates exist
top20_products[, short_name := ifelse(duplicated(short_name) | duplicated(short_name, fromLast = TRUE),
                                     paste0(short_name, "\n(", hs_8digit, ")"),
                                     short_name)]

# Chart: Hypothetical Tariff Revenue by Product - Highest at Top
product_chart_data <- top20_products %>%
  arrange(weighted_tariff_revenue) %>%  # Ascending order for coord_flip to put highest at top
  mutate(product_ordered = factor(short_name, levels = short_name))

p_product <- ggplot(product_chart_data, aes(x = product_ordered, y = weighted_tariff_revenue)) +
  geom_bar(stat = "identity", fill = "#2171C2", width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", weighted_tariff_revenue)), 
            hjust = -0.1,
            color = "#333333",
            family = "sans", 
            size = 5.5,
            fontface = "bold") +
  labs(
    title = "Hypothetical New U.S. Tariff Revenue",
    subtitle = "Top 10 Products by Tariff Revenue (in USD bn)\n(ignoring supply/demand adjustments)",
    x = "",
    y = "",
    caption = "Source: Global Trade Alert, author's calculations using USITC DataWeb import statistics.\nRevenue calculated using additional tariff x 2024 imports."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(size = 28, face = "bold", color = "#2171C2", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
    plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15, color = "#333333", face = "bold", lineheight = 0.9),
    plot.margin = margin(t = 20, r = 40, b = 20, l = 40),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  coord_flip()

# Save chart
ggsave(file.path(RUN_OUTPUT_DIR, "analysis", "charts", "hypothetical_tariff_revenue_by_product_top10.png"), plot = p_product, width = 14, height = 16,
       dpi = 300, bg = "white", units = "in", limitsize = FALSE)

cat(sprintf("   Saved product revenue chart: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "charts", "hypothetical_tariff_revenue_by_product_top10.png")))

# Export all products revenue data to Excel
write_xlsx(
  list(
    "All Products Tariff Revenue" = product_analysis %>%
      select(hs_8digit, product_name, total_imports_bn, weighted_tariff_revenue, avg_rate, exporter_count) %>%
      rename(
        "HS 8-Digit Code" = hs_8digit,
        "Product Description" = product_name,
        "Total Imports 2024 (bn USD)" = total_imports_bn,
        "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue,
        "Avg Effective Rate (%)" = avg_rate,
        "Number of Exporters" = exporter_count
      ) %>%
      arrange(desc(`Hypothetical Tariff Revenue (bn USD)`)),
    "Top 20 Chart Data" = top20_products %>%
      select(hs_8digit, product_name, total_imports_bn, weighted_tariff_revenue, avg_rate, exporter_count) %>%
      rename(
        "HS 8-Digit Code" = hs_8digit,
        "Product Description" = product_name,
        "Total Imports 2024 (bn USD)" = total_imports_bn,
        "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue,
        "Avg Effective Rate (%)" = avg_rate,
        "Number of Exporters" = exporter_count
      )
  ),
  path = file.path(RUN_OUTPUT_DIR, "analysis", "hypothetical_tariff_revenue_by_product_data.xlsx")
)

cat(sprintf("   Saved product revenue data: %s\n", file.path(RUN_OUTPUT_DIR, "analysis", "hypothetical_tariff_revenue_by_product_data.xlsx")))

cat("\n=== COUNTRY AND PRODUCT ANALYSIS COMPLETE ===\n")