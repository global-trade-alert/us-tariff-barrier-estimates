# =============================================================================
# SCOTUS CHART FUNCTIONS
# =============================================================================
# Chart generation functions for the SCOTUS IEEPA strike-down blog post.
# Produces 4 charts comparing before/after SCOTUS scenarios.
#
# All charts use GTA visual identity via cowplot composition:
#   - Navy top bar (#003366)
#   - #E8EEF5 background throughout
#   - GTA logo (bottom-right)
#   - Source caption (bottom-left)
#   - Active title (key finding as statement)
#
# Charts:
#   1. Hero: Aggregate before/after composition (2 horizontal bars)
#   2. Top 20 import sources: BEFORE SCOTUS composition
#   3. Top 20 import sources: AFTER SCOTUS composition
#   4. Sector breakdown (HS-2): before vs after paired bars
#
# Dependencies: ggplot2, cowplot, grid, data.table, dplyr, tidyr, png
# =============================================================================

library(ggplot2)
library(cowplot)
library(grid)
library(data.table)
library(dplyr)
library(tidyr)

# =============================================================================
# CONSTANTS
# =============================================================================

# GTA Logo
LOGO_PATH <- "/Users/johannesfritz/Documents/GitHub/jf-private/jf-ceo/sgept-backoffice/assets/gta-logos/GTA Logo Color light.png"

# GTA colour palette
GTA_BG <- "#E8EEF5"
GTA_NAVY <- "#003366"
GTA_TEXT_DARK <- "#1a1a2e"
GTA_TEXT_GREY <- "grey40"

# Tariff layer colours (consistent with legacy tariff_composition.R)
LAYER_COLORS <- c(
  "HTS Baseline"  = "#A8D5E5",
  "IEEPA (10%)"   = "#5BA3D9",
  "IEEPA Top-up"  = "#2171C2",
  "Section 232"   = "#F5A623",
  "Emergency"      = "#8B0000",
  "Section 301"    = "#DC3545"
)

# Layer factor levels for stacking order
# Reversed so HTS appears at the axis after coord_flip
LAYER_LEVELS <- c("s301_layer", "emergency_layer", "s232_layer",
                   "ieepa_topup_layer", "ieepa_baseline_layer", "hts_layer")
LAYER_LABELS <- c("Section 301", "Emergency", "Section 232",
                   "IEEPA Top-up", "IEEPA (10%)", "HTS Baseline")

# Source caption
SOURCE_TEXT <- "Source: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"

# Minimum segment value to display a label (percentage points)
LABEL_MIN_PP <- 0.3

# =============================================================================
# GTA BASE THEME (inner ggplot only - no title/subtitle/caption)
# =============================================================================

theme_gta_inner <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = GTA_BG, colour = NA),
    panel.background = element_rect(fill = GTA_BG, colour = NA),
    axis.text        = element_text(size = 11, color = "grey30"),
    axis.title       = element_text(size = 10, color = GTA_TEXT_GREY),
    axis.ticks       = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.5, "cm"),
    legend.background = element_rect(fill = GTA_BG, colour = NA),
    plot.margin      = margin(5, 15, 5, 10)
  )

# =============================================================================
# COWPLOT WRAPPER: gta_wrap()
# =============================================================================
#' Wrap a ggplot in the GTA visual identity using cowplot composition.
#'
#' @param inner_plot ggplot object (no title/subtitle/caption)
#' @param title Active chart title (key finding as statement)
#' @param subtitle Descriptive context (optional)
#' @param source_text Source caption (default: GTA + USITC)
#' @param logo_path Path to GTA logo PNG
#' @param title_size Title font size (default 19)
#' @param chart_y Bottom of chart area (default 0.07)
#' @param chart_top Top of chart area (auto-calculated if NULL)
#' @return cowplot ggdraw object
gta_wrap <- function(inner_plot,
                     title,
                     subtitle = NULL,
                     source_text = SOURCE_TEXT,
                     logo_path = LOGO_PATH,
                     title_size = 19,
                     chart_y = 0.07,
                     chart_top = NULL) {

  # Layout positions
  title_y   <- 0.955
  sub_y     <- 0.905

  if (is.null(chart_top)) {
    chart_top <- if (!is.null(subtitle)) 0.875 else 0.92
  }

  chart_height <- chart_top - chart_y

  final <- ggdraw() +
    # Background
    draw_grob(rectGrob(gp = gpar(fill = GTA_BG, col = NA))) +
    # Title
    draw_label(title,
               x = 0.04, y = title_y, hjust = 0, vjust = 1,
               fontface = "bold", size = title_size, color = GTA_TEXT_DARK)

  # Subtitle (optional)
  if (!is.null(subtitle)) {
    final <- final +
      draw_label(subtitle,
                 x = 0.04, y = sub_y, hjust = 0, vjust = 1,
                 size = 11, color = GTA_TEXT_GREY)
  }

  # Main chart
  final <- final +
    draw_plot(inner_plot, x = 0, y = chart_y,
              width = 1, height = chart_height)

  # Source caption (bottom-left)
  final <- final +
    draw_label(source_text,
               x = 0.04, y = 0.025, hjust = 0,
               size = 9, color = GTA_TEXT_GREY)

  # GTA logo (bottom-right)
  if (file.exists(logo_path)) {
    final <- final +
      draw_image(logo_path,
                 x = 0.70, y = 0.005, width = 0.26, height = 0.045)
  }

  return(final)
}


# =============================================================================
# HELPER: Prepare long-format composition data for stacking
# =============================================================================
#' Convert wide composition data to long format for ggplot stacking.
#'
#' @param chart_data data.table with *_layer columns
#' @return data.frame in long format with layer factor and label positions
prepare_composition_long <- function(chart_data) {

  # Check if IEEPA top-up has any non-zero values
  has_ieepa_topup <- any(chart_data$ieepa_topup_layer > 0.01, na.rm = TRUE)

  chart_long <- chart_data %>%
    select(display_label, geography, iso_code, hts_layer, ieepa_baseline_layer,
           ieepa_topup_layer, s232_layer, emergency_layer, s301_layer, total_rate) %>%
    pivot_longer(
      cols = c(hts_layer, ieepa_baseline_layer, ieepa_topup_layer,
               s232_layer, emergency_layer, s301_layer),
      names_to = "layer",
      values_to = "rate"
    ) %>%
    mutate(
      layer = factor(layer, levels = LAYER_LEVELS, labels = LAYER_LABELS)
    )

  # Remove IEEPA Top-up from legend if all zero
  if (!has_ieepa_topup) {
    chart_long <- chart_long %>% filter(layer != "IEEPA Top-up")
  }

  # Calculate cumulative positions for direct labels
  chart_long <- chart_long %>%
    arrange(display_label, desc(layer)) %>%
    group_by(display_label) %>%
    mutate(
      cumsum_rate = cumsum(rate),
      label_pos   = cumsum_rate - rate / 2,
      rate_label  = ifelse(rate >= LABEL_MIN_PP, sprintf("%.1f%%", rate), "")
    ) %>%
    ungroup()

  return(chart_long)
}


# =============================================================================
# CHART 1: create_before_after_chart()
# =============================================================================
#' Create hero chart showing aggregate tariff composition before vs after SCOTUS.
#'
#' @param before_comp Named list from aggregate_contributions() for baseline
#' @param after_comp Named list from aggregate_contributions() for IEEPA strike-down
#' @return cowplot ggdraw object (landscape)
create_before_after_chart <- function(before_comp, after_comp) {

  # Build 2-row data.table
  chart_data <- data.table(
    geography  = c("After SCOTUS", "Before SCOTUS"),
    iso_code   = c("WLD", "WLD"),
    hts_layer              = c(after_comp$hts_layer, before_comp$hts_layer),
    ieepa_baseline_layer   = c(after_comp$ieepa_baseline_layer, before_comp$ieepa_baseline_layer),
    ieepa_topup_layer      = c(after_comp$ieepa_topup_layer, before_comp$ieepa_topup_layer),
    s232_layer             = c(after_comp$s232_layer, before_comp$s232_layer),
    emergency_layer        = c(after_comp$emergency_layer, before_comp$emergency_layer),
    s301_layer             = c(after_comp$s301_layer, before_comp$s301_layer),
    total_rate             = c(after_comp$total_rate, before_comp$total_rate)
  )

  # Display labels with total rate
  chart_data[, display_label := sprintf("%s (%.1f%%)", geography, total_rate)]
  # Factor: Before at top (after coord_flip), After at bottom
  chart_data[, display_label := factor(display_label, levels = display_label)]

  # Prepare long format
  chart_long <- prepare_composition_long(chart_data)

  # Inner ggplot
  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = layer)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(y = label_pos, label = rate_label),
              color = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = LAYER_COLORS, name = NULL,
                      breaks = rev(LAYER_LABELS),
                      drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = "Effective Tariff Rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))

  # Wrap in GTA cowplot (title built from data)
  title_text <- sprintf(
    "The SCOTUS ruling cuts the average US tariff from %.1f%% to %.1f%%",
    before_comp$total_rate, after_comp$total_rate
  )
  gta_wrap(
    p,
    title    = title_text,
    subtitle = "Trade-weighted average tariff rate on all US imports, by tariff instrument",
    title_size = 18
  )
}


#' Mobile variant of the before/after hero chart (portrait orientation).
#'
#' @param before_comp Named list from aggregate_contributions() for baseline
#' @param after_comp Named list from aggregate_contributions() for IEEPA strike-down
#' @return cowplot ggdraw object (portrait)
create_before_after_chart_mobile <- function(before_comp, after_comp) {

  chart_data <- data.table(
    geography  = c("Before\nSCOTUS", "After\nSCOTUS"),
    iso_code   = c("WLD", "WLD"),
    hts_layer              = c(before_comp$hts_layer, after_comp$hts_layer),
    ieepa_baseline_layer   = c(before_comp$ieepa_baseline_layer, after_comp$ieepa_baseline_layer),
    ieepa_topup_layer      = c(before_comp$ieepa_topup_layer, after_comp$ieepa_topup_layer),
    s232_layer             = c(before_comp$s232_layer, after_comp$s232_layer),
    emergency_layer        = c(before_comp$emergency_layer, after_comp$emergency_layer),
    s301_layer             = c(before_comp$s301_layer, after_comp$s301_layer),
    total_rate             = c(before_comp$total_rate, after_comp$total_rate)
  )

  chart_data[, display_label := sprintf("%s\n(%.1f%%)", geography, total_rate)]
  chart_data[, display_label := factor(display_label, levels = display_label)]

  # Long format (vertical stacking order - bottom to top)
  has_ieepa_topup <- any(chart_data$ieepa_topup_layer > 0.01, na.rm = TRUE)

  chart_long <- chart_data %>%
    select(display_label, geography, iso_code, hts_layer, ieepa_baseline_layer,
           ieepa_topup_layer, s232_layer, emergency_layer, s301_layer, total_rate) %>%
    pivot_longer(
      cols = c(hts_layer, ieepa_baseline_layer, ieepa_topup_layer,
               s232_layer, emergency_layer, s301_layer),
      names_to = "layer",
      values_to = "rate"
    ) %>%
    mutate(
      layer = factor(layer,
                     levels = c("hts_layer", "ieepa_baseline_layer", "ieepa_topup_layer",
                                "s232_layer", "emergency_layer", "s301_layer"),
                     labels = c("HTS Baseline", "IEEPA (10%)", "IEEPA Top-up",
                                "Section 232", "Emergency", "Section 301"))
    )

  if (!has_ieepa_topup) {
    chart_long <- chart_long %>% filter(layer != "IEEPA Top-up")
  }

  chart_long <- chart_long %>%
    arrange(display_label, layer) %>%
    group_by(display_label) %>%
    mutate(
      cumsum_rate = cumsum(rate),
      label_pos   = cumsum_rate - rate / 2,
      rate_label  = ifelse(rate >= LABEL_MIN_PP, sprintf("%.1f%%", rate), "")
    ) %>%
    ungroup()

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = layer)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(y = label_pos, label = rate_label),
              color = "white", fontface = "bold", size = 3.5) +
    scale_fill_manual(values = LAYER_COLORS, name = NULL, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Effective Tariff Rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.x    = element_text(size = 11, face = "bold", lineheight = 0.9),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))

  title_text <- sprintf(
    "SCOTUS cuts the average US tariff\nfrom %.1f%% to %.1f%%",
    before_comp$total_rate, after_comp$total_rate
  )
  gta_wrap(
    p,
    title      = title_text,
    subtitle   = "Trade-weighted average, by tariff instrument",
    title_size = 16,
    chart_top  = 0.82
  )
}


# =============================================================================
# CHARTS 2 & 3: create_composition_chart_scotus()
# =============================================================================
#' Create stacked horizontal bar chart for tariff composition (top 20 + Global).
#' Ported from legacy tariff_composition.R, wrapped in GTA cowplot.
#'
#' @param compositions data.table with country-level composition columns
#' @param title Active chart title
#' @param subtitle Descriptive context
#' @param top_n Number of countries to include (default 20, by import value)
#' @param include_global Include a Global aggregate bar (default TRUE)
#' @return cowplot ggdraw object
create_composition_chart_scotus <- function(compositions,
                                            title,
                                            subtitle = NULL,
                                            top_n = 20,
                                            include_global = TRUE) {

  # Select top N countries by import value (excluding Global)
  country_pool <- compositions[geography != "Global"]
  top_isos <- country_pool[order(-total_imports_bn)][seq_len(min(top_n, nrow(country_pool)))]$iso_code

  # Build geography list
  geographies <- if (include_global) c("Global", top_isos) else top_isos
  chart_data <- compositions[geography %in% geographies | iso_code %in% geographies]

  if (nrow(chart_data) == 0) {
    warning("No matching geographies found")
    return(NULL)
  }

  # Display labels with total rate, ordered by effective rate
  chart_data[, display_label := sprintf("%s (%.1f%%)", geography, total_rate)]
  chart_data <- chart_data[order(total_rate)]
  chart_data[, display_label := factor(display_label, levels = display_label)]

  # Prepare long format
  chart_long <- prepare_composition_long(chart_data)

  # Inner ggplot
  n_bars <- nrow(chart_data)
  bar_width <- ifelse(n_bars > 15, 0.7, 0.6)
  label_size <- ifelse(n_bars > 15, 3.0, 3.5)

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = layer)) +
    geom_bar(stat = "identity", width = bar_width) +
    geom_text(aes(y = label_pos, label = rate_label),
              color = "white", fontface = "bold", size = label_size) +
    scale_fill_manual(values = LAYER_COLORS, name = NULL,
                      breaks = rev(LAYER_LABELS),
                      drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = "Effective Tariff Rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))

  gta_wrap(
    p,
    title    = title,
    subtitle = subtitle
  )
}


# =============================================================================
# CHART: create_country_comparison_chart()
# =============================================================================
#' Create paired horizontal bar chart comparing before/after tariff rates by country.
#' Same style as sector comparison chart (no decomposition, just total rates).
#'
#' @param before_country data.table with country-level compositions (before SCOTUS)
#' @param after_country data.table with country-level compositions (after SCOTUS)
#' @param top_n Number of countries to show (default 20, by import value)
#' @param include_global Include a Global aggregate bar (default TRUE)
#' @return cowplot ggdraw object (landscape)
create_country_comparison_chart <- function(before_country, after_country,
                                            top_n = 20, include_global = TRUE) {

  # Merge before and after
  merged <- merge(
    before_country[geography != "Global", .(geography, iso_code, total_imports_bn,
                                            rate_before = total_rate)],
    after_country[geography != "Global", .(iso_code, rate_after = total_rate)],
    by = "iso_code"
  )
  merged[, change_pp := rate_after - rate_before]

  # Top N by import value
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  # Add Global row if requested
  if (include_global) {
    global_before <- before_country[geography == "Global"]
    global_after  <- after_country[geography == "Global"]
    if (nrow(global_before) > 0 && nrow(global_after) > 0) {
      global_row <- data.table(
        geography = "Global",
        iso_code = "WLD",
        total_imports_bn = global_before$total_imports_bn,
        rate_before = global_before$total_rate,
        rate_after = global_after$total_rate,
        change_pp = global_after$total_rate - global_before$total_rate
      )
      merged <- rbind(global_row, merged)
    }
  }

  # Display labels with country name
  merged[, display_label := geography]
  # Order by before rate (largest rate at top after coord_flip)
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  # Pivot to long for paired bars
  chart_long <- merged %>%
    select(display_label, rate_before, rate_after) %>%
    pivot_longer(
      cols = c(rate_before, rate_after),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = c("rate_before", "rate_after"),
                        labels = c("Before SCOTUS", "After SCOTUS"))
    )

  # Paired bar colours
  scenario_colors <- c(
    "Before SCOTUS" = "#AAAAAA",
    "After SCOTUS"  = GTA_NAVY
  )

  n_bars <- nrow(merged)
  label_size <- ifelse(n_bars > 15, 3.8, 4.2)

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7),
             width = 0.65) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.7),
      hjust = -0.15, size = label_size, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = scenario_colors, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE))

  gta_wrap(
    p,
    title    = "Before vs after SCOTUS: tariff rates for top 20 US import sources",
    subtitle = "Trade-weighted average tariff rate, top 20 countries by import value"
  )
}


# =============================================================================
# CHART 4: create_sector_comparison_chart()
# =============================================================================
#' Create paired horizontal bar chart comparing before/after tariff rates by HS-2 sector.
#'
#' @param before_sector data.table with HS-2 aggregations (before SCOTUS)
#' @param after_sector data.table with HS-2 aggregations (after SCOTUS)
#' @param top_n Number of sectors to show (default 15, by import value)
#' @return cowplot ggdraw object (landscape)
create_sector_comparison_chart <- function(before_sector, after_sector, top_n = 15) {

  # Merge before and after
  merged <- merge(
    before_sector[, .(hs2, chapter_label, total_imports_bn, rate_before = total_rate)],
    after_sector[, .(hs2, rate_after = total_rate)],
    by = "hs2"
  )
  merged[, change_pp := rate_after - rate_before]

  # Top N by import value
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  # Display labels: "84: Machinery"
  merged[, display_label := chapter_label]
  # Order by before-SCOTUS tariff rate (largest rate at top after coord_flip)
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  # Pivot to long for paired bars
  chart_long <- merged %>%
    select(display_label, rate_before, rate_after) %>%
    pivot_longer(
      cols = c(rate_before, rate_after),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = c("rate_before", "rate_after"),
                        labels = c("Before SCOTUS", "After SCOTUS"))
    )

  # Paired bar colours
  scenario_colors <- c(
    "Before SCOTUS" = "#AAAAAA",
    "After SCOTUS"  = GTA_NAVY
  )

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7),
             width = 0.65) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.7),
      hjust = -0.15, size = 4.0, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = scenario_colors, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE))

  gta_wrap(
    p,
    title    = "How tariffs changed across US import sectors",
    subtitle = "Trade-weighted average tariff rate by HS-2 chapter, top 15 sectors by import value"
  )
}


#' Mobile variant of the sector comparison chart (portrait orientation).
#'
#' @param before_sector data.table with HS-2 aggregations (before SCOTUS)
#' @param after_sector data.table with HS-2 aggregations (after SCOTUS)
#' @param top_n Number of sectors to show (default 15, by import value)
#' @return cowplot ggdraw object (portrait)
create_sector_comparison_chart_mobile <- function(before_sector, after_sector, top_n = 15) {

  merged <- merge(
    before_sector[, .(hs2, chapter_label, total_imports_bn, rate_before = total_rate)],
    after_sector[, .(hs2, rate_after = total_rate)],
    by = "hs2"
  )
  merged[, change_pp := rate_after - rate_before]
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  merged[, display_label := chapter_label]
  # Order by before-SCOTUS tariff rate (largest rate at top after coord_flip)
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  chart_long <- merged %>%
    select(display_label, rate_before, rate_after) %>%
    pivot_longer(
      cols = c(rate_before, rate_after),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = c("rate_before", "rate_after"),
                        labels = c("Before SCOTUS", "After SCOTUS"))
    )

  scenario_colors <- c(
    "Before SCOTUS" = "#AAAAAA",
    "After SCOTUS"  = GTA_NAVY
  )

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7),
             width = 0.65) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.7),
      hjust = -0.15, size = 3.5, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = scenario_colors, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 9, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE))

  gta_wrap(
    p,
    title      = "How tariffs changed across\nUS import sectors",
    subtitle   = "Trade-weighted average by HS-2 chapter",
    title_size = 15,
    chart_top  = 0.82
  )
}


# =============================================================================
# CHART 5: create_top_winners_chart()
# =============================================================================
#' Create horizontal bar chart showing tariff reductions by country.
#' Each bar shows the reduction in percentage points (before - after).
#' Import value shown next to country name; subtitle explains the convention.
#'
#' @param before_country data.table with country-level compositions (before SCOTUS)
#' @param after_country data.table with country-level compositions (after SCOTUS)
#' @param top_n Number of countries to show (default 20)
#' @param top_by "reduction" (largest tariff cut) or "imports" (largest import value)
#' @param exclude_geo Character vector of geography names to exclude
#' @return cowplot ggdraw object (landscape)
create_top_winners_chart <- function(before_country, after_country,
                                     top_n = 20,
                                     top_by = "reduction",
                                     exclude_geo = c("Gaza Strip")) {

  # Merge before and after (exclude Global and aggregates)
  merged <- merge(
    before_country[geography != "Global" & iso_code != "EU",
                   .(geography, iso_code, total_imports_bn, rate_before = total_rate)],
    after_country[geography != "Global" & iso_code != "EU",
                  .(iso_code, rate_after = total_rate)],
    by = "iso_code"
  )
  merged[, reduction_pp := rate_before - rate_after]

  # Exclude specified geographies
  if (length(exclude_geo) > 0) {
    merged <- merged[!geography %in% exclude_geo]
  }

  # Top N by chosen criterion
  if (top_by == "imports") {
    merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]
  } else {
    merged <- merged[order(-reduction_pp)][seq_len(min(top_n, nrow(merged)))]
  }

  # Display label: "China ($434.3)" — import value with 1 decimal, no "bn"
  merged[, display_label := sprintf("%s ($%.1f)", geography, total_imports_bn)]
  # Order ascending so largest reduction appears at top after coord_flip
  merged <- merged[order(reduction_pp)]
  merged[, display_label := factor(display_label, levels = display_label)]

  p <- ggplot(merged, aes(x = display_label, y = reduction_pp)) +
    geom_bar(stat = "identity", width = 0.65, fill = GTA_NAVY) +
    geom_text(
      aes(label = sprintf("-%.1f pp", reduction_pp)),
      hjust = -0.15, size = 4.0, fontface = "bold", color = "grey30"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "none"
    )

  # Titles depend on sort criterion
  if (top_by == "imports") {
    chart_title    <- "Tariff cuts for the top 20 US import sources"
    chart_subtitle <- "Reduction in trade-weighted average tariff rate (pp). Figures in brackets: 2024 US imports ($bn)."
  } else {
    chart_title    <- "Which US import sources see the largest tariff cuts"
    chart_subtitle <- "Reduction in trade-weighted average tariff rate (pp). Figures in brackets: 2024 US imports ($bn)."
  }

  gta_wrap(
    p,
    title    = chart_title,
    subtitle = chart_subtitle
  )
}


# =============================================================================
# EU/EMU AGGREGATION HELPER
# =============================================================================

EU_MEMBERS_ISO <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
  "SVN", "ESP", "SWE"
)

EMU_MEMBERS_ISO <- c(
  "AUT", "BEL", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC", "IRL",
  "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)

#' Aggregate EU-level composition from flow-level data.
#'
#' @param flow_data data.table with contribution columns and iso_code
#' @return data.table with one row (geography = "European Union", iso_code = "EU")
aggregate_eu_composition <- function(flow_data) {
  flow_data[iso_code %in% EU_MEMBERS_ISO, .(
    geography    = "European Union",
    iso_code     = "EU",
    hts_layer              = weighted.mean(hts_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_baseline_layer   = weighted.mean(ieepa_baseline_contribution, us_imports_bn, na.rm = TRUE),
    ieepa_topup_layer      = weighted.mean(ieepa_topup_contribution, us_imports_bn, na.rm = TRUE),
    s232_layer             = weighted.mean(s232_contribution, us_imports_bn, na.rm = TRUE),
    emergency_layer        = weighted.mean(emergency_contribution, us_imports_bn, na.rm = TRUE),
    s301_layer             = weighted.mean(s301_contribution, us_imports_bn, na.rm = TRUE),
    total_rate             = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    total_imports_bn       = sum(us_imports_bn)
  )]
}


# =============================================================================
# HS-2 CHAPTER NAME LOOKUP
# =============================================================================

HS2_CHAPTER_NAMES <- c(
  "01" = "Live animals",
  "02" = "Meat",
  "03" = "Fish & seafood",
  "04" = "Dairy, eggs, honey",
  "05" = "Products of animal origin",
  "06" = "Live trees & plants",
  "07" = "Vegetables",
  "08" = "Fruit & nuts",
  "09" = "Coffee, tea, spices",
  "10" = "Cereals",
  "11" = "Milling products",
  "12" = "Oil seeds",
  "13" = "Lac, gums, resins",
  "14" = "Vegetable plaiting materials",
  "15" = "Fats & oils",
  "16" = "Meat preparations",
  "17" = "Sugars",
  "18" = "Cocoa",
  "19" = "Cereal preparations",
  "20" = "Vegetable preparations",
  "21" = "Miscellaneous food",
  "22" = "Beverages",
  "23" = "Food waste, animal feed",
  "24" = "Tobacco",
  "25" = "Salt, sulphur, earth, stone",
  "26" = "Ores, slag, ash",
  "27" = "Mineral fuels & oils",
  "28" = "Inorganic chemicals",
  "29" = "Organic chemicals",
  "30" = "Pharmaceuticals",
  "31" = "Fertilisers",
  "32" = "Tanning, dyeing extracts",
  "33" = "Essential oils, cosmetics",
  "34" = "Soap, wax, candles",
  "35" = "Albuminoidal substances",
  "36" = "Explosives",
  "37" = "Photographic goods",
  "38" = "Miscellaneous chemicals",
  "39" = "Plastics",
  "40" = "Rubber",
  "41" = "Raw hides & skins",
  "42" = "Leather articles",
  "43" = "Furskins",
  "44" = "Wood",
  "45" = "Cork",
  "46" = "Straw manufactures",
  "47" = "Wood pulp",
  "48" = "Paper & paperboard",
  "49" = "Printed books, newspapers",
  "50" = "Silk",
  "51" = "Wool",
  "52" = "Cotton",
  "53" = "Other vegetable fibres",
  "54" = "Man-made filaments",
  "55" = "Man-made staple fibres",
  "56" = "Wadding, felt, nonwovens",
  "57" = "Carpets",
  "58" = "Special woven fabrics",
  "59" = "Impregnated textiles",
  "60" = "Knitted fabrics",
  "61" = "Knitted apparel",
  "62" = "Woven apparel",
  "63" = "Other textile articles",
  "64" = "Footwear",
  "65" = "Headgear",
  "66" = "Umbrellas",
  "67" = "Feathers, artificial flowers",
  "68" = "Stone, plaster, cement",
  "69" = "Ceramic products",
  "70" = "Glass",
  "71" = "Precious metals & stones",
  "72" = "Iron & steel",
  "73" = "Iron/steel articles",
  "74" = "Copper",
  "75" = "Nickel",
  "76" = "Aluminium",
  "78" = "Lead",
  "79" = "Zinc",
  "80" = "Tin",
  "81" = "Other base metals",
  "82" = "Tools, cutlery",
  "83" = "Miscellaneous base metal",
  "84" = "Machinery",
  "85" = "Electrical machinery",
  "86" = "Railway vehicles",
  "87" = "Motor vehicles",
  "88" = "Aircraft",
  "89" = "Ships & boats",
  "90" = "Optical, medical instruments",
  "91" = "Clocks & watches",
  "92" = "Musical instruments",
  "93" = "Arms & ammunition",
  "94" = "Furniture, lighting",
  "95" = "Toys, games, sports",
  "96" = "Miscellaneous manufactures",
  "97" = "Art, antiques",
  "98" = "Special classification",
  "99" = "Special import provisions"
)

#' Get chapter label like "84: Machinery" from HS-2 code.
#'
#' @param hs2 Character HS-2 code (2 digits, zero-padded)
#' @return Character label
get_chapter_label <- function(hs2) {
  name <- HS2_CHAPTER_NAMES[hs2]
  ifelse(is.na(name), paste0(hs2, ": Other"), paste0(hs2, ": ", name))
}


# =============================================================================
# 4-SCENARIO COLOUR PALETTE (S122 Blog Post)
# =============================================================================

SCENARIO_COLORS_4 <- c(
  "Before SCOTUS"      = "#AAAAAA",
  "IEEPA Strike Down"  = "#003366",   # navy
  "S122 Surcharge"     = "#F5A623",   # orange
  "S122 Stack"         = "#2CA58D"    # teal
)

SCENARIO_LEVELS_4 <- c("rate_before", "rate_ieepa", "rate_surcharge", "rate_stack")
SCENARIO_LABELS_4 <- c("Before SCOTUS", "IEEPA Strike Down", "S122 Surcharge", "S122 Stack")

# Extended layer palette including S122
LAYER_COLORS_S122 <- c(
  "HTS Baseline"  = "#A8D5E5",
  "IEEPA (10%)"   = "#5BA3D9",
  "IEEPA Top-up"  = "#2171C2",
  "Section 232"   = "#F5A623",
  "Emergency"      = "#8B0000",
  "Section 301"    = "#DC3545",
  "S122 (15%)"     = "#2CA58D"
)

LAYER_LEVELS_S122 <- c("s301_layer", "s122_layer", "emergency_layer", "s232_layer",
                        "ieepa_topup_layer", "ieepa_baseline_layer", "hts_layer")
LAYER_LABELS_S122 <- c("Section 301", "S122 (15%)", "Emergency", "Section 232",
                        "IEEPA Top-up", "IEEPA (10%)", "HTS Baseline")


#' Prepare long-format composition with S122 layer for 4-scenario charts.
prepare_composition_long_s122 <- function(chart_data) {

  has_ieepa_topup <- any(chart_data$ieepa_topup_layer > 0.01, na.rm = TRUE)
  has_s122 <- any(chart_data$s122_layer > 0.01, na.rm = TRUE)

  chart_long <- chart_data %>%
    select(display_label, geography, iso_code, hts_layer, ieepa_baseline_layer,
           ieepa_topup_layer, s232_layer, emergency_layer, s301_layer, s122_layer, total_rate) %>%
    pivot_longer(
      cols = c(hts_layer, ieepa_baseline_layer, ieepa_topup_layer,
               s232_layer, emergency_layer, s301_layer, s122_layer),
      names_to = "layer",
      values_to = "rate"
    ) %>%
    mutate(
      layer = factor(layer, levels = LAYER_LEVELS_S122, labels = LAYER_LABELS_S122)
    )

  if (!has_ieepa_topup) {
    chart_long <- chart_long %>% filter(layer != "IEEPA Top-up")
  }
  if (!has_s122) {
    chart_long <- chart_long %>% filter(layer != "S122 (15%)")
  }

  chart_long <- chart_long %>%
    arrange(display_label, desc(layer)) %>%
    group_by(display_label) %>%
    mutate(
      cumsum_rate = cumsum(rate),
      label_pos   = cumsum_rate - rate / 2,
      rate_label  = ifelse(rate >= LABEL_MIN_PP, sprintf("%.1f%%", rate), "")
    ) %>%
    ungroup()

  return(chart_long)
}


# =============================================================================
# CHART S122-1: create_four_scenario_chart()
# =============================================================================
#' Create hero chart showing aggregate tariff composition across 4 scenarios.
#' Stacked horizontal bars with layer decomposition.
#'
#' @param baseline_comp Named list from aggregate_contributions() for baseline
#' @param ieepa_comp Named list from aggregate_contributions() for IEEPA strike-down
#' @param surcharge_comp Named list from aggregate_contributions() for S122 surcharge
#' @param stack_comp Named list from aggregate_contributions() for S122 stack
#' @return cowplot ggdraw object (landscape)
create_four_scenario_chart <- function(baseline_comp, ieepa_comp, surcharge_comp, stack_comp) {

  # Build 4-row data.table (order: stack, surcharge, ieepa, before — reversed for coord_flip)
  chart_data <- data.table(
    geography  = c("S122 Stack", "S122 Surcharge", "IEEPA Strike Down", "Before SCOTUS"),
    iso_code   = rep("WLD", 4),
    hts_layer              = c(stack_comp$hts_layer, surcharge_comp$hts_layer,
                               ieepa_comp$hts_layer, baseline_comp$hts_layer),
    ieepa_baseline_layer   = c(stack_comp$ieepa_baseline_layer, surcharge_comp$ieepa_baseline_layer,
                               ieepa_comp$ieepa_baseline_layer, baseline_comp$ieepa_baseline_layer),
    ieepa_topup_layer      = c(stack_comp$ieepa_topup_layer, surcharge_comp$ieepa_topup_layer,
                               ieepa_comp$ieepa_topup_layer, baseline_comp$ieepa_topup_layer),
    s232_layer             = c(stack_comp$s232_layer, surcharge_comp$s232_layer,
                               ieepa_comp$s232_layer, baseline_comp$s232_layer),
    emergency_layer        = c(stack_comp$emergency_layer, surcharge_comp$emergency_layer,
                               ieepa_comp$emergency_layer, baseline_comp$emergency_layer),
    s301_layer             = c(stack_comp$s301_layer, surcharge_comp$s301_layer,
                               ieepa_comp$s301_layer, baseline_comp$s301_layer),
    s122_layer             = c(stack_comp$s122_layer, surcharge_comp$s122_layer,
                               ieepa_comp$s122_layer, baseline_comp$s122_layer),
    total_rate             = c(stack_comp$total_rate, surcharge_comp$total_rate,
                               ieepa_comp$total_rate, baseline_comp$total_rate)
  )

  # Display labels with total rate
  chart_data[, display_label := sprintf("%s (%.1f%%)", geography, total_rate)]
  chart_data[, display_label := factor(display_label, levels = display_label)]

  # Prepare long format (S122-aware)
  chart_long <- prepare_composition_long_s122(chart_data)

  # Inner ggplot
  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = layer)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(y = label_pos, label = rate_label),
              color = "white", fontface = "bold", size = 4.0) +
    scale_fill_manual(values = LAYER_COLORS_S122, name = NULL,
                      breaks = rev(LAYER_LABELS_S122),
                      drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = "Effective Tariff Rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 13, face = "bold"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))

  title_text <- sprintf(
    "Four scenarios: US tariffs range from %.1f%% to %.1f%%",
    ieepa_comp$total_rate, surcharge_comp$total_rate
  )
  gta_wrap(
    p,
    title    = title_text,
    subtitle = "Trade-weighted average tariff rate on all US imports, by tariff instrument",
    title_size = 18
  )
}


#' Mobile variant of the 4-scenario hero chart (portrait orientation).
create_four_scenario_chart_mobile <- function(baseline_comp, ieepa_comp, surcharge_comp, stack_comp) {

  chart_data <- data.table(
    geography  = c("Before\nSCOTUS", "IEEPA\nStrike Down", "S122\nSurcharge", "S122\nStack"),
    iso_code   = rep("WLD", 4),
    hts_layer              = c(baseline_comp$hts_layer, ieepa_comp$hts_layer,
                               surcharge_comp$hts_layer, stack_comp$hts_layer),
    ieepa_baseline_layer   = c(baseline_comp$ieepa_baseline_layer, ieepa_comp$ieepa_baseline_layer,
                               surcharge_comp$ieepa_baseline_layer, stack_comp$ieepa_baseline_layer),
    ieepa_topup_layer      = c(baseline_comp$ieepa_topup_layer, ieepa_comp$ieepa_topup_layer,
                               surcharge_comp$ieepa_topup_layer, stack_comp$ieepa_topup_layer),
    s232_layer             = c(baseline_comp$s232_layer, ieepa_comp$s232_layer,
                               surcharge_comp$s232_layer, stack_comp$s232_layer),
    emergency_layer        = c(baseline_comp$emergency_layer, ieepa_comp$emergency_layer,
                               surcharge_comp$emergency_layer, stack_comp$emergency_layer),
    s301_layer             = c(baseline_comp$s301_layer, ieepa_comp$s301_layer,
                               surcharge_comp$s301_layer, stack_comp$s301_layer),
    s122_layer             = c(baseline_comp$s122_layer, ieepa_comp$s122_layer,
                               surcharge_comp$s122_layer, stack_comp$s122_layer),
    total_rate             = c(baseline_comp$total_rate, ieepa_comp$total_rate,
                               surcharge_comp$total_rate, stack_comp$total_rate)
  )

  chart_data[, display_label := sprintf("%s\n(%.1f%%)", geography, total_rate)]
  chart_data[, display_label := factor(display_label, levels = display_label)]

  # Long format (vertical stacking, S122-aware)
  has_ieepa_topup <- any(chart_data$ieepa_topup_layer > 0.01, na.rm = TRUE)
  has_s122 <- any(chart_data$s122_layer > 0.01, na.rm = TRUE)

  chart_long <- chart_data %>%
    select(display_label, geography, iso_code, hts_layer, ieepa_baseline_layer,
           ieepa_topup_layer, s232_layer, emergency_layer, s301_layer, s122_layer, total_rate) %>%
    pivot_longer(
      cols = c(hts_layer, ieepa_baseline_layer, ieepa_topup_layer,
               s232_layer, emergency_layer, s301_layer, s122_layer),
      names_to = "layer",
      values_to = "rate"
    ) %>%
    mutate(
      layer = factor(layer,
                     levels = c("hts_layer", "ieepa_baseline_layer", "ieepa_topup_layer",
                                "s232_layer", "emergency_layer", "s301_layer", "s122_layer"),
                     labels = c("HTS Baseline", "IEEPA (10%)", "IEEPA Top-up",
                                "Section 232", "Emergency", "Section 301", "S122 (15%)"))
    )

  if (!has_ieepa_topup) {
    chart_long <- chart_long %>% filter(layer != "IEEPA Top-up")
  }
  if (!has_s122) {
    chart_long <- chart_long %>% filter(layer != "S122 (15%)")
  }

  chart_long <- chart_long %>%
    arrange(display_label, layer) %>%
    group_by(display_label) %>%
    mutate(
      cumsum_rate = cumsum(rate),
      label_pos   = cumsum_rate - rate / 2,
      rate_label  = ifelse(rate >= LABEL_MIN_PP, sprintf("%.1f%%", rate), "")
    ) %>%
    ungroup()

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = layer)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(y = label_pos, label = rate_label),
              color = "white", fontface = "bold", size = 3.0) +
    scale_fill_manual(values = LAYER_COLORS_S122, name = NULL, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Effective Tariff Rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.x    = element_text(size = 9, face = "bold", lineheight = 0.9),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))

  title_text <- sprintf(
    "Four scenarios: US tariffs\nrange from %.1f%% to %.1f%%",
    ieepa_comp$total_rate, surcharge_comp$total_rate
  )
  gta_wrap(
    p,
    title      = title_text,
    subtitle   = "Trade-weighted average, by tariff instrument",
    title_size = 15,
    chart_top  = 0.82
  )
}


# =============================================================================
# CHART S122-2: create_four_scenario_country_chart()
# =============================================================================
#' Create grouped horizontal bar chart comparing 4 scenarios by country.
#'
#' @param baseline_country data.table with country-level compositions (before SCOTUS)
#' @param ieepa_country data.table with country-level compositions (IEEPA strike-down)
#' @param surcharge_country data.table with country-level compositions (S122 surcharge)
#' @param stack_country data.table with country-level compositions (S122 stack)
#' @param top_n Number of countries to show (default 20, by import value)
#' @param include_global Include a Global aggregate bar (default TRUE)
#' @return cowplot ggdraw object (landscape)
create_four_scenario_country_chart <- function(baseline_country, ieepa_country,
                                               surcharge_country, stack_country,
                                               top_n = 20, include_global = TRUE) {

  # Merge all 4 scenarios
  merged <- merge(
    baseline_country[geography != "Global", .(geography, iso_code, total_imports_bn,
                                              rate_before = total_rate)],
    ieepa_country[geography != "Global", .(iso_code, rate_ieepa = total_rate)],
    by = "iso_code"
  )
  merged <- merge(merged,
    surcharge_country[geography != "Global", .(iso_code, rate_surcharge = total_rate)],
    by = "iso_code"
  )
  merged <- merge(merged,
    stack_country[geography != "Global", .(iso_code, rate_stack = total_rate)],
    by = "iso_code"
  )

  # Top N by import value
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  # Add Global row if requested
  if (include_global) {
    gb <- baseline_country[geography == "Global"]
    gi <- ieepa_country[geography == "Global"]
    gs <- surcharge_country[geography == "Global"]
    gk <- stack_country[geography == "Global"]
    if (nrow(gb) > 0 && nrow(gi) > 0 && nrow(gs) > 0 && nrow(gk) > 0) {
      global_row <- data.table(
        geography = "Global", iso_code = "WLD",
        total_imports_bn = gb$total_imports_bn,
        rate_before = gb$total_rate, rate_ieepa = gi$total_rate,
        rate_surcharge = gs$total_rate, rate_stack = gk$total_rate
      )
      merged <- rbind(global_row, merged)
    }
  }

  # Display labels
  merged[, display_label := geography]
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  # Pivot to long for grouped bars
  chart_long <- merged %>%
    select(display_label, rate_before, rate_ieepa, rate_surcharge, rate_stack) %>%
    pivot_longer(
      cols = c(rate_before, rate_ieepa, rate_surcharge, rate_stack),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = SCENARIO_LEVELS_4,
                        labels = SCENARIO_LABELS_4)
    )

  n_bars <- nrow(merged)
  label_size <- ifelse(n_bars > 15, 3.0, 3.5)

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.75) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.8),
      hjust = -0.1, size = label_size, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = SCENARIO_COLORS_4, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1))

  gta_wrap(
    p,
    title    = "Four scenarios: tariff rates for top 20 US import sources",
    subtitle = "Trade-weighted average tariff rate, top 20 countries by import value"
  )
}


# =============================================================================
# CHART S122-3: create_four_scenario_sector_chart()
# =============================================================================
#' Create grouped horizontal bar chart comparing 4 scenarios by HS-2 sector.
#'
#' @param baseline_hs2 data.table with HS-2 aggregations (before SCOTUS)
#' @param ieepa_hs2 data.table with HS-2 aggregations (IEEPA strike-down)
#' @param surcharge_hs2 data.table with HS-2 aggregations (S122 surcharge)
#' @param stack_hs2 data.table with HS-2 aggregations (S122 stack)
#' @param top_n Number of sectors to show (default 15, by import value)
#' @return cowplot ggdraw object (landscape)
create_four_scenario_sector_chart <- function(baseline_hs2, ieepa_hs2,
                                              surcharge_hs2, stack_hs2,
                                              top_n = 15) {

  # Merge all 4 scenarios
  merged <- merge(
    baseline_hs2[, .(hs2, chapter_label, total_imports_bn, rate_before = total_rate)],
    ieepa_hs2[, .(hs2, rate_ieepa = total_rate)],
    by = "hs2"
  )
  merged <- merge(merged,
    surcharge_hs2[, .(hs2, rate_surcharge = total_rate)],
    by = "hs2"
  )
  merged <- merge(merged,
    stack_hs2[, .(hs2, rate_stack = total_rate)],
    by = "hs2"
  )

  # Top N by import value
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  # Display labels
  merged[, display_label := chapter_label]
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  # Pivot to long for grouped bars
  chart_long <- merged %>%
    select(display_label, rate_before, rate_ieepa, rate_surcharge, rate_stack) %>%
    pivot_longer(
      cols = c(rate_before, rate_ieepa, rate_surcharge, rate_stack),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = SCENARIO_LEVELS_4,
                        labels = SCENARIO_LABELS_4)
    )

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.75) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.8),
      hjust = -0.1, size = 3.2, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = SCENARIO_COLORS_4, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 11, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1))

  gta_wrap(
    p,
    title    = "Four scenarios: tariff rates by sector",
    subtitle = "Trade-weighted average tariff rate by HS-2 chapter, top 15 sectors by import value"
  )
}


#' Mobile variant of the 4-scenario sector chart (portrait orientation).
create_four_scenario_sector_chart_mobile <- function(baseline_hs2, ieepa_hs2,
                                                     surcharge_hs2, stack_hs2,
                                                     top_n = 15) {

  merged <- merge(
    baseline_hs2[, .(hs2, chapter_label, total_imports_bn, rate_before = total_rate)],
    ieepa_hs2[, .(hs2, rate_ieepa = total_rate)],
    by = "hs2"
  )
  merged <- merge(merged,
    surcharge_hs2[, .(hs2, rate_surcharge = total_rate)],
    by = "hs2"
  )
  merged <- merge(merged,
    stack_hs2[, .(hs2, rate_stack = total_rate)],
    by = "hs2"
  )
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  merged[, display_label := chapter_label]
  merged <- merged[order(rate_before)]
  merged[, display_label := factor(display_label, levels = display_label)]

  chart_long <- merged %>%
    select(display_label, rate_before, rate_ieepa, rate_surcharge, rate_stack) %>%
    pivot_longer(
      cols = c(rate_before, rate_ieepa, rate_surcharge, rate_stack),
      names_to = "scenario",
      values_to = "rate"
    ) %>%
    mutate(
      scenario = factor(scenario,
                        levels = SCENARIO_LEVELS_4,
                        labels = SCENARIO_LABELS_4)
    )

  p <- ggplot(chart_long, aes(x = display_label, y = rate, fill = scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.75) +
    geom_text(
      aes(label = sprintf("%.1f%%", rate)),
      position = position_dodge(width = 0.8),
      hjust = -0.1, size = 2.5, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(values = SCENARIO_COLORS_4, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                       labels = function(x) paste0(x, "%")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_gta_inner +
    theme(
      axis.text.y  = element_text(size = 8, face = "bold"),
      axis.text.x  = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 2))

  gta_wrap(
    p,
    title      = "Four scenarios: tariff rates\nby sector",
    subtitle   = "Trade-weighted average by HS-2 chapter",
    title_size = 15,
    chart_top  = 0.82
  )
}


# =============================================================================
# CHART S122-4: create_surcharge_vs_stack_chart()
# =============================================================================
#' Create stacked horizontal bar chart decomposing the surcharge scenario into
#' three components per country:
#'   1. IEEPA Strike Down rate (base after SCOTUS)
#'   2. S122 Stack increment (stack rate - strike-down rate)
#'   3. Surcharge increment (surcharge rate - stack rate)
#' Together these three sum to the surcharge scenario total.
#'
#' @param ieepa_country data.table with country-level compositions (IEEPA strike-down)
#' @param stack_country data.table with country-level compositions (S122 stack)
#' @param surcharge_country data.table with country-level compositions (S122 surcharge)
#' @param top_n Number of countries to show (default 20, by import value)
#' @param exclude_geo Character vector of geography names to exclude
#' @return cowplot ggdraw object (landscape)
create_surcharge_vs_stack_chart <- function(ieepa_country, stack_country,
                                            surcharge_country,
                                            top_n = 20,
                                            exclude_geo = c("Gaza Strip")) {

  # Merge all three scenarios
  merged <- merge(
    ieepa_country[geography != "Global" & iso_code != "EU",
                  .(geography, iso_code, total_imports_bn,
                    rate_ieepa = total_rate)],
    stack_country[geography != "Global" & iso_code != "EU",
                  .(iso_code, rate_stack = total_rate)],
    by = "iso_code"
  )
  merged <- merge(merged,
    surcharge_country[geography != "Global" & iso_code != "EU",
                      .(iso_code, rate_surcharge = total_rate)],
    by = "iso_code"
  )

  # Compute three stacked components
  merged[, `:=`(
    base_ieepa      = rate_ieepa,
    increment_stack  = pmax(0, rate_stack - rate_ieepa),
    increment_surcharge = pmax(0, rate_surcharge - rate_stack)
  )]

  # Exclude specified geographies
  if (length(exclude_geo) > 0) {
    merged <- merged[!geography %in% exclude_geo]
  }

  # Top N by import value
  merged <- merged[order(-total_imports_bn)][seq_len(min(top_n, nrow(merged)))]

  # Display label with import value
  merged[, display_label := sprintf("%s ($%.1f)", geography, total_imports_bn)]
  # Order by surcharge rate (descending, so highest at top in coord_flip)
  merged <- merged[order(rate_surcharge)]
  merged[, display_label := factor(display_label, levels = display_label)]

  # Pivot to long for stacking
  plot_dt <- melt(merged,
                  id.vars = c("display_label", "rate_surcharge"),
                  measure.vars = c("base_ieepa", "increment_stack", "increment_surcharge"),
                  variable.name = "component", value.name = "value")

  # Factor ordering: base first (leftmost), then stack, then surcharge
  component_levels <- c("base_ieepa", "increment_stack", "increment_surcharge")
  component_labels <- c("IEEPA Strike Down (base)", "+ S122 Stack", "+ Surcharge top-up")
  component_colors <- c("base_ieepa" = "#003366",
                         "increment_stack" = "#2CA58D",
                         "increment_surcharge" = "#F5A623")
  plot_dt[, component := factor(component, levels = component_levels)]

  p <- ggplot(plot_dt, aes(x = display_label, y = value, fill = component)) +
    geom_bar(stat = "identity", width = 0.65) +
    # Total label at bar end
    geom_text(
      data = merged,
      aes(x = display_label, y = rate_surcharge, fill = NULL,
          label = sprintf("%.1f%%", rate_surcharge)),
      hjust = -0.12, size = 3.8, fontface = "bold", color = "grey30"
    ) +
    scale_fill_manual(
      values = component_colors,
      labels = component_labels,
      name   = NULL
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.15))
    ) +
    coord_flip() +
    labs(x = NULL, y = "Trade-weighted average tariff rate (%)") +
    theme_gta_inner +
    theme(
      axis.text.y     = element_text(size = 10, face = "bold"),
      axis.text.x     = element_text(size = 9),
      legend.position = "bottom",
      legend.text     = element_text(size = 10),
      legend.key.size = unit(0.5, "cm")
    )

  gta_wrap(
    p,
    title    = "How the surcharge scenario builds on the strike-down base",
    subtitle = "Stacked components: post-SCOTUS base + S122 stack increment + surcharge top-up. Top 20 by import value."
  )
}


cat("   SCOTUS chart functions loaded.\n")
