# =============================================================================
# US Tariff Analysis - Country Group Analysis Function
# Flexible function to analyze tariff impacts for any specified country group
# =============================================================================

# Load necessary libraries
library(data.table)
library(dplyr)
library(writexl)
library(ggplot2)
library(scales)
library(extrafont)
library(stringr)

# Output directory (set by update_processing.R, or standalone default)
if (!exists("RUN_OUTPUT_DIR")) {
  source("code/date_config.R")
  RUN_OUTPUT_DIR <- "results/standalone"
}

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

analyze_country_group <- function(group_name, group_countries, group_display_name = NULL) {
  
  # Get date configuration (already loaded at top of script or by update_processing.R)
  POLICY_DATE_LONG <- get("POLICY_DATE_LONG", envir = globalenv())
  UPDATE_DATE_LONG <- get("UPDATE_DATE_LONG", envir = globalenv())
  
  # Set default display name if not provided
  if (is.null(group_display_name)) {
    group_display_name <- group_name
  }
  
  cat(sprintf("=== US Tariff Analysis - %s Countries ===\n", group_display_name))
  cat(sprintf("Analyzing tariff impacts for %s countries...\n\n", group_display_name))
  
  # =============================================================================
  # 1. DEFINE COUNTRIES AND LOAD DATA
  # =============================================================================
  
  cat("1. Defining countries and loading data...\n")
  
  cat(sprintf("   %s countries defined: %d countries\n", group_display_name, length(group_countries)))
  
  # Load processed data (assuming it's already loaded in global environment)
  if (!exists("us_imports")) {
    data_file <- file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData"))
    load(data_file)
    # Ensure us_imports is a data.table (may lose class when loaded from RData)
    if (!is.data.table(us_imports)) {
      setDT(us_imports)
    }
    cat(sprintf("   Loaded %s import flows\n", format(nrow(us_imports), big.mark = ",")))
  }

  # Filter for specified countries only
  us_imports_group <- us_imports[exporter %in% group_countries]
  
  cat(sprintf("   Filtered to %s flows from %s countries\n", 
              format(nrow(us_imports_group), big.mark = ","), group_display_name))
  cat(sprintf("   %s countries found in data: %d\n", 
              group_display_name, length(unique(us_imports_group$exporter))))
  
  # Check which countries are missing from the data
  missing_countries <- setdiff(group_countries, unique(us_imports_group$exporter))
  if (length(missing_countries) > 0) {
    cat("   Countries not found in data:", paste(missing_countries, collapse = ", "), "\n")
  }
  
  # Show which countries were found
  found_countries <- intersect(group_countries, unique(us_imports_group$exporter))
  cat(sprintf("   %s countries found in data: %s\n", 
              group_display_name, paste(found_countries, collapse = ", ")))
  
  # =============================================================================
  # 2. CREATE OUTPUT DIRECTORY
  # =============================================================================
  
  cat("\n2. Creating output directories...\n")

  output_dir <- file.path(RUN_OUTPUT_DIR, "countries", group_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cat(sprintf("   Created %s directory\n", output_dir))
  
  # =============================================================================
  # 3. EFFECTIVE RATES ANALYSIS
  # =============================================================================
  
  cat("\n3. Creating effective rates analysis...\n")

  # Calculate effective rates
  us_imports_group[, effective_rate := rate]
  
  # Calculate trade-weighted average rates by exporter
  country_analysis_group <- us_imports_group[, .(
    total_imports_bn = sum(us_imports_bn),
    weighted_tariff_revenue = sum(effective_rate * us_imports_bn / 100),
    total_tariff_points = sum(effective_rate * us_imports_bn),
    total_value_for_weighting = sum(us_imports_bn),
    iso2 = unique(iso2)[1],
    un_code = unique(un_code)[1]
  ), by = .(exporter)]
  
  # Apply ISO2 code fixes (can be extended as needed)
  apply_iso2_fixes <- function(dt) {
    dt[exporter == "North Macedonia", iso2 := "MK"]
    dt[exporter == "Kosovo", iso2 := "XK"]
    dt[exporter == "Namibia", iso2 := "NA"]
    dt[exporter == "Congo, Dem. Rep.", iso2 := "CD"]
    dt[exporter == "Lao PDR", iso2 := "LA"]
    dt[exporter == "Tanzania", iso2 := "TZ"]
    dt[exporter == "Sudan", iso2 := "SD"]
    return(dt)
  }
  
  country_analysis_group <- apply_iso2_fixes(country_analysis_group)
  
  # Calculate trade-weighted average rates
  country_analysis_group[, avg_rate := total_tariff_points / total_value_for_weighting]
  
  # Create final output
  country_summary_group <- country_analysis_group[, .(
    exporter = exporter,
    iso2 = iso2,
    total_value = total_imports_bn,
    avg_rate = round(avg_rate, 2)
  )][order(-total_value)]
  
  cat(sprintf("   Calculated rates for %d %s countries\n", 
              nrow(country_summary_group), group_display_name))
  
  # Handle duplicate ISO codes
  country_summary_group[, iso2 := as.character(iso2)]
  
  if (any(duplicated(country_summary_group$iso2))) {
    cat("   Warning: Duplicate ISO2 codes found. Adding country names to make unique...\n")
    country_summary_group[, iso2_display := ifelse(duplicated(iso2) | duplicated(iso2, fromLast = TRUE),
                                                  paste0(iso2, "-", substr(exporter, 1, 3)),
                                                  iso2)]
  } else {
    country_summary_group[, iso2_display := iso2]
  }
  
  # Chart 1: Effective Rates
  chart1_data <- country_summary_group %>%
    arrange(avg_rate) %>%
    mutate(iso2_ordered = factor(iso2_display, levels = unique(iso2_display)))
  
  p1 <- ggplot(chart1_data, aes(x = iso2_ordered, y = avg_rate)) +
    geom_bar(stat = "identity", fill = "#2171C2", width = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", avg_rate)), 
              hjust = -0.1,
              color = "#333333",
              family = "sans", 
              size = 6,
              fontface = "bold") +
    labs(
      title = sprintf("The New US Tariff Environment - %s Countries", group_display_name),
      subtitle = sprintf("Trade-weighted average tariff by country,\neffective %s", POLICY_DATE_LONG),
      x = "",
      y = "",
      caption = "Source: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 28, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14, color = "#333333", face = "bold"),
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
  
  # Save Chart 1
  chart1_path <- file.path(output_dir, sprintf("effective_rates_%s.png", group_name))
  ggsave(chart1_path, plot = p1, width = 10, height = 10, 
         dpi = 300, bg = "white", units = "in", limitsize = FALSE)
  
  cat(sprintf("   Saved effective rates chart: %s\n", chart1_path))
  
  # Export effective rates table
  excel1_path <- file.path(output_dir, sprintf("effective_rates_%s.xlsx", group_name))
  effective_rates_sheet <- list()
  effective_rates_sheet[[sprintf("%s Effective Rates", group_display_name)]] <- country_summary_group %>%
        rename(
          "Exporter" = exporter,
          "ISO2 Code" = iso2,
          "Total Import Value (bn USD)" = total_value,
          "Trade-Weighted Avg Rate (%)" = avg_rate
        ) %>%
        arrange(desc(`Trade-Weighted Avg Rate (%)`))
  
  write_xlsx(effective_rates_sheet, path = excel1_path)
  
  cat(sprintf("   Saved effective rates table: %s\n", excel1_path))
  
  # =============================================================================
  # 4. RELATIVE ADVANTAGE ANALYSIS
  # =============================================================================
  
  cat("\n4. Creating relative advantage analysis...\n")
  
  # Calculate total imports and tariff-weighted totals by HS code (using full dataset for competitors)
  hs_totals <- us_imports[, .(
    total_imports_by_hs = sum(us_imports_bn),
    total_tariff_weighted_by_hs = sum(rate * us_imports_bn)
  ), by = hs_8digit]
  
  # Merge back to group data
  us_imports_group <- merge(us_imports_group, hs_totals, by = "hs_8digit", all.x = TRUE)
  
  # Calculate competitor weighted average rate (excluding own country)
  us_imports_group[, competitor_rate := ifelse(
    total_imports_by_hs - us_imports_bn > 0,
    (total_tariff_weighted_by_hs - effective_rate * us_imports_bn) / (total_imports_by_hs - us_imports_bn),
    NA_real_
  )]
  
  # Calculate tariff advantage
  us_imports_group[, tariff_advantage := competitor_rate - effective_rate]
  
  # Create relative advantage dataset
  relative_advantage_group <- us_imports_group[!is.na(competitor_rate), .(
    exporter = exporter,
    iso2 = iso2,
    hs_8digit = hs_8digit,
    hs_8digit_name = hs_8digit_name,
    own_rate = round(effective_rate, 2),
    competitor_rate = round(competitor_rate, 2),
    tariff_advantage = round(tariff_advantage, 2),
    import_value_bn = us_imports_bn,
    un_code = un_code
  )][order(-import_value_bn)]
  
  # Apply ISO2 fixes to relative advantage data
  relative_advantage_group <- apply_iso2_fixes(relative_advantage_group)
  
  # Create country-level summary
  country_advantage_summary_group <- relative_advantage_group[, .(
    total_import_value_bn = sum(import_value_bn),
    flow_count = .N,
    avg_own_rate = round(weighted.mean(own_rate, import_value_bn), 2),
    avg_competitor_rate = round(weighted.mean(competitor_rate, import_value_bn), 2),
    avg_tariff_advantage = round(weighted.mean(tariff_advantage, import_value_bn), 2),
    flows_with_advantage = sum(tariff_advantage > 0),
    flows_with_disadvantage = sum(tariff_advantage < 0),
    advantage_share = round(100 * sum(tariff_advantage > 0) / .N, 1)
  ), by = .(exporter, iso2, un_code)][order(-total_import_value_bn)]
  
  # Apply ISO2 fixes to summary data
  country_advantage_summary_group <- apply_iso2_fixes(country_advantage_summary_group)
  
  cat(sprintf("   Calculated relative advantage for %d %s countries\n", 
              nrow(country_advantage_summary_group), group_display_name))
  
  # Handle duplicate ISO codes
  country_advantage_summary_group[, iso2 := as.character(iso2)]
  
  if (any(duplicated(country_advantage_summary_group$iso2))) {
    cat("   Warning: Duplicate ISO2 codes found. Adding country names to make unique...\n")
    country_advantage_summary_group[, iso2_display := ifelse(duplicated(iso2) | duplicated(iso2, fromLast = TRUE),
                                                            paste0(iso2, "-", substr(exporter, 1, 3)),
                                                            iso2)]
  } else {
    country_advantage_summary_group[, iso2_display := iso2]
  }
  
  # Chart 2: Relative Advantage
  chart2_data <- country_advantage_summary_group %>%
    arrange(avg_tariff_advantage) %>%
    mutate(iso2_ordered = factor(iso2_display, levels = unique(iso2_display)))
  
  p2 <- ggplot(chart2_data, aes(x = iso2_ordered, y = avg_tariff_advantage)) +
    geom_bar(stat = "identity", width = 0.8,
             fill = ifelse(chart2_data$avg_tariff_advantage >= 0, "#28A745", "#DC3545")) +
    geom_text(aes(label = sprintf("%.1f%%", avg_tariff_advantage)), 
              hjust = ifelse(chart2_data$avg_tariff_advantage < 0, 1.1, -0.1),
              color = "#333333",
              family = "sans", 
              size = 5,
              fontface = "bold") +
    geom_hline(yintercept = 0, color = "#333333", size = 1, linetype = "solid") +
    labs(
      title = sprintf("Relative 'Trump Tariff Advantage'\n%s Overview", group_display_name),
      subtitle = sprintf("Trade-weighted average own tariff vs. competitors,\neffective %s", POLICY_DATE_LONG),
      x = "",
      y = "",
      caption = "Green = Advantage, Red = Disadvantage\nSource: Global Trade Alert,\nUSITC DataWeb (2024 imports, HS 8-digit level)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 28, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14, color = "#333333", face = "bold"),
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
  
  # Save Chart 2
  chart2_path <- file.path(output_dir, sprintf("relative_advantage_%s.png", group_name))
  ggsave(chart2_path, plot = p2, width = 10, height = 10, 
         dpi = 300, bg = "white", units = "in", limitsize = FALSE)
  
  cat(sprintf("   Saved relative advantage chart: %s\n", chart2_path))
  
  # Export relative advantage table
  excel2_path <- file.path(output_dir, sprintf("relative_advantage_%s.xlsx", group_name))
  relative_advantage_sheet <- list()
  relative_advantage_sheet[[sprintf("%s Relative Advantage", group_display_name)]] <- country_advantage_summary_group %>%
        rename(
          "Exporter" = exporter,
          "ISO2 Code" = iso2,
          "UN Code" = un_code,
          "Total Import Value (bn USD)" = total_import_value_bn,
          "Number of Products" = flow_count,
          "Avg Own Rate (%)" = avg_own_rate,
          "Avg Competitor Rate (%)" = avg_competitor_rate,
          "Avg Tariff Advantage (%)" = avg_tariff_advantage,
          "Products with Advantage" = flows_with_advantage,
          "Products with Disadvantage" = flows_with_disadvantage,
          "Advantage Share (%)" = advantage_share
        )
  
  write_xlsx(relative_advantage_sheet, path = excel2_path)
  
  cat(sprintf("   Saved relative advantage table: %s\n", excel2_path))
  
  # =============================================================================
  # 5. HYPOTHETICAL TARIFF REVENUE ANALYSIS
  # =============================================================================
  
  cat("\n5. Creating hypothetical tariff revenue analysis...\n")
  
  # Handle duplicate ISO codes for revenue chart
  country_analysis_group[, iso2 := as.character(iso2)]
  
  if (any(duplicated(country_analysis_group$iso2))) {
    cat("   Warning: Duplicate ISO2 codes found. Adding country names to make unique...\n")
    country_analysis_group[, iso2_display := ifelse(duplicated(iso2) | duplicated(iso2, fromLast = TRUE),
                                                   paste0(iso2, "-", substr(exporter, 1, 3)),
                                                   iso2)]
  } else {
    country_analysis_group[, iso2_display := iso2]
  }
  
  # Chart 3: Hypothetical Tariff Revenue
  revenue_chart_data <- country_analysis_group %>%
    arrange(weighted_tariff_revenue) %>%
    mutate(iso2_ordered = factor(iso2_display, levels = unique(iso2_display)))
  
  p3 <- ggplot(revenue_chart_data, aes(x = iso2_ordered, y = weighted_tariff_revenue)) +
    geom_bar(stat = "identity", fill = "#2171C2", width = 0.8) +
    geom_text(aes(label = sprintf("%.1f", weighted_tariff_revenue)), 
              hjust = -0.1,
              color = "#333333",
              family = "sans", 
              size = 5,
              fontface = "bold") +
    labs(
      title = sprintf("Hypothetical New U.S. Tariff Revenue - %s Countries", group_display_name),
      subtitle = "By Import Origin (in USD bn)\n(ignoring supply/demand adjustments)",
      x = "",
      y = "",
      caption = "Source: Global Trade Alert, author's calculations using USITC DataWeb import statistics.\nRevenue calculated using additional tariff x 2024 imports."
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(size = 24, face = "bold", color = "#2171C2", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, color = "#666666", margin = margin(b = 20)),
      plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 15), color = "#666666"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14, color = "#333333", face = "bold"),
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
  
  # Save Chart 3
  chart3_path <- file.path(output_dir, sprintf("hypothetical_tariff_revenue_%s.png", group_name))
  ggsave(chart3_path, plot = p3, width = 10, height = 10, 
         dpi = 300, bg = "white", units = "in", limitsize = FALSE)
  
  cat(sprintf("   Saved tariff revenue chart: %s\n", chart3_path))
  
  # Export tariff revenue table
  excel3_path <- file.path(output_dir, sprintf("hypothetical_tariff_revenue_%s.xlsx", group_name))
  tariff_revenue_sheet <- list()
  tariff_revenue_sheet[[sprintf("%s Tariff Revenue", group_display_name)]] <- country_analysis_group %>%
        select(exporter, iso2, total_imports_bn, weighted_tariff_revenue, avg_rate) %>%
        rename(
          "Exporter" = exporter,
          "ISO2 Code" = iso2,
          "Total Imports 2024 (bn USD)" = total_imports_bn,
          "Hypothetical Tariff Revenue (bn USD)" = weighted_tariff_revenue,
          "Avg Effective Rate (%)" = avg_rate
        ) %>%
        arrange(desc(`Hypothetical Tariff Revenue (bn USD)`))
  
  write_xlsx(tariff_revenue_sheet, path = excel3_path)
  
  cat(sprintf("   Saved tariff revenue table: %s\n", excel3_path))
  
  # =============================================================================
  # 6. INDIVIDUAL COUNTRY ANALYSIS
  # =============================================================================
  
  cat("\n6. Creating individual country analysis...\n")
  
  # Load HS code names for better chart labels
  if (!exists("hs_names")) {
    hs_names <- fread("data/us_hts-8_digit_sectioned.csv", sep = ";", select = c("hs_8digit", "hs_8name"))
    hs_names[, hs_8digit := sprintf("%08s", hs_8digit)]
  }
  
  # Function to create abbreviated product labels
  create_product_label <- function(hs_code, hs_name) {
    if (hs_code == "Country Average") {
      return("Country Average")
    }
    
    if (is.na(hs_name) || is.null(hs_name)) {
      return(paste0("Product\n(", hs_code, ")"))
    }
    
    product_name <- trimws(hs_name)
    
    if (is.na(product_name) || nchar(product_name) == 0) {
      return(paste0("Product\n(", hs_code, ")"))
    }
    
    max_chars_line1 <- 25
    max_chars_line2 <- 25
    
    words <- strsplit(product_name, "\\s+")[[1]]
    
    if (length(words) == 0) {
      return(paste0("Product\n(", hs_code, ")"))
    }
    
    line1 <- ""
    line2 <- ""
    current_line <- 1
    
    for (word in words) {
      if (is.na(word) || nchar(word) == 0) next
      
      test_line1 <- if (line1 == "") word else paste(line1, word)
      test_line2 <- if (line2 == "") word else paste(line2, word)
      
      if (current_line == 1) {
        if (!is.na(test_line1) && nchar(test_line1) <= max_chars_line1) {
          line1 <- test_line1
        } else {
          current_line <- 2
          line2 <- word
        }
      } else if (current_line == 2) {
        if (!is.na(test_line2) && nchar(test_line2) <= max_chars_line2) {
          line2 <- test_line2
        } else {
          line2 <- paste(line2, "...")
          break
        }
      }
    }
    
    hs_display <- ifelse(hs_code == "Country Average", "", sprintf("%08s", hs_code))
    
    if (line2 == "") {
      return(paste0(line1, "\n(HS ", as.character(hs_display), ")"))
    } else {
      return(paste0(line1, "\n", line2, "\n(HS ", as.character(hs_display), ")"))
    }
  }
  
  # Get list of countries that have data, sorted by total import value
  country_totals <- relative_advantage_group[, .(
    total_imports = sum(import_value_bn)
  ), by = exporter][order(-total_imports)]
  
  cat(sprintf("   Processing %d %s countries individually...\n", 
              nrow(country_totals), group_display_name))
  
  # Loop through each country
  for (i in seq_len(nrow(country_totals))) {
    
    country_name <- country_totals$exporter[i]
    
    # Clean country name for file naming
    clean_country <- gsub("[^A-Za-z0-9 ]", "", country_name)
    clean_country <- gsub("\\s+", "_", clean_country)
    clean_country <- substr(clean_country, 1, 50)
    
    cat(sprintf("   Processing %d/%d: %s\n", i, nrow(country_totals), country_name))
    
    # Filter data for this country
    country_data <- relative_advantage_group[exporter == country_name]
    
    if (nrow(country_data) == 0) {
      cat(sprintf("     No data available for %s, skipping...\n", country_name))
      next
    }
    
    # Create aggregate summary for this country
    country_summary <- country_data[, .(
      exporter = unique(exporter),
      iso2 = unique(iso2)[1],
      un_code = unique(un_code)[1],
      total_import_value_bn = sum(import_value_bn),
      flow_count = .N,
      avg_own_rate = round(weighted.mean(own_rate, import_value_bn), 2),
      avg_competitor_rate = round(weighted.mean(competitor_rate, import_value_bn), 2),
      avg_tariff_advantage = round(weighted.mean(tariff_advantage, import_value_bn), 2),
      flows_with_advantage = sum(tariff_advantage > 0),
      flows_with_disadvantage = sum(tariff_advantage < 0),
      advantage_share = round(100 * sum(tariff_advantage > 0) / .N, 1)
    )]
    
    # Order product-level data by import value
    country_products <- country_data[order(-import_value_bn)]
    
    # Create Excel file
    excel_filename <- file.path(output_dir, sprintf("%s_tariff_advantage.xlsx", clean_country))
    
    write_xlsx(
      list(
        "Country Summary" = country_summary %>%
          rename(
            "Exporter" = exporter,
            "ISO2 Code" = iso2,
            "UN Code" = un_code,
            "Total Import Value (bn USD)" = total_import_value_bn,
            "Number of Products" = flow_count,
            "Avg Own Rate (%)" = avg_own_rate,
            "Avg Competitor Rate (%)" = avg_competitor_rate,
            "Avg Tariff Advantage (%)" = avg_tariff_advantage,
            "Products with Advantage" = flows_with_advantage,
            "Products with Disadvantage" = flows_with_disadvantage,
            "Advantage Share (%)" = advantage_share
          ),
        "Product Level Detail" = country_products %>%
          select(hs_8digit, hs_8digit_name, own_rate, competitor_rate, 
                 tariff_advantage, import_value_bn) %>%
          rename(
            "HS 8-Digit Code" = hs_8digit,
            "Product Description" = hs_8digit_name,
            "Own Rate (%)" = own_rate,
            "Competitor Avg Rate (%)" = competitor_rate,
            "Tariff Advantage (%)" = tariff_advantage,
            "Import Value (bn USD)" = import_value_bn
          )
      ),
      path = excel_filename
    )
    
    # Create top 10 products chart
    top10_products <- head(country_products, 10)
    
    if (nrow(top10_products) > 0) {
      
      # Add country average bar
      avg_bar <- data.frame(
        hs_8digit = "Country Average",
        tariff_advantage = country_summary$avg_tariff_advantage,
        import_value_bn = 0
      )
      
      # Combine and sort data
      combined_data <- rbind(
        top10_products %>% select(hs_8digit, tariff_advantage, import_value_bn),
        avg_bar
      )
      
      combined_data <- combined_data %>%
        arrange(tariff_advantage)
      
      # Format HS codes
      combined_data$hs_8digit <- ifelse(combined_data$hs_8digit == "Country Average", 
                                       "Country Average", 
                                       sprintf("%08s", combined_data$hs_8digit))
      
      # Merge with HS names
      combined_data <- merge(combined_data, hs_names, by = "hs_8digit", all.x = TRUE)
      combined_data$hs_8name[combined_data$hs_8digit == "Country Average"] <- "Country Average"
      
      # Create product labels
      combined_data$product_label <- mapply(create_product_label, 
                                           combined_data$hs_8digit, 
                                           combined_data$hs_8name)
      
      # Reorder for chart
      combined_data <- combined_data[order(combined_data$tariff_advantage), ]
      combined_data$product_label_ordered <- factor(combined_data$product_label, levels = combined_data$product_label)
      
      # Create chart
      p <- ggplot(combined_data, aes(x = product_label_ordered, y = tariff_advantage)) +
        geom_bar(stat = "identity", width = 0.7,
                 fill = ifelse(combined_data$hs_8digit == "Country Average", "#2171C2",
                             ifelse(combined_data$tariff_advantage >= 0, "#28A745", "#DC3545"))) +
        geom_text(aes(label = ifelse(hs_8digit == "Country Average", 
                                    sprintf("Avg: %.1f%%", tariff_advantage),
                                    sprintf("%.1f%%", tariff_advantage))), 
                  hjust = ifelse(combined_data$tariff_advantage < 0, 1.1, -0.1),
                  color = "#333333",
                  family = "sans", 
                  size = 5,
                  fontface = "bold") +
        geom_hline(yintercept = 0, color = "#333333", size = 1, linetype = "solid") +
        labs(
          title = sprintf("Relative 'Trump Tariff Advantage' for\n%s's top 10 US Exports", country_name),
          subtitle = sprintf("Trade-weighted average own tariff vs. competitors,\neffective %s", POLICY_DATE_LONG),
          x = "",
          y = "Products",
          caption = "Blue = Country Average, Green = Advantage, Red = Disadvantage\nSource: Global Trade Alert, USITC DataWeb (2024 imports, HS 8-digit level)"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(size = 24, face = "bold", color = "#2171C2", margin = margin(b = 10)),
          plot.subtitle = element_text(size = 16, color = "#666666", margin = margin(b = 20)),
          plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 15), color = "#666666"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14, color = "#333333", face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10, color = "#333333", face = "bold", lineheight = 0.8),
          plot.margin = margin(t = 20, r = 60, b = 20, l = 120),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          axis.ticks = element_blank()
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0.15, 0.15))
        ) +
        coord_flip()
      
      # Save chart
      chart_filename <- file.path(output_dir, sprintf("%s_top10_products.png", clean_country))
      ggsave(chart_filename, plot = p, width = 12, height = 8, 
             dpi = 300, bg = "white", units = "in", limitsize = FALSE)
    }
  }
  
  # =============================================================================
  # 7. SUMMARY
  # =============================================================================
  
  cat(sprintf("\n=== %s ANALYSIS COMPLETE ===\n", toupper(group_display_name)))
  cat(sprintf("Processed %d %s countries\n", nrow(country_totals), group_display_name))
  cat(sprintf("Results saved in: %s\n", output_dir))
  cat(sprintf("  - Effective rates chart: effective_rates_%s.png\n", group_name))
  cat(sprintf("  - Effective rates table: effective_rates_%s.xlsx\n", group_name))
  cat(sprintf("  - Relative advantage chart: relative_advantage_%s.png\n", group_name))
  cat(sprintf("  - Relative advantage table: relative_advantage_%s.xlsx\n", group_name))
  cat(sprintf("  - Tariff revenue chart: hypothetical_tariff_revenue_%s.png\n", group_name))
  cat(sprintf("  - Tariff revenue table: hypothetical_tariff_revenue_%s.xlsx\n", group_name))
  cat("  - Individual country Excel files: [CountryName]_tariff_advantage.xlsx\n")
  cat("  - Individual country charts: [CountryName]_top10_products.png\n")
  
  # Return summary data for further use if needed
  return(list(
    country_summary = country_summary_group,
    advantage_summary = country_advantage_summary_group,
    revenue_summary = country_analysis_group
  ))
}

# =============================================================================
# EXAMPLE USAGE SCRIPT
# =============================================================================

# Clear environment
# rm(list = ls())  # Commented out - environment cleared by update_processing.R
options(scipen = 999)

# Define country groups
c4tp_countries <- c(
  "Albania", "Azerbaijan", "Bangladesh", "Brazil", "Cambodia", "Chile", 
  "Colombia", "Costa Rica", "Dominican Republic", "Egypt", "El Salvador", 
  "Eswatini, Swaziland", "Kazakhstan", "Kosovo", "Mexico", "Mongolia", "Morocco", 
  "Montenegro", "Mozambique", "Namibia", "Nepal", "North Macedonia", 
  "Pakistan", "Panama", "Peru", "Philippines", "Serbia", "Singapore", 
  "Solomon Islands", "South Africa", "Thailand", "Tunisia", "United Arab Emirates", 
  "Ukraine", "Uruguay", "Vietnam", "Zambia"
)

ldc_countries <- c(
  "Afghanistan", "Angola", "Bangladesh", "Benin", "Burkina Faso", "Burundi", 
  "Cambodia", "Central African Republic", "Chad", "Comoros", 
  "Congo, Dem. Rep.", "Djibouti", "Eritrea", "Ethiopia", 
  "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", 
  "Lao PDR", "Lesotho", "Liberia", "Madagascar", 
  "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar", "Nepal", 
  "Niger", "Rwanda", "Senegal", "Sierra Leone", "Solomon Islands", 
  "Somalia", "South Sudan", "Sudan", "Timor-Leste", "Togo", "Tuvalu", 
  "Uganda", "Tanzania", "Yemen", "Zambia"
)

# Load the data once
data_file <- file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData"))
load(data_file)
# Ensure us_imports is a data.table (may lose class when loaded from RData)
if (!is.data.table(us_imports)) {
  setDT(us_imports)
}

# Run analysis for both groups
cat("Starting C4TP analysis...\n")
c4tp_results <- analyze_country_group("c4tp", c4tp_countries, "C4TP")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")

cat("Starting LDC analysis...\n")
ldc_results <- analyze_country_group("ldc", ldc_countries, "LDC")

cat("\nAll analyses complete!\n")