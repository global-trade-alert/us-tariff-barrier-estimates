# =============================================================================
# US Tariff Analysis - Country-Specific Relative Tariff Advantage Analysis
# Creates individual analysis for each import origin country
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

# Clear environment (preserve date variables and output config from update_processing.R)
rm(list = setdiff(ls(), c("POLICY_DATE", "POLICY_DATE_LONG", "POLICY_DATE_SHORT",
                          "UPDATE_DATE", "UPDATE_DATE_LONG", "UPDATE_DATE_SHORT",
                          "RUN_OUTPUT_DIR", "OUTPUT_BASENAME", "MFN_SUFFIX")))
options(scipen = 999)

# Set default date values if not provided by update_processing.R
# Load centralized date configuration
if (!exists("POLICY_DATE_LONG")) {
  source("code/date_config.R")
}
cat("=== US Tariff Analysis - Country-Specific Relative Advantage ===\n")
cat("Creating individual analysis for each import origin...\n\n")

# =============================================================================
# 1. LOAD PROCESSED DATA
# =============================================================================

cat("1. Loading processed tariff data...\n")
if (!exists("us_imports")) {
  load(file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData")))
}

# Ensure us_imports is a data.table (may lose class when loaded from RData)
if (!is.data.table(us_imports)) {
  setDT(us_imports)
}

# Load HS code names for better chart labels
hs_names <- fread("data/us_hts-8_digit_sectioned.csv", sep = ";", select = c("hs_8digit", "hs_8name"))

# Ensure HS codes are formatted consistently for merging (8-digit with leading zeros)
hs_names[, hs_8digit := sprintf("%08s", hs_8digit)]

# Function to create abbreviated product labels (2 lines + ellipsis + HS code in brackets)
create_product_label <- function(hs_code, hs_name) {
  # Handle special case for country average
  if (hs_code == "Country Average") {
    return("Country Average")
  }
  
  # Handle NA product names
  if (is.na(hs_name) || is.null(hs_name)) {
    return(paste0("Product\n(", hs_code, ")"))
  }
  
  # Clean and prepare the product name
  product_name <- trimws(hs_name)
  
  # Handle empty or very short product names
  if (is.na(product_name) || nchar(product_name) == 0) {
    return(paste0("Product\n(", hs_code, ")"))
  }
  
  # Define maximum characters per line (adjust based on chart width)
  max_chars_line1 <- 25
  max_chars_line2 <- 25
  
  # Split into words
  words <- strsplit(product_name, "\\s+")[[1]]
  
  # Handle case with no words
  if (length(words) == 0) {
    return(paste0("Product\n(", hs_code, ")"))
  }
  
  # Build lines
  line1 <- ""
  line2 <- ""
  current_line <- 1
  
  for (word in words) {
    # Skip NA or empty words
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
  
  # Combine lines with HS code (ensure leading zeros are displayed)
  # Format HS code with leading zeros for display
  hs_display <- ifelse(hs_code == "Country Average", "", sprintf("%08s", hs_code))
  
  if (line2 == "") {
    return(paste0(line1, "\n(HS ", as.character(hs_display), ")"))
  } else {
    return(paste0(line1, "\n", line2, "\n(HS ", as.character(hs_display), ")"))
  }
}

# Calculate effective rates
us_imports[, effective_rate := rate]

# Calculate total imports and tariff-weighted totals by HS code
hs_totals <- us_imports[, .(
  total_imports_by_hs = sum(us_imports_bn),
  total_tariff_weighted_by_hs = sum(effective_rate * us_imports_bn)
), by = hs_8digit]

# Merge back to main data
us_imports <- merge(us_imports, hs_totals, by = "hs_8digit", all.x = TRUE)

# Calculate competitor weighted average rate (excluding own country)
us_imports[, competitor_rate := ifelse(
  total_imports_by_hs - us_imports_bn > 0,
  (total_tariff_weighted_by_hs - effective_rate * us_imports_bn) / (total_imports_by_hs - us_imports_bn),
  NA_real_
)]

# Calculate tariff advantage (negative = disadvantage, positive = advantage)
us_imports[, tariff_advantage := competitor_rate - effective_rate]

# Create main relative advantage dataset
relative_advantage <- us_imports[!is.na(competitor_rate), .(
  exporter = exporter,
  hs_8digit = hs_8digit,
  hs_8digit_name = hs_8digit_name,
  own_rate = round(effective_rate, 2),
  own_rate_original = rate,
  competitor_rate = round(competitor_rate, 2),
  tariff_advantage = round(tariff_advantage, 2),
  import_value_bn = us_imports_bn,
  iso2 = iso2,
  iso_code = iso_code,
  un_code = un_code
)]

cat(sprintf("   Loaded %s import flows from %d countries\n", 
            format(nrow(relative_advantage), big.mark = ","),
            length(unique(relative_advantage$exporter))))

# =============================================================================
# 2. CREATE OUTPUT DIRECTORY
# =============================================================================

# Create countries output folder
dir.create(file.path(RUN_OUTPUT_DIR, "countries"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 3. PROCESS EACH COUNTRY
# =============================================================================

# Get list of unique countries sorted by total import value
country_totals <- relative_advantage[, .(
  total_imports = sum(import_value_bn)
), by = exporter][order(-total_imports)]

cat(sprintf("\n2. Processing %d countries individually...\n", nrow(country_totals)))

# Loop through each country
for (i in seq_len(nrow(country_totals))) {
  
  country_name <- country_totals$exporter[i]
  
  # Clean country name for file naming (remove special characters)
  clean_country <- gsub("[^A-Za-z0-9 ]", "", country_name)
  clean_country <- gsub("\\s+", "_", clean_country)
  clean_country <- substr(clean_country, 1, 50)  # Limit length
  
  cat(sprintf("   Processing %d/%d: %s\n", i, nrow(country_totals), country_name))
  
  # Filter data for this country
  country_data <- relative_advantage[exporter == country_name]
  
  if (nrow(country_data) == 0) {
    cat(sprintf("     No data available for %s, skipping...\n", country_name))
    next
  }
  
  # Create aggregate summary for this country
  country_summary <- country_data[, .(
    exporter = unique(exporter),
    iso2 = unique(iso2)[1],
    iso_code = unique(iso_code)[1],
    un_code = unique(un_code)[1],
    total_import_value_bn = sum(import_value_bn),
    flow_count = .N,
    avg_own_rate = round(weighted.mean(own_rate, import_value_bn), 2),
    avg_own_rate_original = round(weighted.mean(own_rate_original, import_value_bn), 2),
    avg_competitor_rate = round(weighted.mean(competitor_rate, import_value_bn), 2),
    avg_tariff_advantage = round(weighted.mean(tariff_advantage, import_value_bn), 2),
    flows_with_advantage = sum(tariff_advantage > 0),
    flows_with_disadvantage = sum(tariff_advantage < 0),
    advantage_share = round(100 * sum(tariff_advantage > 0) / .N, 1)
  )]
  
  # Order product-level data by import value
  country_products <- country_data[order(-import_value_bn)]
  
  # =============================================================================
  # 4. EXPORT TO EXCEL
  # =============================================================================
  
  # Create Excel file with two sheets
  excel_filename <- file.path(RUN_OUTPUT_DIR, "countries", sprintf("%s_tariff_advantage.xlsx", clean_country))
  
  write_xlsx(
    list(
          "Country Summary" = country_summary %>%
      select(-avg_own_rate_original) %>%
      rename(
        "Exporter" = exporter,
        "ISO2 Code" = iso2,
        "ISO Code" = iso_code,
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
  
  # =============================================================================
  # 5. CREATE TOP 10 PRODUCTS CHART
  # =============================================================================
  
  # Get top 10 products by import value
  top10_products <- head(country_products, 10)
  
  if (nrow(top10_products) > 0) {
    
    # Add country average as an additional bar to the top 10 products
    avg_bar <- data.frame(
      hs_8digit = "Country Average",
      tariff_advantage = country_summary$avg_tariff_advantage,
      import_value_bn = 0
    )
    
    # Combine data first, then sort everything together by tariff advantage
    combined_data <- rbind(
      top10_products %>% select(hs_8digit, tariff_advantage, import_value_bn),
      avg_bar
    )
    
    # Sort all data (including country average) by tariff advantage
    combined_data <- combined_data %>%
      arrange(tariff_advantage)  # Ascending for coord_flip to put highest advantage at top
    
    # Ensure HS codes are properly formatted before merging
    combined_data$hs_8digit <- ifelse(combined_data$hs_8digit == "Country Average", 
                                     "Country Average", 
                                     sprintf("%08s", combined_data$hs_8digit))
    
    # Merge with HS names and create abbreviated labels 
    # Both datasets now have consistent 8-digit string formatting with leading zeros
    combined_data <- merge(combined_data, hs_names, by = "hs_8digit", all.x = TRUE)
    combined_data$hs_8name[combined_data$hs_8digit == "Country Average"] <- "Country Average"
    
    # Create abbreviated product labels
    combined_data$product_label <- mapply(create_product_label, 
                                         combined_data$hs_8digit, 
                                         combined_data$hs_8name)
    
    # Reorder by tariff advantage for proper chart ordering
    combined_data <- combined_data[order(combined_data$tariff_advantage), ]
    combined_data$product_label_ordered <- factor(combined_data$product_label, levels = combined_data$product_label)
    
    # Create the chart
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
    chart_filename <- file.path(RUN_OUTPUT_DIR, "countries", sprintf("%s_top10_products.png", clean_country))
    ggsave(chart_filename, plot = p, width = 12, height = 8, 
           dpi = 300, bg = "white", units = "in", limitsize = FALSE)
  }
  
  # Progress update every 10 countries
  if (i %% 10 == 0) {
    cat(sprintf("     Completed %d/%d countries...\n", i, nrow(country_totals)))
  }
}

# Clean up
us_imports[, `:=`(total_imports_by_hs = NULL, total_tariff_weighted_by_hs = NULL)]

cat(sprintf("\n=== COUNTRY-SPECIFIC ANALYSIS COMPLETE ===\n"))
cat(sprintf("Processed %d countries\n", nrow(country_totals)))
cat(sprintf("Results saved in: %s\n", file.path(RUN_OUTPUT_DIR, "countries")))
cat("  - Excel files: [CountryName]_tariff_advantage.xlsx\n")
cat("  - Charts: [CountryName]_top10_products.png\n")