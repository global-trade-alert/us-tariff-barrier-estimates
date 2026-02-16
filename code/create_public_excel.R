# =============================================================================
# CREATE PUBLIC EXCEL FILE
# Updates the public version of processed US imports with tariff rates
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("CREATING PUBLIC EXCEL FILE\n")
cat("=============================================================================\n")

# Load required libraries
library(openxlsx)
library(dplyr)

# Output directory (set by update_processing.R, or standalone default)
if (!exists("RUN_OUTPUT_DIR")) {
  source("code/date_config.R")
  RUN_OUTPUT_DIR <- "results/standalone"
}

# Ensure output subdirectory exists
dir.create(file.path(RUN_OUTPUT_DIR, "public"), recursive = TRUE, showWarnings = FALSE)

# Check if us_imports exists in environment
if (!exists("us_imports")) {
  cat("Loading us_imports from RData file...\n")
  load(file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData")))
  # Ensure us_imports is a data.table (may lose class when loaded from RData)
  if (!is.data.table(us_imports)) {
    setDT(us_imports)
  }
}

# Define file paths using OUTPUT_BASENAME for consistent naming
public_file_path <- file.path(RUN_OUTPUT_DIR, "public", sprintf("%s_public.xlsx", OUTPUT_BASENAME))
dated_public_file <- file.path(RUN_OUTPUT_DIR, "public", sprintf("%s_public - %s.xlsx", OUTPUT_BASENAME, UPDATE_DATE_SHORT))

# Sort by exporter A-Z, keep original column order
us_imports_ordered <- us_imports %>%
  arrange(exporter) %>%
  as.data.frame()

cat(sprintf("  Preparing data: %s rows, %s columns\n",
            format(nrow(us_imports_ordered), big.mark = ","),
            ncol(us_imports_ordered)))

# Create or update public file
if (file.exists(public_file_path)) {
  cat("  Updating existing public file...\n")

  # Load the existing workbook to preserve other sheets
  public_wb <- openxlsx::loadWorkbook(public_file_path)

  # Check if "US Imports with Tariff Rates" sheet exists and remove it
  if ("US Imports with Tariff Rates" %in% names(public_wb)) {
    openxlsx::removeWorksheet(public_wb, "US Imports with Tariff Rates")
  }

  # Add the new sheet with reordered us_imports data
  openxlsx::addWorksheet(public_wb, "US Imports with Tariff Rates")
  openxlsx::writeData(public_wb, "US Imports with Tariff Rates", us_imports_ordered)

  # Save the updated public file (preserving other sheets)
  openxlsx::saveWorkbook(public_wb, public_file_path, overwrite = TRUE)

} else {
  cat("  Creating new public file...\n")

  # Create new public file
  public_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(public_wb, "US Imports with Tariff Rates")
  openxlsx::writeData(public_wb, "US Imports with Tariff Rates", us_imports_ordered)
  openxlsx::saveWorkbook(public_wb, public_file_path)
}

cat(sprintf("  Saved: %s\n", public_file_path))

# Create dated copy
file.copy(public_file_path, dated_public_file, overwrite = TRUE)
cat(sprintf("  Created dated copy: %s\n", dated_public_file))

cat("\n")
