# =============================================================================
# Create Replication ZIP File - Minimal Package for us_tariff_calculation.R
# =============================================================================
# SCOPE: Packages only the files needed to run us_tariff_calculation.R
#
# APPROACH: Pattern-based auto-selection with explicit exclusions
# - Automatically includes all CSV files in data/ folder
# - Uses patterns to exclude system files, backups, and temporary files
# - Explicitly excludes known unused/superseded files
# - Validates presence of required files
# - Future-proof: new CSVs are automatically included
# =============================================================================

# Load required libraries
library(utils)

# =============================================================================
# OUTPUT DIRECTORY CONFIGURATION
# =============================================================================
# When called from the pipeline, RUN_OUTPUT_DIR is set by the caller.
# When run standalone, default to results/standalone.
if (!exists("RUN_OUTPUT_DIR")) {
  source("code/date_config.R")
  RUN_OUTPUT_DIR <- "results/standalone"
}

# =============================================================================
# SELF-CONTAINED: Detect project root for standalone execution
# =============================================================================
# Determine script location for standalone use
get_script_dir <- function() {
  # Try multiple methods to find script location

  # Method 1: Running via Rscript (commandArgs)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg))
    return(dirname(script_path))
  }


  # Method 2: Running in RStudio via source()
  if (exists("oFile", envir = sys.frame(1))) {
    return(dirname(normalizePath(sys.frame(1)$oFile)))
  }

  # Method 3: Check if rstudioapi is available (RStudio IDE)
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (rstudioapi::isAvailable()) {
      script_path <- tryCatch(
        rstudioapi::getSourceEditorContext()$path,
        error = function(e) NULL
      )
      if (!is.null(script_path) && nchar(script_path) > 0) {
        return(dirname(normalizePath(script_path)))
      }
    }
  }

  # Method 4: Fall back to current working directory
  return(getwd())
}

# Get script directory and set project root (one level up from code/)
# Used for resolving input paths; output paths use RUN_OUTPUT_DIR
script_dir <- get_script_dir()
if (basename(script_dir) == "code") {
  project_root <- dirname(script_dir)
} else {
  # Assume we're already in project root or code folder exists as sibling
  if (dir.exists(file.path(script_dir, "code"))) {
    project_root <- script_dir
  } else if (dir.exists(file.path(script_dir, "..", "code"))) {
    project_root <- normalizePath(file.path(script_dir, ".."))
  } else {
    project_root <- script_dir
  }
}

cat(sprintf("Project root: %s\n", project_root))
cat(sprintf("Output directory: %s\n\n", RUN_OUTPUT_DIR))

# Set date variables if not already defined (for standalone execution)
if (!exists("UPDATE_DATE_SHORT")) {
  UPDATE_DATE <- as.Date("2025-09-30")   # Default processing date
  UPDATE_DATE_SHORT <- format(UPDATE_DATE, "%y%m%d")      # "250930"
  cat(sprintf("Using default update date: %s\n", UPDATE_DATE_SHORT))
}

# Create ZIP file name with update date
zip_filename <- sprintf("GTA - US Tariff Barrier Estimates - Replication Files - %s.zip", UPDATE_DATE_SHORT)

cat("=============================================================================\n")
cat("CREATING MINIMAL REPLICATION PACKAGE\n")
cat("Scope: Files needed to run us_tariff_calculation.R\n")
cat("=============================================================================\n\n")

# =============================================================================
# PATTERN-BASED FILE EXCLUSION FUNCTION
# =============================================================================

should_exclude_file <- function(filename) {
  # Pattern-based exclusions (regex patterns)
  exclude_patterns <- c(
    "^\\._",                      # Mac hidden files (._filename)
    "^\\.DS_Store$",              # Mac metadata
    "^desktop\\.ini$",            # Windows metadata
    "^Thumbs\\.db$",              # Windows thumbnails
    "^~\\$",                      # Office temporary files
    "\\.(tmp|temp|bak|backup)$", # Explicit temp/backup extensions
    " - \\d{6}\\.csv$",           # Dated backup CSVs (e.g., " - 250815.csv")
    " - \\d{8}\\.csv$",           # Dated backup CSVs (e.g., " - 20250815.csv")
    " unsconsolidated\\.csv$",   # Explicitly marked as superseded
    "\\.xlsx$"                    # Excel files (we use CSV versions)
  )

  # Check if filename matches any exclusion pattern
  for (pattern in exclude_patterns) {
    if (grepl(pattern, basename(filename), perl = TRUE)) {
      return(TRUE)
    }
  }

  return(FALSE)
}

# =============================================================================
# EXPLICIT EXCLUSION LIST (Files not needed for us_tariff_calculation.R)
# =============================================================================

explicit_exclusions <- c(
  "data/can_northern_border.csv",              # Superseded by can_northern_border_exception.csv
  "data/mex_southern_border.csv",              # Not referenced in us_tariff_calculation.R
  "data/ind25_scope.csv",                      # Not referenced in us_tariff_calculation.R
  "data/ch_quiet_champions.csv",               # Only used in optional Swiss company analysis
  "data/us_hts-8_digit_sectioned.csv"          # Only used by downstream analysis scripts
)

# =============================================================================
# DEFINE MINIMAL CODE FILES (Only what's needed to run us_tariff_calculation.R)
# =============================================================================

code_files <- c(
  "code/date_config.R",              # Date configuration (sourced by main script)
  "code/us_tariff_calculation.R"     # Main tariff calculation script
)

# =============================================================================
# DOCUMENTATION FILES
# =============================================================================

documentation_files <- c(
  "documentation/methodology.md"
)

# =============================================================================
# SELECT DATA FILES WITH PATTERN-BASED FILTERING
# =============================================================================

cat("STEP 1: Selecting data files\n")
cat("---------------------------------------------\n")

# Get all files in data directory
all_data_files <- list.files("data", full.names = TRUE, recursive = FALSE)

# Filter out directories
all_data_files <- all_data_files[!file.info(all_data_files)$isdir]

cat(sprintf("Total files found in data/: %d\n", length(all_data_files)))

# Apply pattern-based exclusions
excluded_by_pattern <- character(0)
for (file in all_data_files) {
  if (should_exclude_file(file)) {
    excluded_by_pattern <- c(excluded_by_pattern, file)
  }
}

# Remove pattern-excluded files
remaining_files <- setdiff(all_data_files, excluded_by_pattern)

# Apply explicit exclusions
remaining_files <- setdiff(remaining_files, explicit_exclusions)

# Final data file list
data_files_selected <- remaining_files

cat(sprintf("Excluded by pattern: %d\n", length(excluded_by_pattern)))
cat(sprintf("Excluded explicitly: %d\n",
            length(intersect(explicit_exclusions, all_data_files))))
cat(sprintf("Data files selected: %d\n\n", length(data_files_selected)))

# Show what was excluded and why
if (length(excluded_by_pattern) > 0) {
  cat("Files excluded by pattern:\n")
  for (file in excluded_by_pattern) {
    cat(sprintf("  - %s\n", file))
  }
  cat("\n")
}

explicitly_excluded_existing <- intersect(explicit_exclusions, all_data_files)
if (length(explicitly_excluded_existing) > 0) {
  cat("Files explicitly excluded (not needed for us_tariff_calculation.R):\n")
  for (file in explicitly_excluded_existing) {
    cat(sprintf("  - %s\n", file))
  }
  cat("\n")
}

# =============================================================================
# VALIDATE CRITICAL FILES
# =============================================================================

cat("STEP 2: Validating critical files\n")
cat("---------------------------------------------\n")

# Define files that are absolutely required for us_tariff_calculation.R
critical_files <- c(
  # Core data
  "data/usitc_us_imports_2024.csv",
  "data/gta_jurisdiction_list.csv",
  "data/us_mfn_2025.csv",

  # Section 232 tariffs
  "data/sec232_auto.csv",
  "data/sec232_mhdv.csv",
  "data/sec232_steel.csv",
  "data/sec232_alu.csv",
  "data/sec232_copper.csv",
  "data/s232_lumber.csv",

  # Reciprocal tariffs (all versions - date conditional)
  "data/reciprocal_country_specific_topup.csv",
  "data/reciprocal_exceptions_apr2.csv",
  "data/reciprocal_exceptions_aug1.csv",
  "data/reciprocal_exceptions_251113.csv",

  # Country-specific exceptions
  "data/can_northern_border_exception.csv",
  "data/sec301.csv",
  "data/chli_exceptions.csv",
  "data/pharma_exceptions.csv",

  # Code files
  "code/date_config.R",
  "code/us_tariff_calculation.R"
)

missing_critical <- setdiff(critical_files,
                            c(all_data_files, code_files))

if (length(missing_critical) > 0) {
  cat("WARNING: Missing critical files:\n")
  for (file in missing_critical) {
    cat(sprintf("  ✗ %s\n", file))
  }
  cat("\n")
} else {
  cat("✓ All critical files present\n\n")
}

# =============================================================================
# ASSEMBLE COMPLETE FILE LIST
# =============================================================================

cat("STEP 3: Assembling file list\n")
cat("---------------------------------------------\n")

all_files <- c(
  code_files,
  documentation_files,
  data_files_selected
)

# Verify all files exist
existing_files <- all_files[file.exists(all_files)]
missing_files <- setdiff(all_files, existing_files)

cat(sprintf("Total files to include: %d\n", length(all_files)))
cat(sprintf("  - Code files: %d\n", length(code_files[code_files %in% existing_files])))
cat(sprintf("  - Documentation: %d\n", length(documentation_files[documentation_files %in% existing_files])))
cat(sprintf("  - Data files: %d\n", length(data_files_selected[data_files_selected %in% existing_files])))

if (length(missing_files) > 0) {
  cat(sprintf("\nWARNING: %d files missing and will be excluded:\n", length(missing_files)))
  for (file in missing_files) {
    cat(sprintf("  ✗ %s\n", file))
  }
}

cat(sprintf("\nFinal file count: %d\n\n", length(existing_files)))

# =============================================================================
# CREATE ZIP FILE
# =============================================================================

if (length(existing_files) > 0) {
  cat("STEP 4: Creating ZIP file\n")
  cat("---------------------------------------------\n")

  # Create output directory
  dir.create(file.path(RUN_OUTPUT_DIR, "public"), recursive = TRUE, showWarnings = FALSE)

  zip_output_path <- file.path(RUN_OUTPUT_DIR, "public", zip_filename)
  cat(sprintf("Output file: %s\n\n", zip_output_path))

  # Create the ZIP file with folder structure preserved
  zip(zipfile = zip_output_path,
      files = existing_files,
      flags = "-r")

  if (file.exists(zip_output_path)) {
    zip_size <- file.info(zip_output_path)$size / (1024^2)  # Size in MB
    cat(sprintf("✓ Successfully created: %s (%.1f MB)\n\n", zip_output_path, zip_size))

    # List contents for verification
    cat("ZIP file contents summary:\n")
    zip_contents <- unzip(zip_output_path, list = TRUE)

    # Group by folder
    code_count <- sum(grepl("^code/", zip_contents$Name))
    data_count <- sum(grepl("^data/", zip_contents$Name))
    doc_count <- sum(grepl("^documentation/", zip_contents$Name))

    cat(sprintf("  - code/: %d files\n", code_count))
    cat(sprintf("  - data/: %d files\n", data_count))
    cat(sprintf("  - documentation/: %d files\n", doc_count))
    cat(sprintf("  - Total: %d files\n", nrow(zip_contents)))

  } else {
    cat("✗ Error: Failed to create ZIP file\n")
  }
} else {
  cat("✗ Error: No files found to include in ZIP\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("REPLICATION PACKAGE SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat(sprintf("ZIP File: %s\n", file.path(RUN_OUTPUT_DIR, "public", zip_filename)))
cat(sprintf("Files included: %d\n", length(existing_files)))
cat(sprintf("Files missing: %d\n\n", length(missing_files)))

cat("Package contents:\n")
cat("  - Minimal code files: us_tariff_calculation.R + date_config.R\n")
cat(sprintf("  - Data files: %d CSV files (auto-selected)\n",
            length(data_files_selected[data_files_selected %in% existing_files])))
cat("  - Documentation: methodology.md\n\n")

cat("This package contains ONLY the files needed to run us_tariff_calculation.R\n")
cat("For the complete analysis pipeline, use the full project repository.\n\n")

cat("To reproduce the tariff calculations:\n")
cat("1. Extract the ZIP file\n")
cat("2. Set working directory to the extracted folder\n")
cat("3. Run: Rscript code/us_tariff_calculation.R\n")
cat("4. Refer to methodology.md for detailed documentation\n\n")

cat("Future-proof design:\n")
cat("  ✓ New CSV files in data/ are automatically included\n")
cat("  ✓ System files and backups are automatically excluded\n")
cat("  ✓ Pattern-based filtering requires no manual updates\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
