# =============================================================================
# Create Charts & Data ZIP File
# Packages charts and data files for distribution
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

# Create output directory for distribution
dir.create(file.path(RUN_OUTPUT_DIR, "distribution"), recursive = TRUE, showWarnings = FALSE)

# Create ZIP file name with update date
# UPDATE_DATE_SHORT is passed from update_processing.R
zip_filename <- file.path(RUN_OUTPUT_DIR, "distribution",
                          sprintf("GTA - US Tariff Barrier Estimates - Charts & Data %s.zip", UPDATE_DATE_SHORT))

cat("Creating Charts & Data ZIP file:", zip_filename, "\n")

# =============================================================================
# PREPARE FILES AND FOLDERS
# =============================================================================

# Create a temporary directory for organizing files
temp_dir <- tempdir()
zip_staging_dir <- file.path(temp_dir, "charts_data_staging")
dir.create(zip_staging_dir, recursive = TRUE, showWarnings = FALSE)

# Define source directories
aggregates_dir <- file.path(RUN_OUTPUT_DIR, "analysis", "charts")
cty_dir <- file.path(RUN_OUTPUT_DIR, "countries")

# Define target structure in staging directory
target_cty_dir <- file.path(zip_staging_dir, "relative advantage by import origin")
dir.create(target_cty_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# COPY FILES TO STAGING DIRECTORY
# =============================================================================

files_copied <- 0
files_failed <- 0

# 1. Copy all files from results/aggregates to root of ZIP
cat(sprintf("\n1. Copying files from %s to ZIP root...\n", aggregates_dir))

if (dir.exists(aggregates_dir)) {
  aggregates_files <- list.files(aggregates_dir, full.names = TRUE, recursive = FALSE)
  
  if (length(aggregates_files) > 0) {
    for (file in aggregates_files) {
      if (file.info(file)$isdir == FALSE) {  # Only copy files, not subdirectories
        target_file <- file.path(zip_staging_dir, basename(file))
        
        if (file.copy(file, target_file, overwrite = TRUE)) {
          cat(sprintf("  ✓ %s\n", basename(file)))
          files_copied <- files_copied + 1
        } else {
          cat(sprintf("  ✗ Failed to copy: %s\n", basename(file)))
          files_failed <- files_failed + 1
        }
      }
    }
  } else {
    cat("  No files found in aggregates directory\n")
  }
} else {
  cat("  ✗ Aggregates directory not found:", aggregates_dir, "\n")
}

# 2. Copy all files from results/cty to "relative advantage by import origin" folder
cat(sprintf("\n2. Copying files from %s to 'relative advantage by import origin' folder...\n", cty_dir))

if (dir.exists(cty_dir)) {
  cty_files <- list.files(cty_dir, full.names = TRUE, recursive = TRUE)
  
  if (length(cty_files) > 0) {
    for (file in cty_files) {
      if (file.info(file)$isdir == FALSE) {  # Only copy files, not subdirectories
        # Preserve subdirectory structure within the target folder
        rel_path <- file.path(cty_dir, "")
        rel_file_path <- sub(paste0("^", gsub("\\\\", "/", normalizePath(rel_path, winslash = "/"))), "", 
                            gsub("\\\\", "/", normalizePath(file, winslash = "/")))
        target_file <- file.path(target_cty_dir, rel_file_path)
        
        # Create subdirectories if needed
        target_dir <- dirname(target_file)
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
        
        if (file.copy(file, target_file, overwrite = TRUE)) {
          cat(sprintf("  ✓ %s\n", rel_file_path))
          files_copied <- files_copied + 1
        } else {
          cat(sprintf("  ✗ Failed to copy: %s\n", rel_file_path))
          files_failed <- files_failed + 1
        }
      }
    }
  } else {
    cat("  No files found in cty directory\n")
  }
} else {
  cat("  ✗ Cty directory not found:", cty_dir, "\n")
}

# =============================================================================
# CREATE ZIP FILE
# =============================================================================

cat(sprintf("\n3. Creating ZIP file: %s\n", zip_filename))

if (files_copied > 0) {
  # Change to staging directory to create ZIP with proper structure
  old_wd <- getwd()
  setwd(zip_staging_dir)
  
  # Get all files and directories in staging area
  all_items <- list.files(".", recursive = TRUE, include.dirs = FALSE)
  
  if (length(all_items) > 0) {
    # Create the ZIP file
    zip_result <- zip(zipfile = file.path(old_wd, zip_filename), 
                     files = all_items,
                     flags = "-r")
    
    # Return to original directory
    setwd(old_wd)
    
    if (zip_result == 0 && file.exists(zip_filename)) {
      zip_size <- file.info(zip_filename)$size / (1024^2)  # Size in MB
      cat(sprintf("✓ Successfully created: %s (%.1f MB)\n", zip_filename, zip_size))
      
      # List contents for verification
      cat("\nZIP file contents:\n")
      zip_contents <- unzip(zip_filename, list = TRUE)
      if (nrow(zip_contents) > 0) {
        for (i in seq_len(nrow(zip_contents))) {
          cat(sprintf("  %s (%.1f KB)\n", zip_contents$Name[i], zip_contents$Length[i]/1024))
        }
      }
      
      success <- TRUE
    } else {
      cat("✗ Error: Failed to create ZIP file\n")
      success <- FALSE
    }
  } else {
    setwd(old_wd)
    cat("✗ Error: No files found in staging directory\n")
    success <- FALSE
  }
} else {
  cat("✗ Error: No files were successfully copied\n")
  success <- FALSE
}

# =============================================================================
# CLEANUP
# =============================================================================

# Remove staging directory
unlink(zip_staging_dir, recursive = TRUE)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CHARTS & DATA PACKAGE SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat(sprintf("ZIP File: %s\n", zip_filename))
cat(sprintf("Files copied: %d\n", files_copied))
cat(sprintf("Files failed: %d\n", files_failed))

if (success) {
  cat(sprintf("Package location: %s\n", normalizePath(zip_filename, winslash = "/")))
  cat("\nPackage contents:\n")
  cat(sprintf("- Root level: All files from %s\n", aggregates_dir))
  cat(sprintf("- 'relative advantage by import origin/': All files from %s\n", cty_dir))
  
  cat("\nTo distribute this package:\n")
  cat("1. Upload to cloud storage or file sharing service\n")
  cat("2. Use the upload script: python code/upload_results.py documents\n")
  cat("3. Share the resulting public URL\n")
} else {
  cat("Package creation failed. Please check the error messages above.\n")
}
