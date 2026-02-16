# Professional Chart Book Generation System
# Assembles existing analysis results into high-quality PDF Chart Books
# 
# IMPORTANT: This is a POST-PROCESSING tool that assembles existing results.
# Run country_specific_advantage.R FIRST to generate the underlying analysis.
# 
# This script:
# - Uses existing PNG charts from results/{group}/
# - Extracts tables from existing XLSX files
# - Uses relative paths (no file copying needed)
# - Assembles everything into a professional PDF
#
# Author: AI Assistant / Johannes Fritz
# Date: August 2025

cat("=== Professional Chart Book Generation System ===\n")

# Required packages
required_packages <- c("rmarkdown", "readxl", "ggplot2", "scales", "knitr", "dplyr")

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

# Load libraries
suppressPackageStartupMessages({
  library(rmarkdown)
  library(readxl)
  library(ggplot2)
  library(scales)
  library(knitr)
  library(dplyr)
})

# =============================================================================
# OUTPUT DIRECTORY CONFIGURATION
# =============================================================================
# When called from the pipeline, RUN_OUTPUT_DIR is set by the caller.
# When run standalone, default to results/standalone.
if (!exists("RUN_OUTPUT_DIR")) {
  source("code/date_config.R")
  RUN_OUTPUT_DIR <- "results/standalone"
}

# Configuration
pandoc_path <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
miktex_path <- file.path(Sys.getenv("USERPROFILE"), "AppData/Local/Programs/MiKTeX/miktex/bin/x64")

# Set environment
Sys.setenv(RSTUDIO_PANDOC = pandoc_path)
if (dir.exists(miktex_path)) {
  current_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(miktex_path, current_path, sep = ";"))
}

# Function to generate Chart Book for a specific country group
# This assembles existing analysis results into a professional PDF
generate_chartbook <- function(group_name = "c4tp", output_name = NULL, update_date = NULL) {
  
  cat("PDF Assembly for:", toupper(group_name), "group\n")
  cat("This tool assembles existing analysis results into a professional PDF Chart Book.\n")
  cat("Note: Run country_specific_advantage.R first to generate the underlying analysis.\n\n")
  
  # Set default output name
  if (is.null(output_name)) {
    output_name <- paste0(toupper(group_name), "_Chart_Book")
  }
  
  # Set default update date
  if (is.null(update_date)) {
    # Use UPDATE_DATE_LONG from update_processing.R if available, otherwise fall back to today's date
    if (exists("UPDATE_DATE_LONG")) {
      update_date <- UPDATE_DATE_LONG
    } else {
      update_date <- format(Sys.Date(), "%d %B %Y")
    }
  }
  
  # Check required files (charts and data created by country_specific_advantage.R)
  results_dir <- file.path(RUN_OUTPUT_DIR, "countries", group_name)
  
  required_aggregate_charts <- c(
    paste0(results_dir, "/effective_rates_", group_name, ".png"),
    paste0(results_dir, "/relative_advantage_", group_name, ".png"),
    paste0(results_dir, "/hypothetical_tariff_revenue_", group_name, ".png")
  )
  
  required_template_files <- c(
    "styling/simple_c4tp_template.Rmd",
    "styling/preamble.tex",
    "styling/GTA_logo.png",
    "styling/SECO_logo.png",
    "styling/C4TP_logo.png"
  )
  
  # Check all required files
  all_required_files <- c(required_aggregate_charts, required_template_files)
  missing_files <- all_required_files[!sapply(all_required_files, file.exists)]
  
  # Check for individual country files
  country_excel_files <- list.files(results_dir, pattern = "_tariff_advantage\\.xlsx$", full.names = FALSE)
  country_chart_files <- list.files(results_dir, pattern = "_top10_products\\.png$", full.names = FALSE)
  
  if (length(missing_files) > 0 || length(country_excel_files) == 0 || length(country_chart_files) == 0) {
    cat("ERROR: Missing required analysis files:\n")
    for (file in missing_files) {
      cat("  -", file, "\n")
    }
    if (length(country_excel_files) == 0) cat("  - No country Excel files found in", results_dir, "\n")
    if (length(country_chart_files) == 0) cat("  - No country chart files found in", results_dir, "\n")
    cat("Please run country_specific_advantage.R first to generate the analysis.\n")
    stop("Cannot proceed without required files.")
  }
  
  cat("Found", length(country_excel_files), "countries with analysis results.\n")
  
  # Create timestamped output directory structure (single timestamp for entire execution)
  timestamp <- format(Sys.time(), "%y%m%d-%H%M")
  edition_folder <- paste0(group_name, "_", timestamp)
  base_output_dir <- file.path(RUN_OUTPUT_DIR, "chartbooks", edition_folder)
  
  # Create directory structure - PDF in root, data/charts in subfolder
  output_dir <- base_output_dir # PDF will be stored here
  data_charts_dir <- file.path(base_output_dir, "data & charts") # Data and charts in subfolder
  aux_dir <- file.path(base_output_dir, "aux") # Auxiliary files (LaTeX, logos, etc.)
  
  # Create all directories
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_charts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(aux_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Created output structure:\n")
  cat("  Root folder:", output_dir, "\n")
  cat("  Data/Charts subfolder:", data_charts_dir, "\n")
  cat("  Auxiliary subfolder:", aux_dir, "\n")
  
  # Copy logos to auxiliary directory
  file.copy("styling/GTA_logo.png", file.path(aux_dir, "GTA_logo.png"), overwrite = TRUE)
  file.copy("styling/SECO_logo.png", file.path(aux_dir, "SECO_logo.png"), overwrite = TRUE)
  file.copy("styling/C4TP_logo.png", file.path(aux_dir, "C4TP_logo.png"), overwrite = TRUE)
  
  # Copy aggregate chart images to main output directory
  aggregate_charts <- c("effective_rates_c4tp.png", "relative_advantage_c4tp.png", "hypothetical_tariff_revenue_c4tp.png")
  for (chart in aggregate_charts) {
    chart_source <- file.path(results_dir, chart)
    if (file.exists(chart_source)) {
      file.copy(chart_source, file.path(output_dir, chart), overwrite = TRUE)
    }
  }
  
  # Copy all individual country chart images to main output directory
  country_chart_files <- list.files(results_dir, pattern = "_top10_products\\.png$", full.names = TRUE)
  for (chart_file in country_chart_files) {
    chart_name <- basename(chart_file)
    file.copy(chart_file, file.path(output_dir, chart_name), overwrite = TRUE)
  }
  
  # Copy all Excel files to main output directory
  country_excel_files_full <- list.files(results_dir, pattern = "_tariff_advantage\\.xlsx$", full.names = TRUE)
  for (excel_file in country_excel_files_full) {
    excel_name <- basename(excel_file)
    file.copy(excel_file, file.path(output_dir, excel_name), overwrite = TRUE)
  }
  
  # Copy preamble.tex to auxiliary directory
  file.copy("styling/preamble.tex", file.path(aux_dir, "preamble.tex"), overwrite = TRUE)
  
  cat("Copied", length(aggregate_charts), "aggregate charts and", length(country_chart_files), "country charts to output directory.\n")
  
  # Generate the PDF Chart Book
  tryCatch({
    cat("Rendering R Markdown to LaTeX...\n")
    
    # Copy the template to the output directory for local rendering
    template_copy <- file.path(output_dir, "temp_template.Rmd")
    file.copy("styling/simple_c4tp_template.Rmd", template_copy, overwrite = TRUE)
    
    # Generate PDF with pdflatex engine from the output directory
    old_wd <- getwd()
    setwd(output_dir)
    
    latex_file <- tryCatch({
      rmarkdown::render(
        input = "temp_template.Rmd",
        output_format = rmarkdown::pdf_document(
          toc = FALSE, # TOC is now manually handled
          includes = rmarkdown::includes(in_header = file.path("aux", "preamble.tex")), # Relative to output directory
          latex_engine = "pdflatex", # Use pdflatex for better compatibility
          keep_tex = TRUE
        ),
        output_file = paste0(output_name, ".pdf"),
        clean = FALSE,
        quiet = FALSE,
        params = list(
          data_charts_dir = data_charts_dir,
          aux_dir = aux_dir,
          edition_folder = edition_folder,
          update_date = update_date
        )
      )
    }, finally = {
      setwd(old_wd)
    })
    
    # Clean up temporary template
    file.remove(template_copy)
    
    # Check if PDF was created
    pdf_file <- paste0(output_name, ".pdf")
    full_pdf_path <- file.path(output_dir, pdf_file)
    
    if (file.exists(full_pdf_path)) {
      # Move auxiliary files (TEX, LOG, AUX, etc.) to aux directory
      aux_extensions <- c(".tex", ".log", ".aux", ".toc", ".out", ".fls", ".fdb_latexmk")
      base_name <- tools::file_path_sans_ext(pdf_file)
      
      for (ext in aux_extensions) {
        aux_file <- file.path(output_dir, paste0(base_name, ext))
        if (file.exists(aux_file)) {
          file.copy(aux_file, file.path(aux_dir, basename(aux_file)), overwrite = TRUE)
          file.remove(aux_file)
        }
      }
      
      # Also move intermediate Markdown files if they exist
      md_files <- list.files(output_dir, pattern = "\\.md$", full.names = TRUE)
      for (md_file in md_files) {
        file.copy(md_file, file.path(aux_dir, basename(md_file)), overwrite = TRUE)
        file.remove(md_file)
      }
      
      # Move all data and chart files to the data_charts_dir subfolder, leaving only PDF in root
      cat("Organizing final file structure...\n")
      
      # Get all files in output_dir that are not the PDF or directories
      all_files <- list.files(output_dir, full.names = TRUE, recursive = FALSE)
      data_files <- all_files[!file.info(all_files)$isdir & !grepl(paste0(tools::file_path_sans_ext(pdf_file), "\\.pdf$"), all_files)]
      
      # Move all data and chart files to the data_charts_dir subfolder, leaving only PDF in root
      cat("Organizing final file structure...\n")
      
      # Get all files in output_dir that are not the PDF or directories
      all_files <- list.files(output_dir, full.names = TRUE, recursive = FALSE)
      data_files <- all_files[!file.info(all_files)$isdir & !grepl(paste0(tools::file_path_sans_ext(pdf_file), "\\.pdf$"), all_files)]
      
      # Move data/chart files to data_charts_dir
      for (data_file in data_files) {
        file_name <- basename(data_file)
        # Skip if it's the temp template (should already be deleted)
        if (!grepl("temp_template", file_name)) {
          dest_path <- file.path(data_charts_dir, file_name)
          file.copy(data_file, dest_path, overwrite = TRUE)
          file.remove(data_file)
        }
      }
      
      # Create ZIP file of data & charts folder
      cat("Creating ZIP package of data & charts...\n")
      # Use UPDATE_DATE_SHORT from update_processing.R if available, otherwise fall back to today's date
      current_date <- if (exists("UPDATE_DATE_SHORT")) UPDATE_DATE_SHORT else format(Sys.Date(), "%y%m%d")
      zip_name <- paste0("RTTA - C4TP - data & charts - ", current_date, ".zip")
      zip_path <- file.path(output_dir, zip_name)
      
      # Create ZIP of the data_charts_dir contents
      old_wd <- getwd()
      setwd(data_charts_dir)
      
      # Get all files in data_charts_dir
      files_to_zip <- list.files(".", recursive = TRUE)
      
      if (length(files_to_zip) > 0) {
        zip_result <- zip(zipfile = file.path("..", zip_name), files = files_to_zip)
        setwd(old_wd)
        
        if (zip_result == 0 && file.exists(zip_path)) {
          zip_size <- file.info(zip_path)$size / (1024^2)  # MB
          cat("ZIP package created successfully!\n")
          cat("ZIP file:", zip_path, "\n")
          cat("ZIP file size:", round(zip_size, 2), "MB\n")
        } else {
          setwd(old_wd)
          cat("Warning: Failed to create ZIP package\n")
        }
      } else {
        setwd(old_wd)
        cat("Warning: No files found in data & charts folder to ZIP\n")
      }
      
      # Final summary
      file_size <- file.info(full_pdf_path)$size / (1024^2)  # MB
      cat("SUCCESS: Professional PDF Chart Book generated!\n")
      cat("Output file (PDF):", full_pdf_path, "\n")
      cat("Output file (ZIP):", if(file.exists(zip_path)) zip_path else "Not created", "\n")
      cat("PDF file size:", round(file_size, 2), "MB\n")
      cat("Final structure:\n")
      cat(sprintf("  - Root folder: %s (PDF + ZIP)\n", output_dir))
      cat(sprintf("  - Data/Charts: %s (%d files)\n", data_charts_dir, length(list.files(data_charts_dir))))
      cat(sprintf("  - Auxiliary: %s (%d files)\n", aux_dir, length(list.files(aux_dir))))
      
      return(list(pdf_path = full_pdf_path, zip_path = if(file.exists(zip_path)) zip_path else NULL))
      
    } else {
      stop("PDF generation failed. Check error messages above.")
    }
    
  }, error = function(e) {
    cat("ERROR generating Chart Book:\n")
    cat(e$message, "\n")
    return(NULL)
  })
}

# Main execution (removed to prevent double execution when sourcing)
# To run: source('code/generate_chartbook.R'); generate_chartbook("c4tp", "RTTA - C4TP - 250818")