################################################################################
# Generate General Chartbook - Aggregate-Level Trump Tariff Analysis
#
# This script generates a 6-page PDF chartbook with high-level summaries of
# relative Trump tariff advantages (no individual country pages).
#
# Author: Johannes Fritz, St.Gallen Endowment
# Created: November 2025
################################################################################

# Load required packages
required_packages <- c("rmarkdown", "knitr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Generate General Chartbook
#'
#' Creates a PDF chartbook with aggregate-level analysis of Trump tariffs
#'
#' @param output_name Name for the output PDF (without extension)
#' @param policy_date Policy effective date (default: "14 October 2025")
#' @param update_date Document update date (default: today's date formatted as "DD Month YYYY")
#' @param aggregates_dir Directory containing aggregate charts (default: "../../results/aggregates")
#' @param output_dir Base directory for output (default: "../../results/chartbooks_general")
#' @param template_dir Directory containing template and styling files (default: "styling")
#'
#' @return List containing paths to generated PDF and output directory
#'
#' @examples
#' generate_chartbook_general(
#'   output_name = "GTA - Trump Tariff Advantage - Chart Book",
#'   policy_date = "14 October 2025",
#'   update_date = "26 November 2025"
#' )
#'
generate_chartbook_general <- function(
  output_name = "GTA - Trump Tariff Advantage - Chart Book",
  policy_date = "14 October 2025",
  update_date = format(Sys.Date(), "%d %B %Y"),
  aggregates_dir = "../../results/aggregates",
  output_dir = "../../results/chartbooks_general",
  template_dir = "styling",
  cty_dir = "../../results/cty"
) {

  cat("\n=== Generating General Chartbook ===\n\n")

  # Get absolute paths
  # Use current working directory (should be code/chartbook_general when sourced)
  script_dir <- getwd()

  aggregates_path <- normalizePath(file.path(script_dir, aggregates_dir), mustWork = FALSE)
  output_base <- normalizePath(file.path(script_dir, output_dir), mustWork = FALSE)
  template_path <- normalizePath(file.path(script_dir, template_dir), mustWork = FALSE)
  cty_path <- normalizePath(file.path(script_dir, cty_dir), mustWork = FALSE)

  cat("Script directory:", script_dir, "\n")
  cat("Aggregates directory:", aggregates_path, "\n")
  cat("Output base directory:", output_base, "\n")
  cat("Template directory:", template_path, "\n")
  cat("Country charts directory:", cty_path, "\n\n")

  # Create output base directory if it doesn't exist
  if (!dir.exists(output_base)) {
    dir.create(output_base, recursive = TRUE)
    cat("Created output base directory:", output_base, "\n")
  }

  # Validate required aggregate charts exist
  required_charts <- c(
    "effective_rates_top20.png",
    "relative_advantage_top20.png",
    "hypothetical_tariff_revenue_top20.png",
    "hypothetical_tariff_revenue_by_product_top10.png"
  )

  cat("Validating required charts...\n")
  missing_charts <- c()
  for (chart in required_charts) {
    chart_path <- file.path(aggregates_path, chart)
    if (!file.exists(chart_path)) {
      missing_charts <- c(missing_charts, chart)
      cat("  [MISSING]", chart, "\n")
    } else {
      cat("  [OK]", chart, "\n")
    }
  }

  if (length(missing_charts) > 0) {
    stop(sprintf(
      "\nMissing required charts in %s:\n  - %s\n\nPlease generate these charts before running the chartbook.",
      aggregates_path,
      paste(missing_charts, collapse = "\n  - ")
    ))
  }

  cat("\nAll required charts found!\n\n")

  # Validate template exists
  template_file <- file.path(template_path, "general_template.Rmd")
  if (!file.exists(template_file)) {
    stop(sprintf("Template file not found: %s", template_file))
  }

  # Create timestamped output directory
  timestamp <- format(Sys.time(), "%y%m%d-%H%M")
  output_folder_name <- paste0("general_", timestamp)
  output_folder <- file.path(output_base, output_folder_name)
  dir.create(output_folder, recursive = TRUE)

  cat("Created output folder:", output_folder, "\n")

  # Create subdirectories
  data_charts_dir <- file.path(output_folder, "data & charts")
  aux_dir <- file.path(output_folder, "aux")
  dir.create(data_charts_dir, recursive = TRUE)
  dir.create(aux_dir, recursive = TRUE)

  cat("Created subdirectories\n")

  # Copy aggregate charts to working directory
  cat("\nCopying aggregate charts...\n")
  for (chart in required_charts) {
    src <- file.path(aggregates_path, chart)
    dst_data <- file.path(data_charts_dir, chart)
    dst_output <- file.path(output_folder, chart)

    file.copy(src, dst_data, overwrite = TRUE)
    file.copy(src, dst_output, overwrite = TRUE)  # Copy to output root for LaTeX
    cat("  Copied:", chart, "\n")
  }

  # Copy Switzerland chart from cty directory
  cat("\nCopying country-specific charts...\n")
  # cty_dir is now passed as parameter and normalized to cty_path
  switzerland_chart_src <- "Switzerland_top10_products.png"
  switzerland_chart_dst <- "switzerland_top10_products.png"
  switzerland_src <- file.path(cty_path, switzerland_chart_src)

  if (file.exists(switzerland_src)) {
    dst_data <- file.path(data_charts_dir, switzerland_chart_dst)
    dst_output <- file.path(output_folder, switzerland_chart_dst)

    file.copy(switzerland_src, dst_data, overwrite = TRUE)
    file.copy(switzerland_src, dst_output, overwrite = TRUE)
    cat("  Copied:", switzerland_chart_src, "as", switzerland_chart_dst, "\n")
  } else {
    cat("  [WARNING] Switzerland chart not found at:", switzerland_src, "\n")
    cat("  The chartbook will be generated without this chart.\n")
  }

  # Copy styling files to output directory for LaTeX
  cat("\nCopying styling files...\n")
  styling_files <- list.files(template_path, pattern = "\\.(tex|png)$", full.names = TRUE)
  for (file in styling_files) {
    file.copy(file, output_folder, overwrite = TRUE)
    cat("  Copied:", basename(file), "\n")
  }

  # Copy template file to output folder
  cat("\nCopying template...\n")
  local_template <- file.path(output_folder, "general_template.Rmd")
  file.copy(template_file, local_template, overwrite = TRUE)
  cat("  Copied: general_template.Rmd\n")

  # Set up LaTeX path for macOS/Linux
  if (Sys.info()["sysname"] != "Windows") {
    latex_paths <- c(
      "/Library/TeX/texbin",
      "/usr/local/texlive/2023/bin/x86_64-darwin",
      "/usr/local/texlive/2024/bin/x86_64-darwin",
      "/usr/local/texlive/2025/bin/x86_64-darwin",
      "/usr/local/bin"
    )

    existing_paths <- latex_paths[sapply(latex_paths, dir.exists)]
    if (length(existing_paths) > 0) {
      current_path <- Sys.getenv("PATH")
      Sys.setenv(PATH = paste(existing_paths[1], current_path, sep = ":"))
      cat("\nAdded LaTeX path:", existing_paths[1], "\n")
    }
  }

  # Render the PDF
  cat("\n=== Rendering PDF ===\n")
  output_file <- paste0(output_name, ".pdf")
  output_pdf <- file.path(output_folder, output_file)

  tryCatch({
    # Change to output folder so LaTeX and knitr can find all files
    old_wd <- getwd()
    setwd(output_folder)

    rmarkdown::render(
      input = local_template,
      output_format = "pdf_document",
      output_file = output_file,
      params = list(
        policy_date = policy_date,
        update_date = update_date
      ),
      envir = new.env()
    )

    setwd(old_wd)

    cat("\n=== SUCCESS ===\n")
    cat("PDF generated:", output_pdf, "\n")

    # Move intermediate files to aux directory
    intermediate_files <- list.files(
      output_folder,
      pattern = "\\.(tex|log|aux|toc|out)$",
      full.names = TRUE
    )
    for (file in intermediate_files) {
      file.rename(file, file.path(aux_dir, basename(file)))
    }

    # Create ZIP archive of data & charts
    zip_name <- sprintf(
      "GTA - Trump Tariff Advantage - Data & Charts - %s.zip",
      format(Sys.Date(), "%y%m%d")
    )
    zip_path <- file.path(output_folder, zip_name)

    cat("\nCreating ZIP archive...\n")
    current_dir <- getwd()
    setwd(output_folder)

    if (Sys.info()["sysname"] == "Windows") {
      zip(zipfile = zip_name, files = "data & charts", extras = "-r")
    } else {
      system(sprintf("zip -r '%s' 'data & charts'", zip_name))
    }

    setwd(current_dir)
    cat("ZIP archive created:", zip_path, "\n")

    # Print summary
    cat("\n=== Output Summary ===\n")
    cat("Output directory:", output_folder, "\n")
    cat("PDF:", output_pdf, "\n")
    cat("Data & Charts:", data_charts_dir, "\n")
    cat("Auxiliary files:", aux_dir, "\n")
    cat("ZIP archive:", zip_path, "\n")

    return(list(
      pdf = output_pdf,
      output_dir = output_folder,
      data_dir = data_charts_dir,
      aux_dir = aux_dir,
      zip = zip_path
    ))

  }, error = function(e) {
    cat("\n=== ERROR ===\n")
    cat("Failed to generate PDF:\n")
    cat(conditionMessage(e), "\n")

    # Check for common issues
    if (!nzchar(Sys.which("pdflatex"))) {
      cat("\nLaTeX installation not found!\n")
      cat("Please install:\n")
      cat("  - macOS: MacTeX (https://www.tug.org/mactex/)\n")
      cat("  - Windows: MiKTeX (https://miktex.org/)\n")
      cat("  - Linux: TeX Live (sudo apt-get install texlive-full)\n")
    }

    stop(e)
  })
}

# Print usage information when sourced
if (!interactive()) {
  cat("\n")
  cat("=============================================================\n")
  cat("  General Chartbook Generator\n")
  cat("=============================================================\n")
  cat("\n")
  cat("Usage:\n")
  cat("  source('generate_chartbook_general.R')\n")
  cat("  result <- generate_chartbook_general(\n")
  cat("    output_name = 'GTA - Trump Tariff Advantage - Chart Book',\n")
  cat("    policy_date = '14 October 2025',\n")
  cat("    update_date = '26 November 2025'\n")
  cat("  )\n")
  cat("\n")
  cat("Required files:\n")
  cat("  - results/aggregates/effective_rates_top20.png\n")
  cat("  - results/aggregates/relative_advantage_top20.png\n")
  cat("  - results/aggregates/hypothetical_tariff_revenue_top20.png\n")
  cat("  - results/aggregates/hypothetical_tariff_revenue_by_product_top10.png\n")
  cat("\n")
  cat("Output:\n")
  cat("  - PDF chartbook in results/chartbooks_general/\n")
  cat("  - Data & charts folder with all input files\n")
  cat("  - Auxiliary folder with LaTeX files\n")
  cat("  - ZIP archive of data & charts\n")
  cat("\n")
  cat("=============================================================\n")
  cat("\n")
}
