################################################################################
# Demo Script for General Chartbook Generator
#
# This script demonstrates how to use the generate_chartbook_general function
# to create the aggregate-level Trump tariff analysis PDF.
#
# Author: Johannes Fritz, St.Gallen Endowment
# Created: November 2025
################################################################################

# Clear environment
rm(list = ls())

# Source the generation script
cat("Loading chartbook generator...\n")
source("generate_chartbook_general.R")

# Validate that required files exist
cat("\n=== Validating Input Files ===\n")

aggregates_dir <- "../../results/aggregates"
required_charts <- c(
  "effective_rates_top20.png",
  "relative_advantage_top20.png",
  "hypothetical_tariff_revenue_top20.png",
  "hypothetical_tariff_revenue_by_product_top10.png"
)

all_exist <- TRUE
for (chart in required_charts) {
  chart_path <- file.path(aggregates_dir, chart)
  if (file.exists(chart_path)) {
    cat("  [OK]", chart, "\n")
  } else {
    cat("  [MISSING]", chart, "\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  cat("\n")
  cat("ERROR: Some required chart files are missing!\n")
  cat("Please ensure all aggregate charts have been generated.\n")
  cat("\n")
  cat("To generate aggregate charts, run:\n")
  cat("  source('code/country_group_analysis.R')\n")
  cat("\n")
  stop("Missing required input files")
}

cat("\nAll required files found!\n")

# Check for LaTeX installation
cat("\n=== Checking LaTeX Installation ===\n")
latex_installed <- nzchar(Sys.which("pdflatex"))

if (latex_installed) {
  cat("LaTeX installation: FOUND\n")
  cat("pdflatex location:", Sys.which("pdflatex"), "\n")
} else {
  cat("LaTeX installation: NOT FOUND\n")
  cat("\n")
  cat("ERROR: LaTeX is required to generate PDF output.\n")
  cat("\n")
  cat("Please install LaTeX:\n")
  cat("  - macOS: MacTeX (https://www.tug.org/mactex/)\n")
  cat("  - Windows: MiKTeX (https://miktex.org/)\n")
  cat("  - Linux: TeX Live (sudo apt-get install texlive-full)\n")
  cat("\n")
  stop("LaTeX installation required")
}

# Generate the chartbook
cat("\n=== Generating Demo Chartbook ===\n")

result <- generate_chartbook_general(
  output_name = "GTA - Trump Tariff Advantage - Chart Book - DEMO",
  policy_date = "14 October 2025",
  update_date = format(Sys.Date(), "%d %B %Y")
)

# Print results
cat("\n=== Demo Complete ===\n")
cat("\nGenerated files:\n")
cat("  PDF:", result$pdf, "\n")
cat("  Output directory:", result$output_dir, "\n")
cat("\n")
cat("To view the PDF, run:\n")
if (Sys.info()["sysname"] == "Darwin") {
  cat(sprintf("  system('open \"%s\"')\n", result$pdf))
} else if (Sys.info()["sysname"] == "Windows") {
  cat(sprintf("  shell.exec(\"%s\")\n", result$pdf))
} else {
  cat(sprintf("  system('xdg-open \"%s\"')\n", result$pdf))
}
cat("\n")
