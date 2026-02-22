# =============================================================================
# UPDATE PROCESSING PIPELINE
# Runs all analysis scripts in order for US Tariff Barrier Estimates
# =============================================================================
#
# Usage: Rscript code/update_processing.R          (from project root)
#
# Output structure:
#   results/{YYMMDD}/
#     dataset/        Core tariff estimates (.RData, .xlsx, .csv)
#     public/         Public Excel + replication ZIP
#     analysis/       Tariff analysis + relative advantage
#       charts/       Aggregate PNG charts
#     countries/      Per-country files
#       c4tp/         C4TP group aggregates
#     chartbooks/     PDF chartbooks (c4tp/, general/)
#     scorecards/     Country scorecards (all/, c4tp/, g20/, world/)
#     distribution/   ZIPs for upload
# =============================================================================

# Clear environment and set options
rm(list = ls())
options(scipen = 999)

# =============================================================================
# DATE CONFIGURATION
# =============================================================================
source("code/date_config.R")

cat("=============================================================================\n")
cat("US TARIFF BARRIER ESTIMATES - UPDATE PROCESSING PIPELINE\n")
cat("=============================================================================\n\n")

cat("Configuration:\n")
cat(sprintf("  Policy Date:    %s (%s)\n", POLICY_DATE_LONG, POLICY_DATE_SHORT))
cat(sprintf("  Update Date:    %s (%s)\n", UPDATE_DATE_LONG, UPDATE_DATE_SHORT))
cat(sprintf("  Scenario:       %s\n", SCENARIO_NAME))
cat(sprintf("  MFN Rate Source: %s (%s)\n", MFN_RATE_SOURCE, MFN_SUFFIX))
cat(sprintf("  Output Basename: %s\n", OUTPUT_BASENAME))

# =============================================================================
# OUTPUT DIRECTORY SETUP
# =============================================================================
# All outputs go under results/{YYMMDD}/ — each run is self-contained.
# No cleanup needed: a new date folder is a clean start.

RUN_OUTPUT_DIR <- file.path("results", paste0(UPDATE_DATE_SHORT, "-", POLICY_DATE_SHORT, "-", MFN_SUFFIX))

# Create the full directory tree
subdirs <- c(
  "dataset",
  "public",
  "analysis",
  file.path("analysis", "charts"),
  "countries",
  file.path("countries", "c4tp"),
  "chartbooks",
  file.path("chartbooks", "c4tp"),
  file.path("chartbooks", "general"),
  "scorecards",
  file.path("scorecards", "all"),
  "distribution"
)

for (d in subdirs) {
  dir.create(file.path(RUN_OUTPUT_DIR, d), recursive = TRUE, showWarnings = FALSE)
}

cat(sprintf("\nOutput directory: %s/\n\n", RUN_OUTPUT_DIR))

# Set SCENARIO_OUTPUT_DIR so us_tariff_calculation.R writes to dataset/
SCENARIO_OUTPUT_DIR <- file.path(RUN_OUTPUT_DIR, "dataset")

# =============================================================================
# STEP 1/7: CORE TARIFF CALCULATION
# =============================================================================

cat("STEP 1/7: Running us_tariff_calculation.R...\n")
cat("-----------------------------------------------\n")

source("code/us_tariff_calculation.R")

cat("\n  Creating public Excel...\n")
source("code/create_public_excel.R")

cat("\n  Creating replication ZIP...\n")
source("code/create_replication_zip.R")

cat("STEP 1 COMPLETE\n\n")

# =============================================================================
# STEP 2/7: TARIFF ANALYSIS
# =============================================================================

cat("STEP 2/7: Running tariff_analysis.R...\n")
cat("-----------------------------------------------\n")

source("code/tariff_analysis.R")

cat("STEP 2 COMPLETE\n\n")

# =============================================================================
# STEP 3/7: RELATIVE ADVANTAGE ANALYSIS
# =============================================================================

cat("STEP 3/7: Running relative_advantage.R...\n")
cat("-----------------------------------------------\n")

source("code/relative_advantage.R")

cat("STEP 3 COMPLETE\n\n")

# =============================================================================
# STEP 4/7: COUNTRY GROUP ANALYSIS (C4TP)
# =============================================================================

cat("STEP 4/7: Running country group analysis...\n")
cat("-----------------------------------------------\n")

source("code/country_specific_advantage.R")
source("code/country_group_analysis.R")

# Define C4TP countries
c4tp_countries <- c(
  "Albania", "Azerbaijan", "Bangladesh", "Brazil", "Cambodia", "Chile",
  "Colombia", "Costa Rica", "Dominican Republic", "Egypt", "El Salvador",
  "Eswatini, Swaziland", "Kazakhstan", "Kosovo", "Mexico", "Mongolia", "Morocco",
  "Montenegro", "Mozambique", "Namibia", "Nepal", "North Macedonia",
  "Pakistan", "Panama", "Peru", "Philippines", "Serbia", "Singapore",
  "Solomon Islands", "South Africa", "Thailand", "Tunisia", "United Arab Emirates",
  "Ukraine", "Uruguay", "Vietnam", "Zambia"
)

# Load processed data if not already in environment
if (!exists("us_imports")) {
  load(file.path(RUN_OUTPUT_DIR, "dataset", paste0(OUTPUT_BASENAME, ".RData")))
}

cat("Running C4TP country group analysis...\n")
c4tp_results <- analyze_country_group("c4tp", c4tp_countries, "C4TP")

cat("STEP 4 COMPLETE\n\n")

# =============================================================================
# STEP 5/7: CHARTBOOK GENERATION
# =============================================================================

cat("STEP 5/7: Generating chartbooks...\n")
cat("-----------------------------------------------\n")

# C4TP chartbook
source("code/generate_chartbook.R")
chartbook_name <- sprintf("RTTA - C4TP - %s", UPDATE_DATE_SHORT)
generate_chartbook("c4tp", chartbook_name, update_date = UPDATE_DATE_LONG)
cat("  C4TP chartbook generated\n")

# General illustration chartbook
source("code/chartbook_general/generate_chartbook_general.R")
general_chartbook_name <- sprintf("RTTA - Illustration ChartBook - %s", UPDATE_DATE_SHORT)
generate_chartbook_general(
  output_name = general_chartbook_name,
  policy_date = POLICY_DATE_LONG,
  update_date = UPDATE_DATE_LONG,
  aggregates_dir = file.path(RUN_OUTPUT_DIR, "analysis", "charts"),
  output_dir = file.path(RUN_OUTPUT_DIR, "chartbooks", "general"),
  cty_dir = file.path(RUN_OUTPUT_DIR, "countries"),
  template_dir = "code/chartbook_general/styling"
)
cat("  General chartbook generated\n")

cat("STEP 5 COMPLETE\n\n")

# =============================================================================
# STEP 6/7: CHARTS & DATA ZIP + SCORECARDS
# =============================================================================

cat("STEP 6/7: Creating distribution packages + scorecards...\n")
cat("-----------------------------------------------\n")

source("code/create_charts_data_zip.R")
cat("  Charts & data ZIP created\n")

# Scorecards
scorecard_cmd <- sprintf(
  'python3 code/scorecards/generate_scorecards.py --pdf --policy-date "%s" --update-date "%s" --output-dir "%s"',
  POLICY_DATE_LONG, UPDATE_DATE_LONG,
  file.path(RUN_OUTPUT_DIR, "scorecards", "all")
)
system(scorecard_cmd)
cat("  Scorecards generated\n")

# Copy scorecards for country groups (if copy_scorecards.py exists)
copy_script <- "code/scorecards/copy_scorecards.py"
if (file.exists(copy_script)) {
  copy_cmd <- sprintf('python3 %s %%s --date %s --output-dir "%s"',
                       copy_script, UPDATE_DATE_SHORT,
                       file.path(RUN_OUTPUT_DIR, "scorecards"))
  system(sprintf(copy_cmd, "c4tp"))
  system(sprintf(copy_cmd, "g20"))
  system(sprintf(copy_cmd, "wb-all"))
  system(sprintf(copy_cmd, "world"))
  cat("  Scorecard groups copied\n")
}

cat("STEP 6 COMPLETE\n\n")

# =============================================================================
# STEP 7/7: UPLOAD
# =============================================================================

cat("STEP 7/7: Uploading results...\n")
cat("-----------------------------------------------\n")

# NOTE: Review results before uploading
if (file.exists("code/upload_results.py")) {
  system(sprintf('python3 code/upload_results.py all --run-dir "%s"', RUN_OUTPUT_DIR))
  cat("  Upload complete\n")
} else {
  cat("  upload_results.py not found — skipping upload\n")
}

cat("STEP 7 COMPLETE\n\n")

# =============================================================================
# PIPELINE SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("PROCESSING PIPELINE COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nSteps completed:\n")
cat("  1. Core tariff calculation + public Excel + replication ZIP\n")
cat("  2. Tariff analysis (country-level summary)\n")
cat("  3. Relative advantage analysis\n")
cat("  4. Country group analysis (C4TP)\n")
cat("  5. Chartbook generation (C4TP + general)\n")
cat("  6. Distribution packages + scorecards\n")
cat("  7. Upload\n")
cat(sprintf("\nAll results in: %s/\n", RUN_OUTPUT_DIR))
cat("=============================================================================\n")
