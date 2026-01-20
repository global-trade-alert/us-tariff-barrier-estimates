# =============================================================================
# DATE CONFIGURATION - CENTRALIZED DATE MANAGEMENT
# Single source of truth for all dates used across the project
# =============================================================================
# Update these dates for each processing run
# Policy date: When the tariff policies took effect
# Update date: When this data was last processed/updated

# Check for environment variable override (used by timeline extraction scripts)
if (Sys.getenv("POLICY_DATE") != "") {
  POLICY_DATE <- as.Date(Sys.getenv("POLICY_DATE"))
} else {
  POLICY_DATE <- as.Date("2025-12-22")  # Policy effective date (default)
}

UPDATE_DATE <- as.Date("2025-12-22")   # Processing/update date

# Set locale to English for consistent date formatting
Sys.setlocale("LC_TIME", "C")

# Formatted versions for different contexts
POLICY_DATE_LONG <- format(POLICY_DATE, "%d %B %Y")     # "22 December 2025"
POLICY_DATE_SHORT <- format(POLICY_DATE, "%y%m%d")      # "251222"
UPDATE_DATE_LONG <- format(UPDATE_DATE, "%d %B %Y")     # "22 December 2025"
UPDATE_DATE_SHORT <- format(UPDATE_DATE, "%y%m%d")      # "251222"

# =============================================================================
# SCENARIO AND MFN RATE SOURCE CONFIGURATION
# =============================================================================
# These settings control the output file naming convention:
#   processed_us_imports_with_rates_{mfn_suffix}_{SCENARIO_NAME}
#
# MFN_RATE_SOURCE options:
#   - hts_schedule: HTS schedule rates (2024)
#   - mixed_schedule: Blended schedule rates (2024)
#   - observed_product: Trade-weighted observed rates per product (2024)
#   - observed_bilateral: Origin-specific observed rates per product-country (2024)

# SCENARIO_NAME: Load from env var or default to "baseline"
if (!exists("SCENARIO_NAME") || is.null(SCENARIO_NAME) || SCENARIO_NAME == "") {
  env_scenario <- Sys.getenv("SCENARIO_NAME", unset = "")
  if (env_scenario != "") {
    SCENARIO_NAME <- env_scenario
  } else {
    SCENARIO_NAME <- "baseline"
  }
}

# MFN_RATE_SOURCE: Load from env var or default to "hts_schedule"
if (!exists("MFN_RATE_SOURCE") || is.null(MFN_RATE_SOURCE) || MFN_RATE_SOURCE == "") {
  env_mfn_source <- Sys.getenv("MFN_RATE_SOURCE", unset = "")
  if (env_mfn_source != "") {
    MFN_RATE_SOURCE <- env_mfn_source
  } else {
    MFN_RATE_SOURCE <- "hts_schedule"
  }
}

# Compute MFN suffix for output filenames
MFN_SUFFIX <- switch(MFN_RATE_SOURCE,
  "hts_schedule" = "schedule24",
  "mixed_schedule" = "mixed24",
  "observed_product" = "product24",
  "observed_bilateral" = "bilateral24",
  "baseline_boe" = "boe24",
  "schedule24"  # fallback
)

# =============================================================================
# OPTION FLAGS (for observational MFN sources)
# =============================================================================
# These options control treatment of observational data where certain tariffs
# may already be reflected in observed 2024 rates.

# DISABLE_HTS_USMCA_WEIGHTING: Skip USMCA compliance weighting for HTS rates
if (!exists("DISABLE_HTS_USMCA_WEIGHTING") || is.null(DISABLE_HTS_USMCA_WEIGHTING)) {
  env_disable_usmca <- Sys.getenv("DISABLE_HTS_USMCA_WEIGHTING", unset = "")
  if (env_disable_usmca != "") {
    DISABLE_HTS_USMCA_WEIGHTING <- toupper(env_disable_usmca) %in% c("TRUE", "1", "YES")
  } else {
    DISABLE_HTS_USMCA_WEIGHTING <- FALSE
  }
}

# DISABLE_HTS_KORUS_WEIGHTING: Skip KORUS compliance weighting for HTS rates
if (!exists("DISABLE_HTS_KORUS_WEIGHTING") || is.null(DISABLE_HTS_KORUS_WEIGHTING)) {
  env_disable_korus <- Sys.getenv("DISABLE_HTS_KORUS_WEIGHTING", unset = "")
  if (env_disable_korus != "") {
    DISABLE_HTS_KORUS_WEIGHTING <- toupper(env_disable_korus) %in% c("TRUE", "1", "YES")
  } else {
    DISABLE_HTS_KORUS_WEIGHTING <- FALSE
  }
}

# EXCLUDE_S301_TARIFFS: Skip Section 301 tariff loading
if (!exists("EXCLUDE_S301_TARIFFS") || is.null(EXCLUDE_S301_TARIFFS)) {
  env_exclude_s301 <- Sys.getenv("EXCLUDE_S301_TARIFFS", unset = "")
  if (env_exclude_s301 != "") {
    EXCLUDE_S301_TARIFFS <- toupper(env_exclude_s301) %in% c("TRUE", "1", "YES")
  } else {
    EXCLUDE_S301_TARIFFS <- FALSE
  }
}

# Add option indicators to MFN_SUFFIX if non-default
if (DISABLE_HTS_USMCA_WEIGHTING) MFN_SUFFIX <- paste0(MFN_SUFFIX, "_nousmca")
if (DISABLE_HTS_KORUS_WEIGHTING) MFN_SUFFIX <- paste0(MFN_SUFFIX, "_nokorus")
if (EXCLUDE_S301_TARIFFS) MFN_SUFFIX <- paste0(MFN_SUFFIX, "_nos301")

# Standard output basename used across all scripts
OUTPUT_BASENAME <- paste0("processed_us_imports_with_rates_", MFN_SUFFIX, "_", SCENARIO_NAME)

# Helper function to get the data file path (handles scenario directories)
get_data_file_path <- function(output_dir = "results", extension = ".RData") {
  if (SCENARIO_NAME != "baseline" && output_dir == "results") {
    # Non-baseline scenarios are stored in subdirectories
    file.path("results", "scenarios", SCENARIO_NAME, paste0(OUTPUT_BASENAME, extension))
  } else {
    file.path(output_dir, paste0(OUTPUT_BASENAME, extension))
  }
}
