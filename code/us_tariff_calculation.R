# =============================================================================
# US TARIFF CALCULATIONS - REFACTORED VERSION
# =============================================================================
# Created: 2025-11-17
# Author: Johannes Fritz, St.Gallen Endowment
# Purpose: Calculate applied tariff rates on US imports in 2025 following logical rate-building architecture
#
# Rate Column Architecture:
# - hts_rate: MFN rate from US tariff schedule (includes USMCA preferential)
# - s232_rate: Rate from applicable S232 investigation (highest precedence only)
# - ieepa_rate: IEEPA baseline (10%) + country-specific reciprocal top-up
# - emergency_rate: Country-specific security actions (China opioid, Canada border)
# - s301_rate: Section 301 rate (China only)
# - rate: Final applied rate = mfn + [applicable mix of s232, ieepa and emergency] + China-specific actions (S301; Opioid)
# =============================================================================

# Load required libraries
library(data.table)
library(writexl)
library(dplyr)

# =============================================================================
# SCENARIO LOADING: Load Rate & Share Tables from CSV (Option C: Rate Database)
# =============================================================================
# This enables scenario-based tariff modeling by externalizing all hardcoded rates
# and shares to CSV files. Set SCENARIO_NAME before sourcing this script.
# =============================================================================

# Determine which scenario to load (default: "baseline")
# Check: 1) R variable, 2) environment variable, 3) default to baseline
if (!exists("SCENARIO_NAME") || is.null(SCENARIO_NAME) || SCENARIO_NAME == "") {
  env_scenario <- Sys.getenv("SCENARIO_NAME", unset = "")
  if (env_scenario != "") {
    SCENARIO_NAME <- env_scenario
  } else {
    SCENARIO_NAME <- "baseline"
  }
}
cat(sprintf("\nLoading scenario: %s\n", SCENARIO_NAME))

# Determine MFN rate source (default: "hts_schedule")
# Options: "hts_schedule", "mixed_schedule", "observed_product", "observed_bilateral", "baseline_boe"
# - hts_schedule: Statutory MFN rates from US Harmonized Tariff Schedule
# - mixed_schedule: Mode-based empirical rates with MFN fallbacks (2024)
# - observed_product: Trade-weighted observed rates per product (2024)
# - observed_bilateral: Origin-specific observed rates per product-country (2024)
# - baseline_boe: Bank of England 2024 tariff estimates (origin-specific, HS6-expanded)
if (!exists("MFN_RATE_SOURCE") || is.null(MFN_RATE_SOURCE) || MFN_RATE_SOURCE == "") {
  env_mfn_source <- Sys.getenv("MFN_RATE_SOURCE", unset = "")
  if (env_mfn_source != "") {
    MFN_RATE_SOURCE <- env_mfn_source
  } else {
    MFN_RATE_SOURCE <- "hts_schedule"  # Default
  }
}
cat(sprintf("Using MFN rate source: %s\n", MFN_RATE_SOURCE))

# Option: Disable USMCA compliance weighting for HTS rates
# Useful with observational MFN sources where USMCA preferences are already reflected
if (!exists("DISABLE_HTS_USMCA_WEIGHTING") || is.null(DISABLE_HTS_USMCA_WEIGHTING)) {
  env_disable_usmca <- Sys.getenv("DISABLE_HTS_USMCA_WEIGHTING", unset = "")
  if (env_disable_usmca != "") {
    DISABLE_HTS_USMCA_WEIGHTING <- toupper(env_disable_usmca) %in% c("TRUE", "1", "YES")
  } else {
    DISABLE_HTS_USMCA_WEIGHTING <- FALSE
  }
}
cat(sprintf("USMCA HTS weighting: %s\n", ifelse(DISABLE_HTS_USMCA_WEIGHTING, "DISABLED", "enabled")))

# Option: Disable KORUS compliance weighting for HTS rates
# Useful with observational MFN sources where KORUS preferences are already reflected
if (!exists("DISABLE_HTS_KORUS_WEIGHTING") || is.null(DISABLE_HTS_KORUS_WEIGHTING)) {
  env_disable_korus <- Sys.getenv("DISABLE_HTS_KORUS_WEIGHTING", unset = "")
  if (env_disable_korus != "") {
    DISABLE_HTS_KORUS_WEIGHTING <- toupper(env_disable_korus) %in% c("TRUE", "1", "YES")
  } else {
    DISABLE_HTS_KORUS_WEIGHTING <- FALSE
  }
}
cat(sprintf("KORUS HTS weighting: %s\n", ifelse(DISABLE_HTS_KORUS_WEIGHTING, "DISABLED", "enabled")))

# Option: Exclude Section 301 tariffs
# Useful with observational MFN sources where S301 tariffs are already reflected in 2024 data
if (!exists("EXCLUDE_S301_TARIFFS") || is.null(EXCLUDE_S301_TARIFFS)) {
  env_exclude_s301 <- Sys.getenv("EXCLUDE_S301_TARIFFS", unset = "")
  if (env_exclude_s301 != "") {
    EXCLUDE_S301_TARIFFS <- toupper(env_exclude_s301) %in% c("TRUE", "1", "YES")
  } else {
    EXCLUDE_S301_TARIFFS <- FALSE
  }
}
cat(sprintf("Section 301 tariffs: %s\n", ifelse(EXCLUDE_S301_TARIFFS, "EXCLUDED", "included")))

# Option: Fill gaps for a specific jurisdiction with synthetic zero-trade rows
# When set to a UN country code (e.g., 276 for Germany), generates synthetic rows
# for all HTS8 codes in the MFN schedule that have no trade data for that jurisdiction.
# This allows the pipeline to calculate exact applied rates for non-traded products.
if (!exists("FILL_GAPS_FOR") || is.null(FILL_GAPS_FOR) || FILL_GAPS_FOR == "") {
  env_fill_gaps <- Sys.getenv("FILL_GAPS_FOR", unset = "")
  if (env_fill_gaps != "") {
    FILL_GAPS_FOR <- as.integer(env_fill_gaps)
  } else {
    FILL_GAPS_FOR <- NULL
  }
}
if (!is.null(FILL_GAPS_FOR)) {
  cat(sprintf("Fill gaps for jurisdiction: UN %d\n", FILL_GAPS_FOR))
}

# Get scenarios directory - expects to run from project root
# (This script is run from project root, not code/ directory)
scenarios_dir <- "data/scenarios"

# Load rate table filtered by scenario
rates_file <- file.path(scenarios_dir, "rates.csv")
shares_file <- file.path(scenarios_dir, "shares.csv")

if (!file.exists(rates_file)) {
  stop(paste("Rates file not found:", rates_file))
}
if (!file.exists(shares_file)) {
  stop(paste("Shares file not found:", shares_file))
}

all_rates <- fread(rates_file)
all_shares <- fread(shares_file)

# =============================================================================
# SCENARIO MERGING: Merge scenario-specific values with baseline defaults
# =============================================================================
# Principle: Scenarios define ONLY deviations from baseline.
# - For rates: scenario rows override baseline rows with same (rate_type, effective_date)
# - For shares: scenario rows override baseline rows with same share_type
# - Missing values in BOTH scenario AND baseline throw errors (not silent 0s)
# =============================================================================

baseline_rates <- all_rates[scenario == "baseline"]
baseline_shares <- all_shares[scenario == "baseline"]

if (nrow(baseline_rates) == 0) {
  stop("No baseline rates found in rates.csv - baseline scenario must be defined")
}
if (nrow(baseline_shares) == 0) {
  stop("No baseline shares found in shares.csv - baseline scenario must be defined")
}

if (SCENARIO_NAME == "baseline") {
  # Baseline scenario: use baseline directly
  rates <- copy(baseline_rates)
  shares <- copy(baseline_shares)
} else {
  # Non-baseline scenario: merge scenario-specific values with baseline
  scenario_rates <- all_rates[scenario == SCENARIO_NAME]
  scenario_shares <- all_shares[scenario == SCENARIO_NAME]

  # RATES MERGING: Key is rate_type (complete override per rate_type)
  # If scenario defines ANY row for a rate_type, ALL baseline rows for that rate_type are excluded
  # This ensures scenario has complete control over time-series for rate_types it defines
  if (nrow(scenario_rates) > 0) {
    # Start with scenario rates
    rates <- copy(scenario_rates)

    # Get rate_types defined in scenario
    scenario_rate_types <- unique(scenario_rates$rate_type)

    # Add ALL baseline rows for rate_types NOT defined in scenario
    baseline_to_add <- baseline_rates[!rate_type %in% scenario_rate_types]
    rates <- rbind(rates, baseline_to_add, fill = TRUE)
  } else {
    # Scenario has no rate overrides - use baseline entirely
    rates <- copy(baseline_rates)
  }

  # SHARES MERGING: Key is share_type (single value per type)
  # Scenario rows override baseline rows with same share_type
  if (nrow(scenario_shares) > 0) {
    # Start with scenario shares
    shares <- copy(scenario_shares)

    # Add baseline shares where scenario doesn't define the share_type
    baseline_to_add <- baseline_shares[!share_type %in% scenario_shares$share_type]
    shares <- rbind(shares, baseline_to_add, fill = TRUE)
  } else {
    # Scenario has no share overrides - use baseline entirely
    shares <- copy(baseline_shares)
  }
}

# Validate we have rates (shares can technically be empty if baseline is empty)
if (nrow(rates) == 0) {
  stop(paste("No rates available for scenario:", SCENARIO_NAME))
}

cat(sprintf("  Scenario '%s': %d rates (%d scenario-specific), %d shares (%d scenario-specific)\n",
            SCENARIO_NAME,
            nrow(rates),
            nrow(all_rates[scenario == SCENARIO_NAME]),
            nrow(shares),
            nrow(all_shares[scenario == SCENARIO_NAME])))

# Helper function to get rate value (date-aware)
# Throws error if rate_type not found (except for ieepa_topup_ which defaults to 0)
get_rate <- function(rate_type_name, as_of_date = NULL) {
  # Default to policy_date if no date specified (backward compatible)
  if (is.null(as_of_date)) {
    as_of_date <- if (exists("policy_date")) policy_date else as.Date("2099-12-31")
  }


  # Filter by rate_type
  candidates <- rates[rate_type == rate_type_name]

  if (nrow(candidates) == 0) {
    # Country topups legitimately default to 0 if not defined (not all countries have topups)
    if (grepl("^ieepa_topup_", rate_type_name)) {
      return(0)
    }
    # All other missing rates are errors - forces explicit definition
    stop(paste("MISSING RATE:", rate_type_name,
               "- must be defined in rates.csv for scenario", SCENARIO_NAME, "or baseline"))
  }

  # If effective_date column exists, filter to latest before as_of_date
  if ("effective_date" %in% names(candidates)) {
    candidates[, eff_date := as.Date(effective_date)]
    candidates <- candidates[eff_date <= as_of_date]
    if (nrow(candidates) == 0) {
      # No rate found before as_of_date - return 0 (rate not yet in effect)
      return(0)
    }
    # Return the most recent rate
    return(candidates[order(-eff_date)][1, rate])
  }

  # Fallback: no effective_date column (legacy behavior)
  return(candidates[1, rate])
}

# Helper function to get country-specific IEEPA top-up
get_country_topup <- function(un_code, as_of_date = NULL) {
  rate_type_name <- paste0("ieepa_topup_", un_code)
  return(get_rate(rate_type_name, as_of_date))
}

# Helper function to get country-specific IEEPA floor override
# Returns NA if no override exists (use default EU floor)
get_country_ieepa_floor_override <- function(un_code, as_of_date = NULL) {
  rate_type_name <- paste0("ieepa_floor_override_", un_code)
  # Check if this rate type exists in rates data
  if (!rate_type_name %in% rates$rate_type) {
    return(NA_real_)
  }
  return(get_rate(rate_type_name, as_of_date))
}

# Helper function to get country-specific S232 floor override
# Returns NA if no override exists (use default EU floor)
get_country_s232_floor_override <- function(un_code, as_of_date = NULL) {
  rate_type_name <- paste0("s232_floor_override_", un_code)
  # Check if this rate type exists in rates data
  if (!rate_type_name %in% rates$rate_type) {
    return(NA_real_)
  }
  return(get_rate(rate_type_name, as_of_date))
}

# Helper function to get share value
# Throws error if share_type not found - forces explicit definition
get_share <- function(share_type_name) {
  val <- shares[share_type == share_type_name, share]
  if (length(val) == 0) {
    stop(paste("MISSING SHARE:", share_type_name,
               "- must be defined in shares.csv for scenario", SCENARIO_NAME, "or baseline"))
  }
  return(val[1])
}

# Load countries database (once, cached in global environment)
countries_file <- file.path(scenarios_dir, "countries.csv")
if (file.exists(countries_file)) {
  COUNTRIES_DB <- fread(countries_file, sep = ";")
} else {
  warning(paste("Countries file not found:", countries_file, "- using empty database"))
  COUNTRIES_DB <- data.table(
    un_code = integer(),
    iso_3 = character(),
    country_name = character(),
    special_treatment = character()
  )
}

# Load country groups database (once, cached in global environment)
country_groups_file <- file.path(scenarios_dir, "country_groups.csv")
if (file.exists(country_groups_file)) {
  COUNTRY_GROUPS_DB <- fread(country_groups_file, sep = ";")
} else {
  warning(paste("Country groups file not found:", country_groups_file, "- using empty database"))
  COUNTRY_GROUPS_DB <- data.table(
    group_name = character(),
    un_code = integer(),
    description = character()
  )
}

# Load MFN rates database (normalized table with all rate sources)
# Columns: source_key, hs_8digit, un_code (NULL for product-level), rate
mfn_rates_file <- file.path(scenarios_dir, "mfn_rates.csv")
if (file.exists(mfn_rates_file)) {
  MFN_RATES_DB <- fread(mfn_rates_file, colClasses = c(hs_8digit = "character"))
  # Ensure hs_8digit is padded to 8 digits (in case fread stripped leading zeros)
  MFN_RATES_DB[, hs_8digit := sprintf("%08s", hs_8digit)]
  # Convert empty un_code to NA for proper filtering
  MFN_RATES_DB[un_code == "", un_code := NA_character_]
  MFN_RATES_DB[, un_code := as.integer(un_code)]
  cat(sprintf("Loaded %s MFN rates from database\n", format(nrow(MFN_RATES_DB), big.mark = ",")))
} else {
  stop(paste("MFN rates file not found:", mfn_rates_file))
}

# Validate MFN_RATE_SOURCE
valid_mfn_sources <- c("hts_schedule", "mixed_schedule", "observed_product", "observed_bilateral", "baseline_boe")
if (!MFN_RATE_SOURCE %in% valid_mfn_sources) {
  stop(paste("Invalid MFN_RATE_SOURCE:", MFN_RATE_SOURCE,
             "- must be one of:", paste(valid_mfn_sources, collapse = ", ")))
}

# Helper function to get MFN rate from database
# For bilateral sources (observed_bilateral, baseline_boe): tries (source, hs, country) first,
#   then falls back to product-level rates from same source
# For product-level sources: matches (source, hs) where un_code is NA
get_mfn_rate <- function(hs_code, un_code_val = NULL, source = MFN_RATE_SOURCE) {
  # Ensure hs_code is properly formatted (8 digits)
  hs_code <- sprintf("%08s", as.character(hs_code))

  # Bilateral sources: observed_bilateral and baseline_boe
  bilateral_sources <- c("observed_bilateral", "baseline_boe")

  if (source %in% bilateral_sources && !is.null(un_code_val) && !is.na(un_code_val)) {
    # Try bilateral lookup first
    rate <- MFN_RATES_DB[source_key == source & hs_8digit == hs_code & un_code == un_code_val, rate]
    if (length(rate) > 0 && !is.na(rate[1])) {
      return(rate[1])
    }
    # Fall back to product-level for same source (or observed_product for observed_bilateral)
    if (source == "observed_bilateral") {
      source <- "observed_product"
    }
    # For baseline_boe, fall through to product-level lookup with same source
  }

  # Product-level lookup (un_code is NA)
  rate <- MFN_RATES_DB[source_key == source & hs_8digit == hs_code & is.na(un_code), rate]
  if (length(rate) > 0 && !is.na(rate[1])) {
    return(rate[1])
  }

  # Default fallback - return 0 with warning
  return(0)
}

# Helper function to get country UN code by ISO-3 code
get_country <- function(iso_3_code) {
  result <- COUNTRIES_DB[iso_3 == iso_3_code, un_code]
  if (length(result) == 0) {
    stop(paste("Country not found:", iso_3_code))
  }
  return(result[1])
}

# Helper function to get all UN codes for a country group
# Throws error if group not found - forces explicit definition in country_groups.csv
get_country_group <- function(grp_name) {
  result <- COUNTRY_GROUPS_DB[group_name == grp_name, un_code]
  if (length(result) == 0) {
    stop(paste("MISSING COUNTRY GROUP:", grp_name,
               "- must be defined in country_groups.csv"))
  }
  return(result)
}

# Load exceptions database (once, cached in global environment)
exceptions_file <- file.path(scenarios_dir, "exceptions.csv")
if (file.exists(exceptions_file)) {
  EXCEPTIONS_DB <- fread(exceptions_file, sep = ";")
  EXCEPTIONS_DB[, hs_8digit := sprintf("%08s", hs_8digit)]
  EXCEPTIONS_DB[, effective_date := as.Date(effective_date)]
  EXCEPTIONS_DB[, end_date := as.Date(end_date)]
} else {
  warning(paste("Exceptions file not found:", exceptions_file, "- using empty database"))
  EXCEPTIONS_DB <- data.table(
    hs_8digit = character(),
    rate_type = character(),
    import_origin = character(),
    exception_type = character(),
    effective_date = as.Date(character()),
    end_date = as.Date(character()),
    source = character()
  )
}

#' Load product scope or exceptions for a given rate_type
#' @param rate_type string matching rates.csv (e.g., "s232_auto_rate", "ieepa_baseline_rate")
#' @param exception_type Optional filter (e.g., "annex2", "statutory", "product_scope")
#' @param as_of_date Date for time-varying exceptions (default: policy_date or far future)
#' @param import_origin Optional country filter (ISO-3 or UN code)
#' @return vector of hs_8digit codes
get_exceptions <- function(rate_type_name,
                           exception_type = NULL,
                           as_of_date = NULL,
                           import_origin = NULL) {
  # Default to policy_date if no date specified (backward compatible)
  if (is.null(as_of_date)) {
    as_of_date <- if (exists("policy_date")) policy_date else as.Date("2099-12-31")
  }

  # Start with all exceptions
  result <- copy(EXCEPTIONS_DB)

  # Filter by rate_type
  result <- result[rate_type == rate_type_name]

  if (nrow(result) == 0) {
    return(character())
  }

  # Filter by exception_type if provided
  if (!is.null(exception_type)) {
    exc_type_filter <- exception_type  # Store in local variable to avoid data.table scoping issues
    result <- result[exception_type == exc_type_filter]
  }

  # Filter by date (active on as_of_date)
  result <- result[
    effective_date <= as_of_date &
    (is.na(end_date) | end_date >= as_of_date)
  ]

  # Filter by origin if provided
  if (!is.null(import_origin)) {
    origin_filter <- import_origin  # Store in local variable to avoid data.table scoping issues
    result <- result[import_origin %in% c(origin_filter, "ALL")]
  }

  return(unique(result$hs_8digit))
}

cat(sprintf("  Loaded %d rates, %d shares, and %d exceptions for scenario: %s\n",
            nrow(rates), nrow(shares), nrow(EXCEPTIONS_DB), SCENARIO_NAME))
cat("=============================================================================\n")

cat("\n")
cat("=============================================================================\n")
cat("US TARIFF CALCULATIONS - REFACTORED VERSION\n")
cat("=============================================================================\n")
cat("\n")

# =============================================================================
# SECTION 0: CONFIGURATION & PARAMETERS
# =============================================================================
# All assumptions centralized here for transparency and adjustability
# =============================================================================

cat("SECTION 0: Configuration & Parameters\n")
cat("--------------------------------------\n")

# -----------------------------------------------------------------------------
# 0.0: Policy Date (MUST be set before any get_rate() calls)
# -----------------------------------------------------------------------------
# All time-dependent rate lookups use policy_date to determine effective rates.
# If policy_date is before a rate's effective_date, get_rate() returns 0.
# -----------------------------------------------------------------------------

cat("  0.0 Setting policy date...\n")

# POLICY DATE (for future reconstruction capability)
# Load centralized date configuration
if (!exists("POLICY_DATE")) {
  source("code/date_config.R")
}
policy_date <- POLICY_DATE

cat(sprintf("    Policy date: %s\n", policy_date))

# -----------------------------------------------------------------------------
# 0.1: Country Code Definitions (UN Statistical Division Codes)
# -----------------------------------------------------------------------------
# NOTE: Country codes now loaded from data/scenarios/countries.csv
# Country groups loaded from data/scenarios/country_groups.csv
# -----------------------------------------------------------------------------

cat("  0.1 Loading country code definitions...\n")

# Major trading partners (loaded from countries.csv)
uk_un_code <- get_country("GBR")
japan_un_code <- get_country("JPN")
canada_un_code <- get_country("CAN")
mexico_un_code <- get_country("MEX")
china_un_code <- get_country("CHN")
hong_kong_un_code <- get_country("HKG")
brazil_un_code <- get_country("BRA")
india_un_code <- get_country("IND")
korea_un_code <- get_country("KOR")
switzerland_un_code <- get_country("CHE")
liechtenstein_un_code <- get_country("LIE")
australia_un_code <- get_country("AUS")
ukraine_un_code <- get_country("UKR")
argentina_un_code <- get_country("ARG")
twn_un_code <- get_country("TWN")
slv_un_code <- get_country("SLV")
gtm_un_code <- get_country("GTM")
bgd_un_code <- get_country("BGD")

# Country groups (loaded from country_groups.csv)
usmca_partners <- get_country_group("usmca")
china_hkg_codes <- get_country_group("china_pair")
swiss_liechtenstein <- get_country_group("swiss_pair")
eu_members <- get_country_group("eu_member")
sanctioned_countries <- get_country_group("sanctioned")

cat(sprintf("    Loaded %d countries, %d groups from CSV\n",
            nrow(COUNTRIES_DB), length(unique(COUNTRY_GROUPS_DB$group_name))))
cat(sprintf("    USMCA: %d, EU: %d, Sanctioned: %d\n",
            length(usmca_partners), length(eu_members),
            length(sanctioned_countries)))


# =============================================================================
# SECTION 1: LOAD & SCOPE DATA
# =============================================================================
# Load import data and define analysis scope
# =============================================================================

cat("SECTION 1: Load & Scope Data\n")
cat("-----------------------------\n")

# -----------------------------------------------------------------------------
# 1.1: Load US Import Data
# -----------------------------------------------------------------------------

cat("  1.1 Loading US import data...\n")

# Load USITC 2024 import data
us_imports <- fread("data/usitc_us_imports_2024.csv", sep = ";")
cat(sprintf("    Loaded %s rows of 2024 US import data\n", format(nrow(us_imports), big.mark = ",")))

# Load 2025 missing combinations (new import origins not observed in 2024)
# These are generated by data/generate_2025_missing_combos.R
us_imports_2025_file <- "data/us_imports_2025.csv"
if (file.exists(us_imports_2025_file)) {
  us_imports_2025 <- fread(us_imports_2025_file, sep = ";")
  cat(sprintf("    Loaded %s rows of 2025 new combinations (zero-value placeholders)\n", 
              format(nrow(us_imports_2025), big.mark = ",")))
  us_imports <- rbindlist(list(us_imports, us_imports_2025), fill = TRUE)

  # Deduplicate: 15 small countries have broken un_code/iso2 in the 2024 USITC file
  # (un_code=NA, iso2=#N/A) but valid codes in the 2025 placeholders. When both files
  # contain the same exporter × hs8, keep the 2024 row (has real trade data) and drop
  # the zero-trade 2025 placeholder.
  n_before <- nrow(us_imports)
  dup_keys <- us_imports[, .N, by = .(exporter, hs_8digit)][N > 1]
  if (nrow(dup_keys) > 0) {
    is_dup <- us_imports[dup_keys, on = .(exporter, hs_8digit), nomatch = 0L,
                          which = TRUE]
    is_dup_zero <- intersect(is_dup, which(us_imports$us_imports == 0))
    if (length(is_dup_zero) > 0) {
      us_imports <- us_imports[-is_dup_zero]
    }
    cat(sprintf("    Deduplicated: removed %d zero-trade rows duplicating 2024 data\n",
                length(is_dup_zero)))
  }

  cat(sprintf("    Combined total: %s rows\n", format(nrow(us_imports), big.mark = ",")))
} else {
  cat("    Note: No 2025 combinations file found (data/us_imports_2025.csv)\n")
  cat("          Run data/generate_2025_missing_combos.R to generate it.\n")
}

# Convert imports to billions for easier handling
us_imports[, us_imports_bn := us_imports / 1e9]

# Ensure HS8 codes are properly formatted with leading zeros (8 digits)
us_imports[, hs_8digit := sprintf("%08s", hs_8digit)]

cat(sprintf("    Total import value: $%.1f billion\n", sum(us_imports$us_imports_bn, na.rm = TRUE)))

# Merge 3-letter ISO codes from GTA jurisdiction list
gta_jurisdictions <- fread("data/gta_jurisdiction_list.csv")
us_imports <- merge(us_imports,
                   gta_jurisdictions[, .(un_code, iso_code)],
                   by = "un_code",
                   all.x = TRUE)
cat(sprintf("    Merged 3-letter ISO codes from GTA jurisdiction list\n"))

# --- Optional: Inject synthetic zero-trade rows for gap HTS8 codes ---
# When FILL_GAPS_FOR is set, this creates rows for every MFN schedule HTS8
# that has no trade data for the target jurisdiction. The pipeline then
# calculates exact applied rates instead of requiring downstream estimation.
if (!is.null(FILL_GAPS_FOR)) {
  cat("    Injecting synthetic zero-trade rows for gap HTS8 codes...\n")

  # All HTS8 codes in the MFN schedule
  mfn_hts8 <- unique(MFN_RATES_DB[source_key == "hts_schedule", hs_8digit])

  # HTS8 codes already present for this jurisdiction
  existing_hts8 <- unique(us_imports[un_code == FILL_GAPS_FOR, hs_8digit])

  # Gap = MFN schedule minus existing
  gap_hts8 <- setdiff(mfn_hts8, existing_hts8)

  if (length(gap_hts8) > 0) {
    # Look up jurisdiction details
    fill_jurisdiction <- gta_jurisdictions[un_code == FILL_GAPS_FOR]
    fill_exporter <- if (nrow(fill_jurisdiction) > 0) fill_jurisdiction$jurisdiction_name[1] else paste("UN", FILL_GAPS_FOR)
    fill_iso_code <- if (nrow(fill_jurisdiction) > 0) fill_jurisdiction$iso_code[1] else NA_character_

    synthetic <- data.table(
      data_type = "Synthetic Gap Fill",
      exporter = fill_exporter,
      iso2 = NA_character_,
      un_code = FILL_GAPS_FOR,
      hs_8digit = gap_hts8,
      hs_8digit_name = NA_character_,
      us_imports = 0,
      us_imports_bn = 0,
      iso_code = fill_iso_code
    )

    us_imports <- rbindlist(list(us_imports, synthetic), fill = TRUE)

    cat(sprintf("    Injected %s synthetic zero-trade rows for %s (UN %d)\n",
                format(length(gap_hts8), big.mark = ","), fill_exporter, FILL_GAPS_FOR))
    cat(sprintf("    MFN schedule HTS8: %s, existing: %s, gap: %s\n",
                format(length(mfn_hts8), big.mark = ","),
                format(length(existing_hts8), big.mark = ","),
                format(length(gap_hts8), big.mark = ",")))
  } else {
    cat("    No gap HTS8 codes to inject (full coverage already exists)\n")
  }
}

# Load product-level USMCA compliance rates
cat("    Loading product-level USMCA compliance rates...\n")
compliance_shares <- fread("data/compliance_shares.csv")
compliance_shares[, hs_8digit := sprintf("%08s", hs_8digit)]
cat(sprintf("    Loaded %s product-country compliance rates\n", format(nrow(compliance_shares), big.mark = ",")))

# -----------------------------------------------------------------------------
# 1.2: Analysis Scope Limitations
# -----------------------------------------------------------------------------

cat("\n  1.2 Applying analysis scope limitations...\n")

# Report pre-exclusion statistics
total_imports_before <- sum(us_imports$us_imports_bn)
rows_before <- nrow(us_imports)

# Exclude Chapter 99
chapter99_rows <- nrow(us_imports[as.numeric(hs_8digit) >= 99000000])
chapter99_value <- sum(us_imports[as.numeric(hs_8digit) >= 99000000]$us_imports_bn, na.rm = TRUE)
us_imports <- us_imports[as.numeric(hs_8digit) < 99000000]

cat(sprintf("    Excluded Chapter 99: %s rows ($%.2f billion)\n",
            format(chapter99_rows, big.mark = ","), chapter99_value))

# Exclude sanctioned countries
sanctioned_rows <- nrow(us_imports[un_code %in% sanctioned_countries])
sanctioned_value <- sum(us_imports[un_code %in% sanctioned_countries]$us_imports_bn, na.rm = TRUE)
us_imports <- us_imports[!un_code %in% sanctioned_countries]

cat(sprintf("    Excluded sanctioned countries: %s rows ($%.2f billion)\n",
            format(sanctioned_rows, big.mark = ","), sanctioned_value))

# Report final scope
total_imports_after <- sum(us_imports$us_imports_bn)
cat(sprintf("    Analysis scope: %s rows, $%.1f billion in imports\n",
            format(nrow(us_imports), big.mark = ","), total_imports_after))
cat(sprintf("    Total excluded: $%.2f billion (%.1f%% of original)\n",
            total_imports_before - total_imports_after,
            100 * (total_imports_before - total_imports_after) / total_imports_before))

# -----------------------------------------------------------------------------
# 1.3: Initialize All Rate and Marker Columns
# -----------------------------------------------------------------------------

cat("\n  1.3 Initializing rate and marker columns...\n")

# Initialize rate columns (all start at 0)
# Note: hts_rate will be populated by merge in Section 2.3
us_imports[, `:=`(
  s232_rate = 0,
  ieepa_rate = 0,
  emergency_rate = 0,
  s301_rate = 0,
  country_surcharge_rate = 0,  # Country-specific surcharges (Section 7.99)
  rate = 0
)]

# Initialize boolean marker columns (all start at 0)
us_imports[, `:=`(
  s232_auto = 0, s232_auto_parts = 0, s232_mhdv = 0, s232_pv_parts = 0,
  s232_steel = 0, s232_steel_derivative = 0, s232_steel_exempt = 0,
  s232_alu = 0, s232_alu_derivative = 0,
  s232_copper = 0, s232_copper_derivative = 0,
  s232_lumber = 0, s232_lumber_derivative = 0,
  rr_exception = 0,
  ieepa_statute_exception = 0,
  missing_mfn = 0,
  bra_exception = 0, bra_aircraft = 0, bra_vehicle = 0,
  eu_exception = 0, eu_pharma = 0,
  uk_aircraft = 0, uk_auto = 0,
  jpn_lumber_derivative = 0, eu_lumber_derivative = 0,
  kor_floor = 0, kor_lumber_derivative = 0, kor_aircraft = 0, kor_auto = 0,
  che_floor = 0, che_aircraft = 0, che_exception = 0, che_pharma = 0,
  arg_exception = 0, arg_pharma = 0, arg_aircraft = 0,
  twn_floor = 0, twn_aircraft = 0, twn_exception = 0, twn_pharma = 0,
  slv_exception = 0, slv_pharma = 0, slv_aircraft = 0, slv_cafta = 0,
  gtm_exception = 0, gtm_pharma = 0, gtm_aircraft = 0, gtm_cafta = 0,
  bgd_exception = 0, bgd_pharma = 0, bgd_aircraft = 0,
  wto_aircraft = 0,
  chn_opioid = 0,
  sec301_tariff = 0,
  can_northern_border = 0,
  can_northern_border_energy = 0,
  mex_emergency = 0
)]

cat(sprintf("    Initialized %d rate columns and %d boolean marker columns\n", 13, 45))

cat("\n  Section 1 complete: Data loaded and scoped\n")
cat("\n")


# =============================================================================
# SECTIONS 0.2-8: Rate parameters + Rate assignment
# =============================================================================
# Extracted to apply_rates.R for reuse by run_timeline.R
# =============================================================================

source("code/apply_rates.R")

# =============================================================================
# SECTION 9: SUMMARY STATISTICS & EXPORT
# =============================================================================
# Report results and save outputs
# =============================================================================

cat("SECTION 9: Summary Statistics and Export\n")
cat("-----------------------------------------\n")

# Summary statistics by rate type
cat("\n  By rate type:\n")

mfn_only <- sum(us_imports[rate == hts_rate]$us_imports_bn, na.rm = TRUE)
s232_applied <- sum(us_imports[s232_rate > 0]$us_imports_bn, na.rm = TRUE)
ieepa_applied <- sum(us_imports[ieepa_rate > 0 & s232_rate == 0]$us_imports_bn, na.rm = TRUE)

cat(sprintf("    MFN only: $%.1f billion (%.1f%%)\n",
            mfn_only, 100 * mfn_only / sum(us_imports$us_imports_bn)))
cat(sprintf("    S232 applied: $%.1f billion (%.1f%%)\n",
            s232_applied, 100 * s232_applied / sum(us_imports$us_imports_bn)))
cat(sprintf("    IEEPA applied: $%.1f billion (%.1f%%)\n",
            ieepa_applied, 100 * ieepa_applied / sum(us_imports$us_imports_bn)))

# Determine output directory (scenario-aware)
# Baseline goes to results/, scenarios go to results/scenarios/{scenario_name}/
if (exists("SCENARIO_OUTPUT_DIR") && !is.null(SCENARIO_OUTPUT_DIR)) {
  output_dir <- SCENARIO_OUTPUT_DIR
} else if (SCENARIO_NAME != "baseline") {
  output_dir <- file.path("results", "scenarios", SCENARIO_NAME)
} else {
  output_dir <- "results"
}

# Create directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (SCENARIO_NAME != "baseline") {
  cat(sprintf("\n  [SCENARIO] Saving results to: %s\n", output_dir))
}

# -----------------------------------------------------------------------------
# Export configuration ("menu")
# -----------------------------------------------------------------------------
#
# Non-interactive (recommended):
#   EXPORT_PROFILE=standard|main_only|minimal_only|none
#   EXPORT_WRITE_GZIP=TRUE|FALSE
#
# Interactive:
#   Leave EXPORT_PROFILE empty to get an R menu prompt.
#
EXPORT_PROFILE <- Sys.getenv("EXPORT_PROFILE", unset = "")
if (EXPORT_PROFILE == "" && interactive()) {
  choice <- menu(
    c(
      "standard (main CSV + .csv.gz + minimal CSV)",
      "main_only (main CSV + .csv.gz)",
      "minimal_only (minimal CSV only)",
      "none (skip all exports)"
    ),
    title = "Choose export profile"
  )
  EXPORT_PROFILE <- c("standard", "main_only", "minimal_only", "none")[choice]
}
if (EXPORT_PROFILE == "") EXPORT_PROFILE <- "standard"
EXPORT_WRITE_GZIP <- toupper(Sys.getenv("EXPORT_WRITE_GZIP", unset = "TRUE")) %in% c("TRUE", "T", "1", "YES", "Y")
cat(sprintf("  Export profile: %s (gzip=%s)\n", EXPORT_PROFILE, ifelse(EXPORT_WRITE_GZIP, "TRUE", "FALSE")))

# Helper to call fwrite with only supported args (older data.table compatibility)
fwrite_safe <- function(x, file, sep = ";", compress = NULL) {
  fw_args <- names(formals(data.table::fwrite))
  arg_list <- list(
    x = x,
    file = file,
    sep = sep
  )

  # Prefer safe quoting/escaping for Excel + semicolon CSV
  if ("quote" %in% fw_args) arg_list$quote <- "auto"
  if ("na" %in% fw_args) arg_list$na <- ""
  if ("qmethod" %in% fw_args) arg_list$qmethod <- "double"

  # Some data.table versions support integer64 handling; only pass if available
  if ("integer64" %in% fw_args) arg_list$integer64 <- "double"

  # Some data.table versions support built-in compression
  if (!is.null(compress) && ("compress" %in% fw_args)) arg_list$compress <- compress

  do.call(data.table::fwrite, arg_list)
  invisible(TRUE)
}

# Helper to write gzipped CSV consistently (semicolon-separated)
# Returns TRUE on success, FALSE on failure (never throws)
fwrite_gzip <- function(x, file, sep = ";") {
  # Method 1: Try data.table's built-in gzip compression
  fw_args <- names(formals(data.table::fwrite))
  if ("compress" %in% fw_args) {
    result <- tryCatch({
      fwrite_safe(x, file, sep = sep, compress = "gzip")
      TRUE
    }, error = function(e) {
      # Built-in gzip failed (likely missing zlib), try fallback
      FALSE
    })
    if (result) return(invisible(TRUE))
  }

  # Method 2: Fallback to system gzip (POSIX)
  result <- tryCatch({
    tmp <- tempfile(fileext = ".csv")
    fwrite_safe(x, tmp, sep = sep)
    exit_code <- system2("gzip", args = c("-c", shQuote(tmp)), stdout = file,
                         stderr = FALSE)
    unlink(tmp)
    exit_code == 0 && file.exists(file) && file.info(file)$size > 0
  }, error = function(e) {
    FALSE
  })

  if (result) return(invisible(TRUE))

  # Both methods failed
  warning("gzip compression unavailable (zlib missing, system gzip failed)")
  invisible(FALSE)
}

# Select columns for export (same for RData and Excel)
cat("\n  Saving results...\n")
if (!("us_imports" %in% names(us_imports)) && ("us_imports_bn" %in% names(us_imports))) {
  # Keep both absolute $ value and $bn for convenience.
  # (If us_imports doesn't exist, reconstruct from bn.)
  us_imports <- us_imports %>%
    mutate(us_imports = us_imports_bn * 1e9)
}

# Ensure identifiers are complete for export
# - HS codes: keep as 8-digit strings (preserve leading zeros)
# - ISO-3 codes: fill a small set of known missing territories from exporter name
us_imports <- us_imports %>%
  mutate(
    hs_8digit = sprintf("%08s", as.character(hs_8digit))
  )

iso3_fixes <- c(
  # Common territories / special cases seen in US import data
  "Christmas Island" = "CXR",
  "Cocos (Keeling) Islands" = "CCK",
  "Turks and Caicos Islands" = "TCA",
  "Antigua and Barbuda" = "ATG",
  "Montserrat" = "MSR",
  "Gibraltar" = "GIB",
  "British Indian Ocean Territory" = "IOT",
  "French Southern and Antarctic Lands" = "ATF",
  "Svalbard and Jan Mayen" = "SJM",
  "Vatican City" = "VAT",
  "Wallis and Futuna" = "WLF",
  "Heard and McDonald Islands" = "HMD",
  "Pitcairn Islands" = "PCN",
  "Macau" = "MAC",
  "Kosovo" = "XKX",
  "Curaçao" = "CUW",
  "CuraÃ§ao" = "CUW",
  "São Tomé and Príncipe" = "STP",
  "SÃ£o TomÃ© and PrÃ­ncipe" = "STP",
  "Saint Pierre and Miquelon" = "SPM",
  "North Korea" = "PRK",
  "Sudan" = "SDN",
  # Palestine (UN M49: 275) – use ISO3 PSE for both West Bank and Gaza Strip
  "West Bank" = "PSE",
  "Gaza Strip" = "PSE",
  "Cabo Verde" = "CPV",
  "Faroe Islands" = "FRO",
  "Sint Maarten" = "SXM",
  "Reunion" = "REU",
  "Saint Kitts and Nevis" = "KNA",
  "Saint Vincent and the Grenadines" = "VCT",
  "Republic of the Congo" = "COG"
)

us_imports <- us_imports %>%
  mutate(
    iso_code = ifelse(
      is.na(iso_code) | iso_code == "",
      dplyr::recode(exporter, !!!iso3_fixes, .default = NA_character_),
      iso_code
    )
  )

missing_iso3_n <- sum(is.na(us_imports$iso_code) | us_imports$iso_code == "")
if (missing_iso3_n > 0) {
  missing_examples <- us_imports %>%
    filter(is.na(iso_code) | iso_code == "") %>%
    distinct(exporter) %>%
    head(20)
  cat(sprintf("WARNING: %s rows still missing ISO-3 codes (showing up to 20 exporters)\n", missing_iso3_n))
  print(missing_examples)
}
us_imports <- us_imports %>%
  select(exporter, iso_code, hs_8digit,
         us_imports, us_imports_bn,
         hts_rate, s232_rate, ieepa_rate, emergency_rate, s301_rate, country_surcharge_rate, rate, rate_formula,
        # Contribution columns (sum = rate)
        hts_contrib, s232_contrib, ieepa_contrib, emergency_contrib, s301_contrib, country_surcharge_contrib,
        # USMCA-weighted rates (intermediate values before content/aircraft weighting)
        hts_rate_weighted, s232_rate_weighted, ieepa_rate_weighted, emergency_rate_weighted,
        # Composition weighting factors
        usmca_compliance, content_share, aircraft_share,
        # Boolean markers
        s232_auto, s232_auto_parts, s232_mhdv, s232_pv_parts,
        s232_steel, s232_steel_derivative,
        s232_alu, s232_alu_derivative,
        s232_copper, s232_copper_derivative,
        s232_lumber, s232_lumber_derivative,
        rr_exception, ieepa_statute_exception, missing_mfn,
        bra_exception, bra_aircraft, bra_vehicle,
        eu_exception, eu_pharma, uk_aircraft, uk_auto,
        jpn_lumber_derivative, eu_lumber_derivative,
        kor_floor, kor_lumber_derivative, kor_aircraft, kor_auto,
        che_floor, che_aircraft, che_exception, che_pharma,
        arg_exception, arg_pharma, arg_aircraft,
        twn_floor, twn_aircraft, twn_exception, twn_pharma,
        slv_exception, slv_pharma, slv_aircraft, slv_cafta,
        gtm_exception, gtm_pharma, gtm_aircraft, gtm_cafta,
        wto_aircraft, chn_opioid, sec301_tariff, can_northern_border, can_northern_border_energy, mex_emergency) %>%
  as.data.frame()

# Build output filename with MFN rate source and scenario name
# MFN rate source suffix: hts_schedule -> schedule, mixed_schedule -> mixed, etc.
mfn_suffix <- switch(MFN_RATE_SOURCE,
  "hts_schedule" = "schedule24",
  "mixed_schedule" = "mixed24",
  "observed_product" = "product24",
  "observed_bilateral" = "bilateral24",
  "baseline_boe" = "boe24",
  "schedule-error"  # fallback
)

# Add option indicators if non-default
option_suffix <- ""
if (DISABLE_HTS_USMCA_WEIGHTING) option_suffix <- paste0(option_suffix, "_nousmca")
if (DISABLE_HTS_KORUS_WEIGHTING) option_suffix <- paste0(option_suffix, "_nokorus")
if (EXCLUDE_S301_TARIFFS) option_suffix <- paste0(option_suffix, "_nos301")

output_basename <- paste0("processed_us_imports_with_rates_", mfn_suffix, option_suffix, "_", SCENARIO_NAME)

# Save RData (same columns as Excel)
save(us_imports, file = file.path(output_dir, paste0(output_basename, ".RData")))
cat(sprintf("    Saved: %s/%s.RData\n", output_dir, output_basename))

# Export to Excel (sorted by import value)
export_data <- us_imports %>%
  arrange(desc(us_imports_bn))

write_xlsx(list("US Imports with Tariff Rates" = export_data),
           file.path(output_dir, paste0(output_basename, ".xlsx")))
cat(sprintf("    Saved: %s/%s.xlsx\n", output_dir, output_basename))

# Export to CSV(s)
#
# IMPORTANT: never "slice" CSVs with shell tools (e.g., cut) because that breaks
# quoting rules when text fields contain ';' or quotes.
#
csv_path <- file.path(output_dir, paste0(output_basename, ".csv"))
minimal_path <- file.path(output_dir, paste0(output_basename, "_minimal.csv"))
gzip_path <- paste0(csv_path, ".gz")

if (EXPORT_PROFILE %in% c("standard", "main_only")) {
  fwrite_safe(export_data, csv_path, sep = ";")
  cat(sprintf("    Saved: %s/%s.csv\n", output_dir, output_basename))

  if (EXPORT_WRITE_GZIP) {
    gzip_success <- fwrite_gzip(export_data, gzip_path, sep = ";")
    if (gzip_success) {
      cat(sprintf("    Saved: %s/%s.csv.gz\n", output_dir, output_basename))
    } else {
      cat(sprintf("    Skipped: %s/%s.csv.gz (gzip unavailable)\n", output_dir, output_basename))
    }
  }
}

if (EXPORT_PROFILE %in% c("standard", "minimal_only")) {
  # Minimal output: just identifiers and final rate
  minimal_data <- export_data %>%
    select(iso_code, hs_8digit, us_imports, rate)
  fwrite_safe(minimal_data, minimal_path, sep = ";")
  cat(sprintf("    Saved: %s/%s_minimal.csv\n", output_dir, output_basename))

  # Decomposed output: weighted rates + contribution columns (sum of contrib = rate)
  decomposed_path <- file.path(output_dir, paste0(output_basename, "_decomposed.csv"))
  decomposed_data <- export_data %>%
    select(iso_code, hs_8digit, us_imports,
           # Weighted rates (after USMCA weighting, before content/aircraft weighting)
           hts_rate_weighted, s232_rate_weighted, ieepa_rate_weighted,
           emergency_rate_weighted, s301_rate, country_surcharge_rate,
           # Contribution to final rate (sum = rate)
           hts_contrib, s232_contrib, ieepa_contrib, emergency_contrib, s301_contrib, country_surcharge_contrib,
           rate)
  fwrite_safe(decomposed_data, decomposed_path, sep = ";")
  cat(sprintf("    Saved: %s/%s_decomposed.csv\n", output_dir, output_basename))
}

cat("\n")
cat("=============================================================================\n")
cat("DATA PROCESSING COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("Total import value: $%.1f billion\n", sum(us_imports$us_imports_bn)))
cat(sprintf("Weighted average applied rate: %.2f%%\n", avg_rate))
cat(sprintf("Output files saved to: %s/%s.[RData|xlsx|csv]\n", output_dir, output_basename))
cat("=============================================================================\n")

# =============================================================================
# AUTO-GENERATE SCENARIO REPORT (for non-baseline scenarios)
# =============================================================================

if (SCENARIO_NAME != "baseline") {
  cat("\n")
  cat("Generating scenario comparison report...\n")
  
  # Source the report generator
  if (file.exists("code/generate_scenario_report.R")) {
    source("code/generate_scenario_report.R")
    
    # Generate report in same folder as data (results/scenarios/{scenario_name}/)
    tryCatch({
      generate_scenario_report(
        scenario_name = SCENARIO_NAME,
        output_dir = output_dir  # Use same output_dir as data files
      )
    }, error = function(e) {
      warning(sprintf("Scenario report generation failed: %s", e$message))
    })
  } else {
    cat("  Note: generate_scenario_report.R not found - skipping report generation\n")
  }
}

cat("\n")
