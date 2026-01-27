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

# -----------------------------------------------------------------------------
# 0.2: S232 Share Parameters (Effective Incidence Ratios)
# -----------------------------------------------------------------------------
# These ratios represent effective tariff incidence, not physical metal content.
# A 40% ratio means observed duties equal 40% of what full S232 application would yield.
# The gap reflects: (a) physical content <100% for derivatives, (b) exclusions, (c) measurement.
# Calibrated from USITC 2025 trade data. See memo_metal_ratio_assumptions.md for methodology.

cat("  0.2 Setting S232 effective incidence parameters...\n")

# DERIVATIVE PRODUCTS: Share represents effective tariff burden on metal component
# NOTE: Values loaded from data/scenarios/shares.csv
steel_derivative_share <- get_share("steel_derivative_share")  # 40% - steel derivatives
alu_derivative_share <- get_share("alu_derivative_share")      # 35% - aluminum derivatives
# NOTE: No copper derivatives currently in S232 scope

# MAIN PRODUCTS: Share represents exclusion-adjusted incidence (physical content = 100%)
# Ratios <100% reflect administrative exclusions (GAE, PSE), not physical composition
steel_main_share <- get_share("steel_main_share")              # 85% - steel main products
alu_main_share <- get_share("alu_main_share")                  # 94% - aluminum main products
copper_main_share <- get_share("copper_main_share")            # 70% - copper (provisional, 2 months data)

# Lumber shares
lumber_main_share <- get_share("lumber_main_share")            # 100% - no empirical calibration available
lumber_cabinet_share <- get_share("lumber_cabinet_share")      # For kitchen cabinets/vanities

# Environment variable overrides for timeline extraction (set shares to 1.0)
# This allows extracting full theoretical tariffs without incidence ratio discounting
if (Sys.getenv("STEEL_CONTENT_SHARE") != "") {
  steel_derivative_share <- as.numeric(Sys.getenv("STEEL_CONTENT_SHARE"))
  steel_main_share <- as.numeric(Sys.getenv("STEEL_CONTENT_SHARE"))
  cat(sprintf("    [ENV OVERRIDE] steel shares = %.2f\n", steel_derivative_share))
}
if (Sys.getenv("ALU_CONTENT_SHARE") != "") {
  alu_derivative_share <- as.numeric(Sys.getenv("ALU_CONTENT_SHARE"))
  alu_main_share <- as.numeric(Sys.getenv("ALU_CONTENT_SHARE"))
  cat(sprintf("    [ENV OVERRIDE] alu shares = %.2f\n", alu_derivative_share))
}
if (Sys.getenv("COPPER_CONTENT_SHARE") != "") {
  copper_main_share <- as.numeric(Sys.getenv("COPPER_CONTENT_SHARE"))
  cat(sprintf("    [ENV OVERRIDE] copper_main_share = %.2f\n", copper_main_share))
}

cat(sprintf("    Effective incidence shares:\n"))
cat(sprintf("      Steel: derivatives=%.0f%%, main=%.0f%%\n", 
            steel_derivative_share * 100, steel_main_share * 100))
cat(sprintf("      Aluminum: derivatives=%.0f%%, main=%.0f%%\n", 
            alu_derivative_share * 100, alu_main_share * 100))
cat(sprintf("      Copper: main=%.0f%% (provisional)\n", copper_main_share * 100))
cat(sprintf("      Lumber: main=%.0f%%, cabinets=%.0f%%\n", lumber_main_share * 100, lumber_cabinet_share * 100))

# -----------------------------------------------------------------------------
# 0.3: S232 Share Parameters (End-Use Shares)
# -----------------------------------------------------------------------------

cat("  0.3 Setting S232 end-use share parameters...\n")

# For parts where tariff applies to share destined for specific end-use
# NOTE: Values loaded from data/scenarios/shares.csv
mhdv_share_in_parts <- get_share("mhdv_share_in_parts")  # Share of parts destined for MHDV assembly
pv_share_in_parts <- get_share("pv_share_in_parts")      # Share destined for passenger vehicle/light truck assembly

cat(sprintf("    End-use shares: MHDV=%.0f%%, PV=%.0f%%\n",
            mhdv_share_in_parts * 100, pv_share_in_parts * 100))

# -----------------------------------------------------------------------------
# 0.4: USMCA Compliance Parameters
# -----------------------------------------------------------------------------

cat("  0.4 Setting USMCA compliance parameters...\n")

# USMCA compliance uses PRODUCT-LEVEL rates from data/compliance_shares.csv
# Loaded and merged in Section 2.4. Default from shares.csv for unmatched products.
# The energy exception list (can_northern_border_energy) only determines the base
# emergency rate (10% vs 35%), NOT the compliance rate.

# USMCA Preferential HTS Rate
# USMCA-compliant products receive preferential (zero) HTS rate instead of standard MFN
usmca_rate <- get_rate("usmca_rate")  # USMCA preferential HTS rate (loaded from rates.csv)

cat("    USMCA compliance: Product-level rates from compliance_shares.csv\n")
cat(sprintf("    Default for unmatched products: %.0f%% (from shares.csv)\n", 
            get_share("usmca_compliance_default") * 100))
cat(sprintf("    USMCA preferential rate: %.0f%%\n", usmca_rate))

# KORUS Preferential HTS Rate (for South Korea)
# KORUS-compliant products receive preferential (zero) HTS rate instead of standard MFN
korus_rate <- get_rate("korus_rate")  # KORUS preferential HTS rate (loaded from rates.csv)
korus_compliance_default <- get_share("korus_compliance_default")  # Flat compliance rate

cat(sprintf("    KORUS compliance: %.0f%% (observed 2024 US imports)\n", korus_compliance_default * 100))
cat(sprintf("    KORUS preferential rate: %.0f%%\n", korus_rate))

# Environment variable override for USMCA compliance (for metal ratio analysis)
# Set USMCA_COMPLIANCE_OVERRIDE=0 to get full theoretical tariffs for non-USMCA imports
usmca_compliance_override <- NULL
if (Sys.getenv("USMCA_COMPLIANCE_OVERRIDE") != "") {
  usmca_compliance_override <- as.numeric(Sys.getenv("USMCA_COMPLIANCE_OVERRIDE"))
  cat(sprintf("    [ENV OVERRIDE] USMCA compliance will be set to %.0f%% in Section 2.4\n", 
              usmca_compliance_override * 100))
}

# -----------------------------------------------------------------------------
# 0.5: Conditional Exception Share Parameters (Country-Specific)
# -----------------------------------------------------------------------------

cat("  0.5 Setting conditional exception share parameters...\n")

# CIVIL AIRCRAFT SHARES (Country-Specific)
# These parameters determine the share of aircraft/parts used for civil (not military) purposes
# Applied in Section 8 rate calculation: (1 - aircraft_share) weights the entire tariff bracket
# Formula: rate = hts + (1-aircraft_share)*[materials_s232 + ieepa + emergency] + transport_s232 + s301
# NOTE: Aircraft share applies ONLY to materials S232 (steel/alu/copper/lumber), NOT transport S232

# NOTE: Values loaded from data/scenarios/shares.csv
bra_civil_aircraft_share <- get_share("bra_civil_aircraft_share")   # Brazil: Share of aircraft parts for civil use
jpn_civil_aircraft_share <- get_share("jpn_civil_aircraft_share")   # Japan: Share of aircraft parts for civil use
eu_civil_aircraft_share <- get_share("eu_civil_aircraft_share")     # EU: Share of aircraft parts for civil use
uk_civil_aircraft_share <- get_share("uk_civil_aircraft_share")     # UK: Share of aircraft parts for civil use
kor_civil_aircraft_share <- get_share("kor_civil_aircraft_share")   # Korea: Share of aircraft parts for civil use
che_civil_aircraft_share <- get_share("che_civil_aircraft_share")   # Switzerland/Liechtenstein: Share of aircraft parts for civil use

# OTHER CONDITIONAL EXCEPTIONS
bra_passenger_vehicle_share <- get_share("bra_passenger_vehicle_share")    # Brazil: Share of auto parts for passenger vehicles
uk_car_parts_uk_built_share <- get_share("uk_car_parts_uk_built_share")    # UK: Share of UK car parts in UK-built cars
uk_steel_alu_quota_share <- get_share("uk_steel_alu_quota_share")          # UK: Share of UK steel/alu within quota

# KOREA EXCEPTION PARAMETERS (November 2025 Deal)
# NOTE: Values loaded from data/scenarios/rates.csv
kor_lumber_derivative_rate <- get_rate("kor_lumber_derivative_rate")  # Korea: S232 rate for lumber cabinets/furniture
kor_s232_floor_rate <- get_rate("kor_s232_floor_rate")                # Korea: S232 auto floor rate
kor_ieepa_floor_rate <- get_rate("kor_ieepa_floor_rate")              # Korea: IEEPA floor rate (reciprocal tariffs)

# SWITZERLAND/LIECHTENSTEIN EXCEPTION PARAMETERS
che_ieepa_floor_rate <- get_rate("che_ieepa_floor_rate")              # Switzerland/Liechtenstein: IEEPA floor rate
che_nonpatented_pharma_share <- get_share("che_nonpatented_pharma_share")   # Switzerland: Share of pharma imports not patented in US (exempt from IEEPA)

# EU PHARMA EXCEPTION PARAMETER
eu_nonpatented_pharma_share <- get_share("eu_nonpatented_pharma_share")     # EU: Share of pharma imports not patented in US (exempt from IEEPA)

cat(sprintf("    Civil aircraft shares: Brazil=%.0f%%, Japan=%.0f%%, EU=%.0f%%, UK=%.0f%%, Korea=%.0f%%, Switzerland=%.0f%%\n",
            bra_civil_aircraft_share * 100, jpn_civil_aircraft_share * 100,
            eu_civil_aircraft_share * 100, uk_civil_aircraft_share * 100,
            kor_civil_aircraft_share * 100, che_civil_aircraft_share * 100))
cat(sprintf("    Korea parameters: floor=%.0f%%, auto_floor=%.0f%%, lumber_rate=%.0f%%\n",
            kor_ieepa_floor_rate, kor_s232_floor_rate, kor_lumber_derivative_rate))
cat(sprintf("    Switzerland/Liechtenstein parameters: floor=%.0f%%, non-patented pharma=%.0f%%\n",
            che_ieepa_floor_rate, che_nonpatented_pharma_share * 100))
cat(sprintf("    EU non-patented pharma share: %.0f%%\n", eu_nonpatented_pharma_share * 100))

# -----------------------------------------------------------------------------
# 0.6: Annex 2 Version Configuration
# -----------------------------------------------------------------------------

cat("  0.6 Setting annex 2 version...\n")

# ANNEX 2 VERSION SELECTION (versioned CSVs for policy reconstruction)
# NOTE: policy_date is already set in Section 0.0 for time-dependent rate lookups
annex2_version <- "nov13"  # Options: "apr2", "semiconductors", "aug1", "nov13"

cat(sprintf("    Annex 2 version: %s\n", annex2_version))
cat(sprintf("    Policy date: %s (set in Section 0.0)\n", policy_date))

cat("\n  Section 0 complete: All parameters configured\n")
cat("\n")

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
  s232_auto = 0, s232_mhdv = 0, s232_pv_parts = 0,
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
  wto_aircraft = 0,
  chn_opioid = 0,
  sec301_tariff = 0,
  can_northern_border = 0,
  can_northern_border_energy = 0,
  mex_emergency = 0
)]

cat(sprintf("    Initialized %d rate columns and %d boolean marker columns\n", 13, 30))

cat("\n  Section 1 complete: Data loaded and scoped\n")
cat("\n")

# =============================================================================
# SECTION 2: MFN RATES (Populate hts_rate column)
# =============================================================================
# Establish base tariff rates including USMCA preferential treatment (Layer 1)
# =============================================================================

cat("SECTION 2: MFN Rates\n")
cat("--------------------\n")

# -----------------------------------------------------------------------------
# 2.1: Load MFN Rates from Database
# -----------------------------------------------------------------------------

cat(sprintf("  2.1 Loading MFN rates (source: %s)...\n", MFN_RATE_SOURCE))

# Extract rates for selected source from MFN_RATES_DB
# For product-level sources: un_code is NA
# For bilateral sources (observed_bilateral, baseline_boe): un_code is populated
is_bilateral_source <- (MFN_RATE_SOURCE %in% c("observed_bilateral", "baseline_boe"))

if (is_bilateral_source) {
  # Bilateral source: extract rates with un_code populated
  hts_rates <- MFN_RATES_DB[source_key == MFN_RATE_SOURCE & !is.na(un_code),
                            .(hs_8digit, un_code, mfn_rate = rate)]
  cat(sprintf("    Loaded %s bilateral MFN rates (product x country)\n",
              format(nrow(hts_rates), big.mark = ",")))

  # Prepare product-level fallback
  # For observed_bilateral: fall back to observed_product
  # For baseline_boe: fall back to product-level baseline_boe rates
  fallback_source <- if (MFN_RATE_SOURCE == "observed_bilateral") "observed_product" else MFN_RATE_SOURCE
  hts_rates_fallback <- MFN_RATES_DB[source_key == fallback_source & is.na(un_code),
                                      .(hs_8digit, mfn_rate_fallback = rate)]
  cat(sprintf("    Loaded %s product-level fallback rates\n",
              format(nrow(hts_rates_fallback), big.mark = ",")))
} else {
  # Product-level source: extract rates where un_code is NA
  hts_rates <- MFN_RATES_DB[source_key == MFN_RATE_SOURCE & is.na(un_code),
                            .(hs_8digit, mfn_rate = rate)]
  cat(sprintf("    Loaded %s MFN rates (product-level)\n",
              format(nrow(hts_rates), big.mark = ",")))
}

# -----------------------------------------------------------------------------
# 2.2: Identify and Handle Missing MFN Codes
# -----------------------------------------------------------------------------

cat("\n  2.2 Checking for missing MFN codes...\n")

# Identify HS codes in imports that are missing from MFN schedule
unique_import_hs <- unique(us_imports$hs_8digit)
if (is_bilateral_source) {
  unique_mfn_hs <- unique(hts_rates$hs_8digit)  # Product dimension only
} else {
  unique_mfn_hs <- unique(hts_rates$hs_8digit)
}
missing_hs_codes <- setdiff(unique_import_hs, unique_mfn_hs)

if(length(missing_hs_codes) > 0) {
  missing_import_value <- sum(us_imports[hs_8digit %in% missing_hs_codes]$us_imports_bn)
  missing_import_pct <- 100 * missing_import_value / sum(us_imports$us_imports_bn)

  cat(sprintf("    WARNING: %d HS8 codes in import data are missing from MFN schedule\n",
              length(missing_hs_codes)))
  cat(sprintf("    Import value affected: $%.2f billion (%.2f%% of total)\n",
              missing_import_value, missing_import_pct))

  # Create data table of missing codes with import values
  missing_codes_detail <- us_imports[hs_8digit %in% missing_hs_codes,
                                     .(import_value = sum(us_imports_bn),
                                       n_flows = .N),
                                     by = .(hs_8digit, hs_8digit_name)][order(-import_value)]

  # Save missing codes list for documentation
  fwrite(missing_codes_detail, "results/mfn_missing_codes_log.csv")
  cat("    Saved missing codes list to: results/mfn_missing_codes_log.csv\n")

  # Apply assumption: Create MFN entries with 0% rate for missing codes
  cat(sprintf("    ASSUMPTION: Setting MFN rate = 0%% for %d missing codes\n",
              length(missing_hs_codes)))

  if (is_bilateral_source) {
    # For bilateral: add missing codes to fallback table
    missing_mfn_entries <- data.table(
      hs_8digit = missing_hs_codes,
      mfn_rate_fallback = 0
    )
    hts_rates_fallback <- rbind(hts_rates_fallback, missing_mfn_entries, fill = TRUE)
  } else {
    missing_mfn_entries <- data.table(
      hs_8digit = missing_hs_codes,
      mfn_rate = 0
    )
    hts_rates <- rbind(hts_rates, missing_mfn_entries, fill = TRUE)
  }
} else {
  cat("    All HS8 codes in import data have MFN rates (100% coverage)\n")
}

# -----------------------------------------------------------------------------
# 2.3: Merge MFN Rates with Imports
# -----------------------------------------------------------------------------

cat("\n  2.3 Merging MFN rates with US imports...\n")

if (is_bilateral_source) {
  # Bilateral merge: join on both hs_8digit and un_code
  us_imports <- merge(us_imports, hts_rates,
                      by = c("hs_8digit", "un_code"), all.x = TRUE)
  
  # Apply fallback for missing bilateral combinations
  us_imports <- merge(us_imports, hts_rates_fallback,
                      by = "hs_8digit", all.x = TRUE)
  
  # Use bilateral rate where available, fallback otherwise
  us_imports[, hts_rate := ifelse(!is.na(mfn_rate), mfn_rate, mfn_rate_fallback)]
  us_imports[, c("mfn_rate", "mfn_rate_fallback") := NULL]
  
  # Count how many used fallback
  n_fallback <- sum(is.na(us_imports$hts_rate) | us_imports$hts_rate == 0)
  cat(sprintf("    Bilateral rates matched: %s flows\n", 
              format(sum(!is.na(us_imports$hts_rate)), big.mark = ",")))
} else {
  # Product-level merge: join on hs_8digit only
  us_imports <- merge(us_imports, hts_rates[, .(hs_8digit, mfn_rate)],
                      by = "hs_8digit", all.x = TRUE, suffixes = c("", "_mfn"))
  
  # Rename to hts_rate for consistency with naming convention
  setnames(us_imports, "mfn_rate", "hts_rate")
}

# Mark products where MFN was missing (for transparency)
if(length(missing_hs_codes) > 0) {
  us_imports[hs_8digit %in% missing_hs_codes, missing_mfn := 1]
}

# Verify no missing values remain
if(any(is.na(us_imports$hts_rate))) {
  stop("ERROR: Missing MFN rates after merge. This should not happen.")
}

cat("    MFN rates successfully merged\n")

# Calculate weighted average MFN rate
avg_mfn <- weighted.mean(us_imports$hts_rate, us_imports$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Weighted average MFN rate: %.2f%%\n", avg_mfn))

# -----------------------------------------------------------------------------
# 2.4: Merge Product-Level USMCA Compliance Rates
# -----------------------------------------------------------------------------

cat("\n  2.4 Merging product-level USMCA compliance rates...\n")

# Merge compliance rates by HS8 and country
us_imports <- merge(us_imports, 
                    compliance_shares[, .(hs_8digit, import_origin, compliance_rate)],
                    by.x = c("hs_8digit", "iso_code"),
                    by.y = c("hs_8digit", "import_origin"),
                    all.x = TRUE)

# Get default compliance rate from shares.csv
usmca_compliance_default <- get_share("usmca_compliance_default")

# Create usmca_compliance column:
# - Use product-level rate if available
# - Default to usmca_compliance_default for unmatched USMCA products
# - NA for non-USMCA countries (compliance not applicable)
us_imports[, usmca_compliance := fifelse(
  iso_code %in% c("CAN", "MEX") & !is.na(compliance_rate), compliance_rate,
  fifelse(iso_code %in% c("CAN", "MEX"), usmca_compliance_default, NA_real_))]

# Report compliance merge statistics
can_matched <- us_imports[iso_code == "CAN" & !is.na(compliance_rate), .N]
can_total <- us_imports[iso_code == "CAN", .N]
mex_matched <- us_imports[iso_code == "MEX" & !is.na(compliance_rate), .N]
mex_total <- us_imports[iso_code == "MEX", .N]

cat(sprintf("    Canada: %s of %s products matched (%.1f%%), unmatched default to %.0f%%\n",
            format(can_matched, big.mark = ","), format(can_total, big.mark = ","),
            100 * can_matched / can_total, usmca_compliance_default * 100))
cat(sprintf("    Mexico: %s of %s products matched (%.1f%%), unmatched default to %.0f%%\n",
            format(mex_matched, big.mark = ","), format(mex_total, big.mark = ","),
            100 * mex_matched / mex_total, usmca_compliance_default * 100))

# Report weighted average compliance rates
can_avg <- weighted.mean(us_imports[iso_code == "CAN", usmca_compliance], 
                         us_imports[iso_code == "CAN", us_imports_bn], na.rm = TRUE)
mex_avg <- weighted.mean(us_imports[iso_code == "MEX", usmca_compliance], 
                         us_imports[iso_code == "MEX", us_imports_bn], na.rm = TRUE)
cat(sprintf("    Trade-weighted compliance: Canada=%.1f%%, Mexico=%.1f%%\n", 
            can_avg * 100, mex_avg * 100))

# Apply USMCA compliance override if set (for metal ratio analysis)
# This forces compliance to a fixed value (typically 0%) to get full theoretical tariffs
if (!is.null(usmca_compliance_override)) {
  us_imports[iso_code %in% c("CAN", "MEX"), usmca_compliance := usmca_compliance_override]
  cat(sprintf("    [ENV OVERRIDE] Forced USMCA compliance to %.0f%% for Canada/Mexico\n", 
              usmca_compliance_override * 100))
}

# Clean up temporary column
us_imports[, compliance_rate := NULL]

# -----------------------------------------------------------------------------
cat("\n  Section 2 complete: MFN rates and USMCA compliance populated\n")
cat("\n")

# =============================================================================
# SECTION 3: SECTION 232 RATES (Populate s232_rate column)
# =============================================================================
# Calculate S232 rates WITHOUT country exceptions
# UK, EU, Japan get standard rates here; exceptions applied in Section 7
#
# KEY PRINCIPLES:
# 1. AUTO and MHDV S232 take HIGHEST PRECEDENCE
# 2. PV Parts S232 takes THIRD PRECEDENCE
# 3. Main products vs derivatives: Main=100%, Derivatives=content share
# 4. Country-specific rate caps (UK, Japan, EU) applied in Section 7
# 5. S232 rates REPLACE reciprocal rates (precedence in Section 8)
# =============================================================================

cat("SECTION 3: Section 232 Tariffs\n")
cat("-------------------------------\n")

# -----------------------------------------------------------------------------
# 3.1: AUTO S232 (Highest Precedence)
# -----------------------------------------------------------------------------

cat("  3.1 Auto S232 (25% or 15% floor) - Highest precedence...\n")

# S232 Auto Rate Parameter (loaded from rates.csv)
s232_auto_rate <- get_rate("s232_auto_rate")  # Automobile and light truck standard rate

# Load auto S232 products (from exceptions.csv)
sec232_auto_codes <- get_exceptions("s232_auto_rate", "product_scope", policy_date)

# Mark all auto products (using parameter from Section 0.3a; Japan/EU get 15% floor in Section 7)
us_imports[hs_8digit %in% sec232_auto_codes, `:=`(
  s232_auto = 1,
  s232_rate = s232_auto_rate  # Standard rate from parameters; floor countries handled in Section 7
)]

auto_value <- sum(us_imports[s232_auto == 1]$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Applied auto S232 to %d HS8 codes ($%.1f billion)\n",
            length(sec232_auto_codes), auto_value))
cat(sprintf("    Standard rate: %.0f%% (Japan/EU 15%% floor applied in Section 7; USMCA compliance in Section 8)\n",
            s232_auto_rate))

# -----------------------------------------------------------------------------
# 3.2: MHDV S232 (Second Precedence) - WITH NESTED USMCA LOGIC
# -----------------------------------------------------------------------------

cat("\n  3.2 MHDV S232 (25%/10%/0%) - Second precedence...\n")

# S232 MHDV Rate Parameters (loaded from rates.csv)
s232_mhdv_main_rate <- get_rate("s232_mhdv_main_rate")  # MHDV main products (trucks, tractors)
s232_mhdv_bus_rate <- get_rate("s232_mhdv_bus_rate")    # Buses
s232_mhdv_parts_rate <- get_rate("s232_mhdv_parts_rate")  # Parts for MHDV

# Load MHDV S232 products (from exceptions.csv)
mhdv_main <- get_exceptions("s232_mhdv_main_rate", "product_scope", policy_date)
bus_codes <- get_exceptions("s232_mhdv_bus_rate", "product_scope", policy_date)
parts_codes <- get_exceptions("s232_mhdv_parts_rate", "product_scope", policy_date)

# Apply STANDARD rates to ALL countries (using parameters from Section 0.3a)
# Country-specific adjustments handled in Section 7

# MHDV main products - ALL COUNTRIES
us_imports[hs_8digit %in% mhdv_main & s232_auto == 0, `:=`(
  s232_mhdv = 1,
  s232_rate = s232_mhdv_main_rate
)]

# Bus products - ALL COUNTRIES
us_imports[hs_8digit %in% bus_codes & s232_auto == 0, `:=`(
  s232_mhdv = 1,
  s232_rate = s232_mhdv_bus_rate
)]

# Parts products - ALL COUNTRIES
us_imports[hs_8digit %in% parts_codes & s232_auto == 0, `:=`(
  s232_mhdv = 1,
  s232_rate = s232_mhdv_parts_rate
)]

# NOTE: Country-specific overrides (USMCA, UK, Japan, EU) applied in Section 7

mhdv_value <- sum(us_imports[s232_mhdv == 1]$us_imports_bn, na.rm = TRUE)
mhdv_all_codes <- unique(c(mhdv_main, bus_codes, parts_codes))
cat(sprintf("    Applied MHDV S232 to %d HS8 codes ($%.1f billion)\n",
            length(mhdv_all_codes), mhdv_value))

# -----------------------------------------------------------------------------
# 3.3: PASSENGER VEHICLE PARTS S232 (Third Precedence)
# -----------------------------------------------------------------------------

cat("\n  3.3 Passenger Vehicle Parts S232 (25%) - Third precedence...\n")

# S232 PV Parts Rate Parameter (loaded from rates.csv)
s232_pv_parts_rate <- get_rate("s232_pv_parts_rate")  # Passenger vehicle parts standard rate

# Load PV parts (from exceptions.csv)
# "Automobile parts include engines, transmissions, powertrain parts, electrical components."
# https://content.govdelivery.com/bulletins/gd/USDHSCBP-3f93b75
sec232_pv_parts_codes <- get_exceptions("s232_pv_parts_rate", "product_scope", policy_date)

if (length(sec232_pv_parts_codes) > 0) {
  # Exclusion logic (Chapters 72, 73, 76 NOT subject to PV parts S232)
  excluded_by_chapter <- us_imports[, substr(hs_8digit, 1, 2) %in% c("72", "73", "76")]

  # Apply to eligible products
  us_imports[hs_8digit %in% sec232_pv_parts_codes &
             !excluded_by_chapter &
             s232_auto == 0 &
             s232_mhdv == 0, `:=`(
    s232_pv_parts = 1,
    s232_rate = s232_pv_parts_rate
  )]

  cat(sprintf("    Applied PV parts S232 to %d HS8 codes\n",
              length(sec232_pv_parts_codes)))
} else {
  cat("    PV parts S232: Empty HS list, structure ready for future\n")
}

cat("\n  Sections 3.1-3.3 complete: Auto/MHDV/PV S232 applied\n")
cat("\n")

# -----------------------------------------------------------------------------
# 3.4: STEEL S232 (50% standard, 25% UK) - WITH CORRECTED COMPOSITES
# -----------------------------------------------------------------------------

cat("  3.4 Steel S232 (50% or 25%) - On products not subject to auto/MHDV/PV...\n")

# S232 Steel Rate Parameter (loaded from rates.csv)
s232_steel_rate <- get_rate("s232_steel_rate")  # Steel standard rate (UK cap at 25% in Section 7)

# Load steel S232 products (from exceptions.csv)
# Time-dependent scope: main steel and derivatives have different effective dates
steel_main <- get_exceptions("s232_steel_rate", "product_scope", policy_date)
steel_derivatives <- get_exceptions("s232_steel_rate", "product_scope_derivative", policy_date)

# Main steel products (excluded from higher-precedence S232s)
us_imports[hs_8digit %in% steel_main &
           s232_auto == 0 &
           s232_mhdv == 0 &
           s232_pv_parts == 0, `:=`(
  s232_steel = 1,
  s232_rate = s232_steel_rate  # UK cap at 25% applied in Section 7
)]

# Steel derivatives (tariff applies to steel content share)
us_imports[hs_8digit %in% steel_derivatives &
           s232_auto == 0 &
           s232_mhdv == 0 &
           s232_pv_parts == 0, `:=`(
  s232_steel_derivative = 1,
  s232_rate = s232_steel_rate  # Full rate for fully in-scope product
)]

steel_value <- sum(us_imports[s232_steel == 1 | s232_steel_derivative == 1]$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Applied steel S232 to %d main + %d derivatives ($%.1f billion)\n",
            sum(us_imports$s232_steel), sum(us_imports$s232_steel_derivative), steel_value))

# -----------------------------------------------------------------------------
# 3.5: ALUMINUM S232 (50% standard, 25% UK)
# -----------------------------------------------------------------------------

cat("\n  3.5 Aluminum S232 (50%) - On products not subject to auto/MHDV/PV...\n")

# S232 Aluminum Rate Parameter (loaded from rates.csv)
s232_alu_rate <- get_rate("s232_alu_rate")  # Aluminum standard rate (UK cap at 25% in Section 7)

# Load aluminum S232 products (from exceptions.csv)
# Time-dependent scope: main aluminum and derivatives have different effective dates
alu_main <- get_exceptions("s232_alu_rate", "product_scope", policy_date)
alu_derivatives <- get_exceptions("s232_alu_rate", "product_scope_derivative", policy_date)

# Main aluminum products
us_imports[hs_8digit %in% alu_main &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_alu = 1,
  s232_rate = s232_alu_rate  # UK cap at 25% in Section 7
)]

# Aluminum derivatives
us_imports[hs_8digit %in% alu_derivatives &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_alu_derivative = 1,
  s232_rate = s232_alu_rate  # Full rate for fully in-scope product
)]

alu_value <- sum(us_imports[s232_alu == 1 | s232_alu_derivative == 1]$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Applied aluminum S232 to %d main + %d derivatives ($%.1f billion)\n",
            sum(us_imports$s232_alu), sum(us_imports$s232_alu_derivative), alu_value))

# -----------------------------------------------------------------------------
# 3.6: COPPER S232 (50%)
# -----------------------------------------------------------------------------

cat("\n  3.6 Copper S232 (50%) - On products not subject to auto/MHDV/PV...\n")

# S232 Copper Rate Parameter (loaded from rates.csv)
s232_copper_rate <- get_rate("s232_copper_rate")  # Copper standard rate

# Load copper S232 products (from exceptions.csv)
copper_main <- get_exceptions("s232_copper_rate", "product_scope", policy_date)

# NOTE: Derivatives NOT currently implemented
copper_derivatives <- character(0)  # Empty - derivatives commented out below

us_imports[hs_8digit %in% copper_main &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_copper = 1,
  s232_rate = s232_copper_rate
)]

# Copper derivatives
# us_imports[hs_8digit %in% copper_derivatives &
#            s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
#   s232_copper_derivative = 1,
#   s232_rate = s232_copper_rate  # Full rate for fully in-scope product
# )]

copper_value <- sum(us_imports[s232_copper == 1 | s232_copper_derivative == 1]$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Applied copper S232 to %d main + %d derivatives ($%.1f billion)\n",
            sum(us_imports$s232_copper), sum(us_imports$s232_copper_derivative), copper_value))

# -----------------------------------------------------------------------------
# 3.7: LUMBER S232 (10-25% variable)
# -----------------------------------------------------------------------------

cat("\n  3.7 Lumber S232 (variable by category) - On products not subject to auto/MHDV/PV...\n")

# Load rates from scenario database
s232_lumber_rate <- get_rate("s232_lumber_rate")
s232_lumber_cabinet_rate <- get_rate("s232_lumber_cabinet_rate")
s232_lumber_upholstered_rate <- get_rate("s232_lumber_upholstered_rate")

# Load lumber S232 products by category (from exceptions.csv)
lumber_main <- get_exceptions("s232_lumber_rate", "product_scope", policy_date)
lumber_cabinet <- get_exceptions("s232_lumber_cabinet_rate", "product_scope", policy_date)
lumber_upholstered <- get_exceptions("s232_lumber_upholstered_rate", "product_scope", policy_date)

# Apply lumber main products
us_imports[hs_8digit %in% lumber_main &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_lumber = 1,
  s232_rate = s232_lumber_rate
)]

# Apply kitchen cabinet products (lumber derivative)
us_imports[hs_8digit %in% lumber_cabinet &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_lumber = 1,
  s232_lumber_derivative = 1,
  s232_rate = s232_lumber_cabinet_rate
)]

# Apply upholstered furniture products
us_imports[hs_8digit %in% lumber_upholstered &
           s232_auto == 0 & s232_mhdv == 0 & s232_pv_parts == 0, `:=`(
  s232_lumber = 1,
  s232_upholstered_furniture = 1,
  s232_rate = s232_lumber_upholstered_rate
)]

lumber_value <- sum(us_imports[s232_lumber == 1]$us_imports_bn, na.rm = TRUE)
lumber_derivative_value <- sum(us_imports[s232_lumber_derivative == 1]$us_imports_bn, na.rm = TRUE)
upholstered_value <- sum(us_imports[s232_upholstered_furniture == 1]$us_imports_bn, na.rm = TRUE)
lumber_all_codes <- unique(c(lumber_main, lumber_cabinet, lumber_upholstered))
cat(sprintf("    Applied lumber S232 to %d HS8 codes ($%.1f billion)\n",
            length(lumber_all_codes), lumber_value))
cat(sprintf("    Including cabinet/vanity derivatives ($%.1f billion, %.0f%% content share)\n",
            lumber_derivative_value, lumber_cabinet_share * 100))
cat(sprintf("    Including upholstered furniture ($%.1f billion)\n", upholstered_value))

cat("\n  Section 3 complete: All S232 rates populated\n")
cat("\n")

# =============================================================================
# SECTION 4: IEEPA RATES (Populate ieepa_rate column)
# =============================================================================
# Apply IEEPA baseline + reciprocal top-ups
# Country exceptions applied in Section 7
# =============================================================================

cat("SECTION 4: IEEPA Rates (Baseline + Reciprocal)\n")
cat("------------------------------------------------\n")

# -----------------------------------------------------------------------------
# 4.1: IEEPA Baseline (10% Universal)
# -----------------------------------------------------------------------------

cat("  4.1 Adding IEEPA baseline rate (10% universal)...\n")

# IEEPA Baseline Rate Parameter (loaded from rates.csv - scenario-aware)
ieepa_baseline_rate <- get_rate("ieepa_baseline_rate")  # Universal IEEPA baseline rate
if (ieepa_baseline_rate != 10) {
  cat(sprintf("  [SCENARIO] IEEPA baseline set to %.0f%%\n", ieepa_baseline_rate))
}

# Add baseline IEEPA rate to all products (all countries initially)
# Country exceptions will modify this in Section 7
us_imports[, ieepa_rate := ieepa_baseline_rate]

total_value_affected <- sum(us_imports$us_imports_bn)
cat(sprintf("    Applied %.0f%% baseline to all imports ($%.1f billion)\n",
            ieepa_baseline_rate, total_value_affected))

# -----------------------------------------------------------------------------
# 4.2: Country-Specific Reciprocal Top-Ups
# -----------------------------------------------------------------------------

cat("\n  4.2 Applying country-specific reciprocal top-ups...\n")

# Check if scenario wants to zero all country top-ups (e.g., Section 122)
if (exists("SCENARIO_OVERRIDES") && !is.null(SCENARIO_OVERRIDES$zero_all_country_topups) &&
    SCENARIO_OVERRIDES$zero_all_country_topups == TRUE) {
  cat("  [SCENARIO] All country-specific IEEPA top-ups set to zero\n")
  # Don't add any country top-ups - baseline rate (already set) is the only IEEPA component
} else {
  # Get unique UN codes in imports
  unique_un_codes <- unique(us_imports$un_code)
  
  # Apply country-specific top-ups from rate database
  countries_with_topup <- 0
  total_value_with_topup <- 0
  
  for (un_code_val in unique_un_codes) {
    topup_rate <- get_country_topup(un_code_val, policy_date)
    if (topup_rate > 0) {
      us_imports[un_code == un_code_val,
                 ieepa_rate := ieepa_rate + topup_rate]
      countries_with_topup <- countries_with_topup + 1
      total_value_with_topup <- total_value_with_topup + 
        sum(us_imports[un_code == un_code_val]$us_imports_bn, na.rm = TRUE)
    }
  }
  
  cat(sprintf("    Applied top-ups for %d countries ($%.1f billion, as of %s)\n",
              countries_with_topup, total_value_with_topup, policy_date))
}

# Apply country-specific IEEPA overrides from scenario (if defined)
if (exists("SCENARIO_OVERRIDES") && !is.null(SCENARIO_OVERRIDES$country_ieepa_overrides)) {
  for (un_code_str in names(SCENARIO_OVERRIDES$country_ieepa_overrides)) {
    un_code_num <- as.integer(un_code_str)
    new_topup <- SCENARIO_OVERRIDES$country_ieepa_overrides[[un_code_str]]
    # Get country name for logging
    country_name <- unique(us_imports[un_code == un_code_num]$exporter)[1]
    if (is.na(country_name)) country_name <- paste0("UN=", un_code_num)
    # Override: set IEEPA to baseline + scenario top-up (replaces database top-up)
    us_imports[un_code == un_code_num,
               ieepa_rate := ieepa_baseline_rate + new_topup]
    cat(sprintf("  [SCENARIO] %s IEEPA top-up overridden to %.0f%% (total: %.0f%%)\n",
                country_name, new_topup, ieepa_baseline_rate + new_topup))
  }
}

# -----------------------------------------------------------------------------
# 4.3: Product Scope - Chapter 98 Exclusion
# -----------------------------------------------------------------------------

cat("\n  4.3 Applying Chapter 98 exclusion...\n")

# Chapter 98 codes that remain subject to reciprocal rates
chapter_98_exceptions <- c("98020080", "98020040", "98020050", "98020060")

# Count Chapter 98 flows
ch98_excluded <- nrow(us_imports[as.numeric(hs_8digit) >= 98000000 &
                                !hs_8digit %in% chapter_98_exceptions])

# Exclude Chapter 98 from reciprocal rates (reset ieepa_rate to 0 and mark as exception)
us_imports[as.numeric(hs_8digit) >= 98000000 &
           !hs_8digit %in% chapter_98_exceptions, `:=`(
           ieepa_rate = 0,
           rr_exception = 1
)]

cat(sprintf("    Excluded %s Chapter 98 flows from reciprocal rates\n",
            format(ch98_excluded, big.mark = ",")))
cat(sprintf("    (4 specific codes remain subject: %s)\n",
            paste(chapter_98_exceptions, collapse = ", ")))

# -----------------------------------------------------------------------------
# 4.4: Product Scope - Annex 2 Exceptions (VERSIONED)
# -----------------------------------------------------------------------------

cat("\n  4.4 Applying Annex 2 product exceptions...\n")

# Load Annex 2 exceptions (date-aware via get_exceptions)
# Note: get_exceptions() handles version selection and lumber removal automatically
# via effective_date/end_date fields in exceptions.csv
# Load Annex II exceptions (time-dependent via effective_date and end_date)
# Timeline covers all versions: Apr 5 initial, Sep 5 additions, Nov 13 additions
# End dates handle: Sep 4 removals, Oct 13 lumber removal
annex2_codes <- get_exceptions("ieepa_baseline_rate", "annex2", policy_date)

# Load semiconductor codes (identified by "Semiconductor" in category)
semiconductor_codes <- get_exceptions("ieepa_baseline_rate", "semiconductor", policy_date)

# Combine into single vector for reciprocal exceptions
reciprocal_exception_codes <- unique(c(annex2_codes, semiconductor_codes))

cat(sprintf("    Annex II exceptions: %d HS8 codes (policy_date: %s)\n",
            length(annex2_codes), format(policy_date, "%Y-%m-%d")))
cat(sprintf("    Semiconductor additions: %d HS8 codes\n",
            length(semiconductor_codes)))
cat(sprintf("    Total reciprocal exceptions: %d HS8 codes\n",
            length(reciprocal_exception_codes)))
cat(sprintf("    Total active exceptions: %d HS8 codes\n",
            length(reciprocal_exception_codes)))

# Apply Annex 2 exceptions (reset ieepa_rate to 0)
us_imports[hs_8digit %in% reciprocal_exception_codes, `:=`(
  ieepa_rate = 0,
  rr_exception = 1
)]

exception_flows <- sum(us_imports$rr_exception)
exception_value <- sum(us_imports[rr_exception == 1]$us_imports_bn, na.rm = TRUE)

cat(sprintf("    Applied Annex 2 exceptions to %s flows ($%.1f billion)\n",
            format(exception_flows, big.mark = ","), exception_value))

# -----------------------------------------------------------------------------
# 4.5: Statutory IEEPA Exceptions
# -----------------------------------------------------------------------------

cat("\n  4.5 Applying statutory IEEPA exceptions...\n")

# Load statutory IEEPA exceptions (from exceptions.csv)
ieepa_statute_codes <- get_exceptions("ieepa_baseline_rate", "statutory", policy_date)

cat(sprintf("    Loaded %d HS8 codes with statutory IEEPA exceptions\n",
            length(ieepa_statute_codes)))

# Apply statutory IEEPA exceptions (reset ieepa_rate AND emergency_rate to 0)
# These products are excluded from BOTH IEEPA baseline/reciprocal AND all emergency orders
# Use separate marker (ieepa_statute_exception) to distinguish from Annex 2 exceptions
us_imports[hs_8digit %in% ieepa_statute_codes, `:=`(
  ieepa_rate = 0,
  emergency_rate = 0,              # Also exempt from emergency orders (China, Canada, Mexico)
  rr_exception = 1,                # Also mark as rr_exception for IEEPA reciprocal tariff
  ieepa_statute_exception = 1      # Separate marker for emergency order exclusions
)]

statute_flows <- sum(us_imports[hs_8digit %in% ieepa_statute_codes, .N])
statute_value <- sum(us_imports[hs_8digit %in% ieepa_statute_codes]$us_imports_bn, na.rm = TRUE)

cat(sprintf("    Applied statutory exceptions to %s flows ($%.1f billion)\n",
            format(statute_flows, big.mark = ","), statute_value))
cat("    Note: These products also exempt from emergency orders (China, Canada, Mexico)\n")

cat("\n  Section 4 complete: IEEPA rates populated\n")
cat("\n")

# =============================================================================
# SECTION 5: EMERGENCY RATES (Populate emergency_rate column)
# =============================================================================
# Apply country-specific security actions (China/Hong Kong, Canada)
# =============================================================================

cat("SECTION 5: Emergency Rates (Country-Specific Security)\n")
cat("--------------------------------------------------------\n")

# -----------------------------------------------------------------------------
# 5.1: China & Hong Kong - Synthetic Opioid Tariff (UPDATED POLICY)
# -----------------------------------------------------------------------------

cat("  5.1 China & Hong Kong - Synthetic Opioid Tariff...\n")

# PRODUCT SCOPE EXCLUSIONS:
# Only statutory IEEPA exceptions (ieepa_statute_exception == 1) are excluded from emergency order
# Annex 2 reciprocal exceptions (rr_exception == 1) remain subject to this tariff

# TIME-VARYING POLICY: Rate loaded from rates.csv based on policy_date
# Effective dates in rates.csv:
# - 2025-02-04: 10% (Feb 4 - Mar 3, 2025)
# - 2025-03-04: 20% (Mar 4 - Nov 9, 2025)
# - 2025-11-10: 10% (Nov 10, 2025 onwards)

# Get China emergency rate from database (date-aware lookup)
china_emergency_rate <- get_rate("china_opioid_rate", policy_date)

# Apply rate to China/Hong Kong products (except statutory IEEPA exceptions)
# NOTE: UK and EU are explicitly exempt from this tariff (not China/HK)
if (china_emergency_rate > 0) {
  us_imports[un_code %in% china_hkg_codes & ieepa_statute_exception == 0, `:=`(
    emergency_rate = china_emergency_rate,
    chn_opioid = 1
  )]
}

china_hkg_affected <- sum(us_imports[chn_opioid == 1]$us_imports_bn)
cat(sprintf("    Applied synthetic opioid tariff (+%d%%) to $%.1f billion\n",
            china_emergency_rate, china_hkg_affected))
cat(sprintf("    Countries: China (%d), Hong Kong (%d)\n",
            china_un_code, hong_kong_un_code))
cat(sprintf("    Policy date: %s\n", format(policy_date, "%Y-%m-%d")))
cat(sprintf("    Scope: All products EXCEPT statutory IEEPA exceptions\n"))
cat(sprintf("    Note: UK and EU exempt from this China-specific tariff\n"))

# -----------------------------------------------------------------------------
# 5.2: Canada - Northern Border Security (3-Category Logic)
# -----------------------------------------------------------------------------

cat("\n  5.2 Canada - Northern Border Security (3 categories)...\n")

# Canada Emergency Rate Parameters (loaded from rates.csv)
can_emergency_energy_rate <- get_rate("can_emergency_energy_rate")   # Canadian energy products
can_emergency_general_rate <- get_rate("can_emergency_general_rate") # Canadian non-energy products

# Load Canadian northern border exception HS codes (energy products from exceptions.csv)
can_northern_border_codes <- get_exceptions("can_emergency_energy_rate", "energy", policy_date)
cat(sprintf("    Loaded %d HS8 codes for Canadian northern border (energy)\n",
            length(can_northern_border_codes)))

# PRODUCT SCOPE EXCLUSIONS:
# Only statutory IEEPA exceptions (ieepa_statute_exception == 1) are excluded from emergency order
# Annex 2 reciprocal exceptions (rr_exception == 1) remain subject to this tariff
# NOTE: Tariff stacking rules in Section 8 will provide relief for transport S232 (auto, MHDV)

# Category 1: Statutory IEEPA Exception Goods - NO emergency rate
# (Already have ieepa_statute_exception = 1 from Section 4.5, emergency_rate already 0)
cat1_value <- sum(us_imports[un_code == canada_un_code & ieepa_statute_exception == 1]$us_imports_bn)
cat(sprintf("    Category 1 (Statutory IEEPA goods): $%.1f billion at 0%% emergency\n", cat1_value))

# Category 2: Energy Products (Northern Border List) - Base rate from parameter
# USMCA compliance weighting (0%) applied in Section 8
us_imports[un_code == canada_un_code &
           hs_8digit %in% can_northern_border_codes &
           ieepa_statute_exception == 0, `:=`(
  emergency_rate = can_emergency_energy_rate,  # Base rate; 0% USMCA compliance → full rate effective (in Sec 8)
  can_northern_border = 1,
  can_northern_border_energy = 1
)]

cat2_value <- sum(us_imports[can_northern_border_energy == 1]$us_imports_bn)
cat(sprintf("    Category 2 (Energy): $%.1f billion at %.0f%% base (0%% USMCA)\n", cat2_value, can_emergency_energy_rate))

# Category 3: Other Products (Non-statutory IEEPA, Non-Energy) - Base rate from parameter
# USMCA compliance weighting (100%) applied in Section 8 → zeroes out to 0%
us_imports[un_code == canada_un_code &
           ieepa_statute_exception == 0 &
           !hs_8digit %in% can_northern_border_codes, `:=`(
  emergency_rate = can_emergency_general_rate,  # Base rate; 100% USMCA compliance → 0% effective (in Sec 8)
  can_northern_border = 1
)]

cat3_value <- sum(us_imports[un_code == canada_un_code &
                             ieepa_statute_exception == 0 &
                             !hs_8digit %in% can_northern_border_codes]$us_imports_bn)
cat(sprintf("    Category 3 (Other): $%.1f billion at %.0f%% base (100%% USMCA → 0%% effective)\n", cat3_value, can_emergency_general_rate))

cat(sprintf("    Note: USMCA weighting applied in Section 8 final calculation\n"))
cat(sprintf("    Note: Tariff stacking rules in Section 8 provide relief for transport S232 products\n"))

# -----------------------------------------------------------------------------
# 5.3: Mexico - Emergency Tariff (2-Category Logic)
# -----------------------------------------------------------------------------

cat("\n  5.3 Mexico - Emergency Tariff (2 categories)...\n")

# Mexico Emergency Rate Parameter (loaded from rates.csv)
mex_emergency_rate <- get_rate("mex_emergency_rate")  # Mexican products emergency rate

# PRODUCT SCOPE EXCLUSIONS:
# Only statutory IEEPA exceptions (ieepa_statute_exception == 1) are excluded from emergency order
# Annex 2 reciprocal exceptions (rr_exception == 1) remain subject to this tariff
# NOTE: Tariff stacking rules in Section 8 will provide relief for transport S232 (auto, MHDV)

# Category 1: Statutory IEEPA Exception Goods - NO emergency rate
# (Already have ieepa_statute_exception = 1 from Section 4.5, emergency_rate already 0)
mex_cat1_value <- sum(us_imports[un_code == mexico_un_code & ieepa_statute_exception == 1]$us_imports_bn)
cat(sprintf("    Category 1 (Statutory IEEPA goods): $%.1f billion at 0%% emergency\n", mex_cat1_value))

# Category 2: Other Products (Non-statutory IEEPA) - Base rate from parameter
# USMCA compliance weighting (100%) applied in Section 8 → zeroes out to 0%
us_imports[un_code == mexico_un_code &
           ieepa_statute_exception == 0, `:=`(
  emergency_rate = mex_emergency_rate,  # Base rate; 100% USMCA compliance → 0% effective (in Sec 8)
  mex_emergency = 1
)]

mex_cat2_value <- sum(us_imports[mex_emergency == 1]$us_imports_bn)
cat(sprintf("    Category 2 (Other): $%.1f billion at %.0f%% base (100%% USMCA → 0%% effective)\n", mex_cat2_value, mex_emergency_rate))

cat(sprintf("    Note: USMCA weighting applied in Section 8 final calculation\n"))
cat(sprintf("    Note: Tariff stacking rules in Section 8 provide relief for transport S232 products\n"))

cat("\n  Section 5 complete: Emergency rates populated\n")
cat("\n")

# =============================================================================
# SECTION 6: SECTION 301 RATES (Populate s301_rate column)
# =============================================================================
# Apply Section 301 tariffs (China only)
# =============================================================================

cat("SECTION 6: Section 301 Tariffs (China)\n")
cat("---------------------------------------\n")

if (EXCLUDE_S301_TARIFFS) {
  cat("  [OPTION] Section 301 tariffs EXCLUDED (observational data already includes these)\n")
  cat("  s301_rate remains 0 for all products\n")
} else {
  # Load Section 301 data
  sec301 <- fread("data/sec301.csv", sep = ";")
  sec301[, hs_8digit := sprintf("%08s", hs_8digit)]
  cat(sprintf("  Loaded %d HS8 codes for Section 301\n", nrow(sec301)))

  # Apply Section 301 tariffs to China only
  for(i in seq_len(nrow(sec301))) {
    hs_code <- sec301$hs_8digit[i]
    add_rate <- as.numeric(sec301$rate[i])

    if(!is.na(add_rate)) {
      us_imports[un_code == china_un_code & hs_8digit == hs_code,
                 s301_rate := add_rate]
    }
  }

  # Mark products with Section 301
  us_imports[s301_rate > 0, sec301_tariff := 1]

  china_sec301_value <- sum(us_imports[sec301_tariff == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("  Applied Section 301 to $%.1f billion of Chinese imports\n",
              china_sec301_value))
}

cat("\n  Section 6 complete: S301 rates populated\n")
cat("\n")

# =============================================================================
# SECTION 7: COUNTRY-SPECIFIC EXCEPTIONS
# =============================================================================
# Apply special deals that modify rate columns (s232_rate and/or ieepa_rate)
# These override the standard framework established in Sections 2-6
# =============================================================================

cat("SECTION 7: Country-Specific Exceptions\n")
cat("---------------------------------------\n")

# Load unified civil aircraft list (used by multiple countries) - from exceptions.csv
aircraft_main <- get_exceptions("ieepa_baseline_rate", "aircraft", policy_date)

cat(sprintf("  Loaded unified civil aircraft list: %d aircraft\n",
            length(aircraft_main)))

# Load unified pharma exceptions list (used by EU and Switzerland) - from exceptions.csv
pharma_hs_codes <- get_exceptions("ieepa_baseline_rate", "pharma", policy_date)

cat(sprintf("  Loaded unified pharma exceptions list: %d products\n",
            length(pharma_hs_codes)))

# -----------------------------------------------------------------------------
# 7.0: S232 STEEL COUNTRY EXEMPTIONS (Pre-March 12, 2025)
# -----------------------------------------------------------------------------
# Multiple countries had S232 steel exemptions that lapsed on March 12, 2025
# Countries: Brazil, Australia, Canada, Mexico, Ukraine, Argentina, 
#            Republic of Korea, Japan, UK, EU member states
# Period: January 1, 2025 to March 11, 2025 (0% rate on S232 steel)
# -----------------------------------------------------------------------------

cat("\n  7.0 S232 Steel Country Exemptions (Pre-March 12)...\n")

# Define countries with S232 steel exemptions
s232_steel_exempt_countries <- c(
  brazil_un_code,
  australia_un_code,
  canada_un_code,
  mexico_un_code,
  ukraine_un_code,
  argentina_un_code,
  korea_un_code,
  japan_un_code,
  uk_un_code,
  eu_members  # This is already a vector of UN codes
)

# Apply exemption only if policy_date is before March 12, 2025
s232_steel_exemption_end <- as.Date("2025-03-12")

if (policy_date < s232_steel_exemption_end) {
  # Set s232_rate = 0 for steel products from exempt countries
  us_imports[un_code %in% s232_steel_exempt_countries & 
             (s232_steel == 1 | s232_steel_derivative == 1), `:=`(
    s232_rate = 0,
    s232_steel_exempt = 1
  )]
  
  exempt_value <- sum(us_imports[s232_steel_exempt == 1]$us_imports_bn, na.rm = TRUE)
  exempt_count <- sum(us_imports$s232_steel_exempt, na.rm = TRUE)
  cat(sprintf("    Applied 0%% S232 steel rate to %d products ($%.1f billion)\n",
              exempt_count, exempt_value))
  cat(sprintf("    Exemption period: Jan 1, 2025 - Mar 11, 2025 (policy_date: %s)\n",
              format(policy_date, "%Y-%m-%d")))
} else {
  cat(sprintf("    Exemption lapsed (policy_date %s >= March 12, 2025)\n",
              format(policy_date, "%Y-%m-%d")))
  # Initialize column to 0 for consistency
  us_imports[, s232_steel_exempt := 0]
}

# -----------------------------------------------------------------------------
# 7.1: INDIA (Type A - Scope-Specific Rate Change)
# -----------------------------------------------------------------------------

cat("\n  7.1 India\n")

# India/Russia Additional Top-Up Parameter (loaded from rates.csv)
india_russia_topup <- get_rate("india_russia_topup")  # Additional top-up for India and Russia

# India surcharge: add top-up to existing IEEPA rate
# ONLY applies to IEEPA-eligible products (NOT Annex 2 exceptions or statutory IEEPA exceptions)
# Skip if zero_all_country_topups is enabled (e.g., Section 122 scenario)
if (exists("SCENARIO_OVERRIDES") && !is.null(SCENARIO_OVERRIDES$zero_all_country_topups) &&
    SCENARIO_OVERRIDES$zero_all_country_topups == TRUE) {
  cat("    [SCENARIO] India top-up skipped (zero_all_country_topups = TRUE)\n")
  india_russia_topup <- 0
} else {
  us_imports[un_code == india_un_code &
             ieepa_statute_exception == 0 &
             rr_exception == 0,
             ieepa_rate := ieepa_rate + india_russia_topup]
}

india_ieepa_eligible <- sum(us_imports[un_code == india_un_code & ieepa_statute_exception == 0 & rr_exception == 0]$us_imports_bn)
india_annex2 <- sum(us_imports[un_code == india_un_code & rr_exception == 1]$us_imports_bn)
india_total <- india_ieepa_eligible + india_annex2

cat(sprintf("    IEEPA-eligible products: $%.1f billion at %.0f%% rate\n", india_ieepa_eligible, ieepa_baseline_rate + india_russia_topup))
cat(sprintf("    Annex 2 exceptions: $%.1f billion at 0%% IEEPA rate\n", india_annex2))
cat(sprintf("    Total India imports: $%.1f billion\n", india_total))
cat(sprintf("    Formula: ieepa_rate = existing + %.0f = %.0f (only for IEEPA-eligible)\n", india_russia_topup, ieepa_baseline_rate + india_russia_topup))

# -----------------------------------------------------------------------------
# 7.2: BRAZIL (Type B - Scope-Specific) - CORRECTED
# -----------------------------------------------------------------------------

cat("\n  7.2 Brazil (Type B - Scope-specific)\n")

# Brazil Additional Top-Up Parameter (loaded from rates.csv)
bra_topup <- get_rate("bra_topup")  # Brazil-specific additional top-up

# CORRECTED: Brazil rate adds top-up to existing IEEPA rate (not flat 50%)
# Exception applies only to statutory IEEPA exceptions, bra_exceptions, and aircraft
# Skip if zero_all_country_topups is enabled (e.g., Section 122 scenario)
if (exists("SCENARIO_OVERRIDES") && !is.null(SCENARIO_OVERRIDES$zero_all_country_topups) &&
    SCENARIO_OVERRIDES$zero_all_country_topups == TRUE) {
  cat("    [SCENARIO] Brazil top-up skipped (zero_all_country_topups = TRUE)\n")
  bra_topup <- 0
} else {
  us_imports[un_code == brazil_un_code &
             ieepa_statute_exception == 0 &
             bra_exception == 0,
             ieepa_rate := ieepa_rate + bra_topup]
}

# Load Brazil exception codes (date-aware via get_exceptions)
# Version selection is automatic: pre-Nov13 vs post-Nov13 handled by effective_date
bra_exception_codes <- get_exceptions("bra_topup", "country_exception", policy_date, "BRA")

if (policy_date >= as.Date("2025-11-13")) {
  cat(sprintf("    Using November 13, 2025 Brazil exceptions (%d codes)\n",
              length(bra_exception_codes)))
} else {
  cat(sprintf("    Using original Brazil exceptions (%d codes)\n",
              length(bra_exception_codes)))
}

# UNCONDITIONAL EXCEPTIONS: Always revert to MFN only
us_imports[un_code == brazil_un_code &
           hs_8digit %in% bra_exception_codes, `:=`(
  ieepa_rate = 0,
  bra_exception = 1
)]

# CONDITIONAL - CIVIL AIRCRAFT
# Mark aircraft products; rate weighting applied in Section 8 using bra_civil_aircraft_share
us_imports[un_code == brazil_un_code & hs_8digit %in% aircraft_main, `:=`(
  bra_aircraft = 1,
  wto_aircraft = 1
)]

# NOTE: Aircraft share weighting is applied in Section 8 rate calculation
# Formula: (1 - bra_civil_aircraft_share) * [materials_s232 + ieepa + emergency]

brazil_total <- sum(us_imports[un_code == brazil_un_code]$us_imports_bn)
brazil_excepted <- sum(us_imports[un_code == brazil_un_code &
                                  (bra_exception == 1 | bra_aircraft == 1 | bra_vehicle == 1)]$us_imports_bn)

cat(sprintf("    Brazil total: $%.1f billion, Excepted: $%.1f billion\n",
            brazil_total, brazil_excepted))
cat(sprintf("    CORRECTED: Rate is existing IEEPA + %.0f = %.0f total (not flat 50%%)\n", bra_topup, ieepa_baseline_rate + bra_topup))

# -----------------------------------------------------------------------------
# 7.3: UK (Type B - Economic Prosperity Deal)
# -----------------------------------------------------------------------------

cat("\n  7.3 UK (Type B - Economic Prosperity Deal)...\n")

# UK Economic Prosperity Deal effective date: June 30, 2025
uk_deal_effective_date <- as.Date("2025-06-30")

# UK S232 Rate Parameters (with scenario override support)
# UK Exception Rate Parameters (loaded from rates.csv)
uk_ceiling_rate <- get_rate("uk_ceiling_rate")              # UK ceiling for auto and PV parts
uk_steel_alu_cap <- get_rate("uk_steel_alu_cap")            # UK steel/aluminum cap (within quota)
uk_lumber_derivative_rate <- get_rate("uk_lumber_derivative_rate")  # UK kitchen cabinets/furniture
jpn_lumber_derivative_rate <- get_rate("jpn_lumber_derivative_rate") # Japan kitchen cabinets/furniture
eu_lumber_derivative_rate <- get_rate("eu_lumber_derivative_rate")   # EU kitchen cabinets/furniture

# Apply UK deal terms only if policy_date >= June 30, 2025
if (policy_date >= uk_deal_effective_date) {
  
  # CIVIL AIRCRAFT
  # Mark aircraft products; rate weighting applied in Section 8 using uk_civil_aircraft_share
  us_imports[un_code == uk_un_code & hs_8digit %in% aircraft_main, `:=`(
    uk_aircraft = 1,
    wto_aircraft = 1
  )]
  # NOTE: Aircraft share weighting is applied in Section 8 rate calculation
  # Formula: (1 - uk_civil_aircraft_share) * [materials_s232 + ieepa + emergency]
  # Transport S232 (auto/MHDV) is NOT affected by aircraft share
  
  # AUTO S232 - Special formula: s232_rate = ceiling - max(ceiling, hts_rate)
  # This ensures hts_rate + ieepa_rate (baseline) + s232_rate = exactly ceiling percent
  # Applies to ALL products with s232_auto = 1
  # Apply formula: ceiling - max(ceiling, hts_rate)
  # If hts_rate >= ceiling%, s232_rate = 0 or negative (floor at 0)
  # If hts_rate < ceiling%, s232_rate makes up the difference to reach ceiling% total
  us_imports[un_code == uk_un_code & s232_auto == 1, `:=`(
    s232_rate = pmax(0, uk_ceiling_rate - hts_rate),
    uk_auto = 1
  )]
  
  # S232 RATE CAPS WITH QUOTA LOGIC
  # Steel/Alu: Cap at specified rate for products within quantity quota
  # Share parameter: uk_steel_alu_quota_share (set to 1.0 in Section 0.5)
  # Within quota (share): Cap applies
  # Outside quota (1-share): Standard S232 rate applies
  us_imports[un_code == uk_un_code & (s232_steel == 1 | s232_steel_derivative == 1 |
                                      s232_alu == 1 | s232_alu_derivative == 1), `:=`(
    s232_rate = uk_steel_alu_quota_share * uk_steel_alu_cap +
                (1 - uk_steel_alu_quota_share) * s232_rate,
    uk_steel_alu = 1
  )]
  
  # Lumber derivatives - kitchen cabinets and upholstered furniture
  us_imports[un_code == uk_un_code &
             (s232_lumber_derivative == 1 | s232_upholstered_furniture == 1),
             s232_rate := uk_lumber_derivative_rate]
  
  # Passenger vehicle parts: Special share-based treatment
  # For UK PV parts used in UK-built cars: s232_rate = ceiling - max(ceiling, hts_rate)
  # For UK PV parts used in other cars: standard s232_rate applies
  # Share parameter: uk_car_parts_uk_built_share (set to 1.0 in Section 0.5)
  us_imports[un_code == uk_un_code & s232_pv_parts == 1, `:=`(
    s232_rate = uk_car_parts_uk_built_share * pmax(0, uk_ceiling_rate - hts_rate) +
                (1 - uk_car_parts_uk_built_share) * s232_rate,
    uk_pv_parts = 1
  )]
  
  uk_value <- sum(us_imports[un_code == uk_un_code]$us_imports_bn)
  cat(sprintf("    UK Economic Prosperity Deal applied to $%.1f billion\n", uk_value))
  
} else {
  cat(sprintf("    UK Economic Prosperity Deal NOT active (effective %s, policy_date %s)\n",
              uk_deal_effective_date, policy_date))
}

# -----------------------------------------------------------------------------
# 7.4: JAPAN (Type B - Tariff Floors) - CORRECTED
# -----------------------------------------------------------------------------

cat("\n  7.4 Japan (Type B - Tariff floors)\n")

# Japan deal effective date: September 16, 2025
jpn_deal_effective_date <- as.Date("2025-09-16")

# Japan Floor Rate Parameters (loaded from rates.csv) - DECOUPLED
jpn_ieepa_floor_rate <- get_rate("jpn_ieepa_floor_rate")  # Japan IEEPA floor rate (reciprocal tariffs)
jpn_s232_floor_rate <- get_rate("jpn_s232_floor_rate")    # Japan S232 auto/MHDV floor rate

# Apply Japan deal terms only if policy_date >= September 16, 2025
if (policy_date >= jpn_deal_effective_date) {
  
  # Load tariff floor rates for Japan
  floor_jpn <- fread("data/tariff_floor_rates.csv", sep = ";")
  floor_jpn[, hts8 := sprintf("%08s", hts8)]
  
  # Determine floor rate column
  floor_rate_col_jpn <- if("floor_rate" %in% names(floor_jpn)) {
    "floor_rate"
  } else if("new_eu_rate" %in% names(floor_jpn)) {
    "new_eu_rate"
  } else {
    NA_character_
  }
  
  # Apply tariff floors (product-by-product) for Japan
  # CORRECTED FORMULA: max(floor, mfn) instead of just floor
  # SCENARIO-AWARE: If jpn_ieepa_floor_rate is 0, skip floor application (set ieepa_rate = 0)
  unique_hs_jpn <- unique(us_imports[un_code == japan_un_code]$hs_8digit)
  
  # Check if IEEPA floor is disabled by scenario (rate = 0)
  if (jpn_ieepa_floor_rate == 0) {
    # IEEPA floor disabled - set ieepa_rate to 0 for all Japan products in IEEPA scope
    us_imports[un_code == japan_un_code & rr_exception == 0 & ieepa_statute_exception == 0,
               ieepa_rate := 0]
    cat("    [SCENARIO] Japan IEEPA floor disabled (rate = 0)\n")
  } else {
    # Apply product-specific floors from CSV, falling back to scenario parameter
    for(hs_code in unique_hs_jpn) {
      floor_row <- floor_jpn[hts8 == hs_code]
    
      if(nrow(floor_row) > 0 && !is.na(floor_rate_col_jpn)) {
        floor_rate_val <- suppressWarnings(as.numeric(floor_row[[floor_rate_col_jpn]][1]))
        if(!is.na(floor_rate_val)) {
          # CORRECTED: Apply max(floor, MFN), store as addition to MFN
          # Only apply to products not exempt from reciprocal rates
          us_imports[un_code == japan_un_code & hs_8digit == hs_code &
                     rr_exception == 0 & ieepa_statute_exception == 0,
                     ieepa_rate := pmax(floor_rate_val, hts_rate) - hts_rate]
        } else {
          # Default floor from parameter, CORRECTED: max(floor, mfn)
          # Only apply to products not exempt from reciprocal rates
          us_imports[un_code == japan_un_code & hs_8digit == hs_code &
                     rr_exception == 0 & ieepa_statute_exception == 0,
                     ieepa_rate := pmax(jpn_ieepa_floor_rate, hts_rate) - hts_rate]
        }
      } else {
        # No floor data: use default from parameter, CORRECTED: max(floor, mfn)
        # Only apply to products not exempt from reciprocal rates
        us_imports[un_code == japan_un_code & hs_8digit == hs_code &
                   rr_exception == 0 & ieepa_statute_exception == 0,
                   ieepa_rate := pmax(jpn_ieepa_floor_rate, hts_rate) - hts_rate]
      }
    }
  }
  
  # CIVIL AIRCRAFT CONDITIONAL EXCEPTIONS - Japan
  # Mark aircraft products; rate weighting applied in Section 8 using jpn_civil_aircraft_share
  us_imports[un_code == japan_un_code & hs_8digit %in% aircraft_main, wto_aircraft := 1]
  # NOTE: Aircraft share weighting is applied in Section 8 rate calculation
  # Formula: (1 - jpn_civil_aircraft_share) * [materials_s232 + ieepa + emergency]
  
  # S232 RATE CAPS FOR JAPAN (floor treatment) - uses separate S232 floor rate
  # Only apply to products not exempt from reciprocal rates
  us_imports[un_code == japan_un_code &
             (s232_auto == 1 | s232_mhdv == 1) &
             rr_exception == 0 & ieepa_statute_exception == 0,
             s232_rate := pmax(jpn_s232_floor_rate, hts_rate) - hts_rate]
  
  # Lumber derivatives - kitchen cabinets and upholstered furniture
  us_imports[un_code == japan_un_code &
             (s232_lumber_derivative == 1 | s232_upholstered_furniture == 1), `:=`(
    s232_rate = jpn_lumber_derivative_rate,
    jpn_lumber_derivative = 1
  )]
  
  japan_value <- sum(us_imports[un_code == japan_un_code]$us_imports_bn)
  cat(sprintf("    Japan tariff floor applied to $%.1f billion\n", japan_value))
  
} else {
  cat(sprintf("    Japan deal NOT active (effective %s, policy_date %s)\n",
              jpn_deal_effective_date, policy_date))
}

# -----------------------------------------------------------------------------
# 7.5: EU (Type B - Tariff Floors) - CORRECTED
# -----------------------------------------------------------------------------

cat("\n  7.5 EU (Type B - Tariff floors)\n")

# EU deal effective date: September 1, 2025
eu_deal_effective_date <- as.Date("2025-09-01")

# EU Floor Rate Parameters (loaded from rates.csv) - DECOUPLED
eu_ieepa_floor_rate <- get_rate("eu_ieepa_floor_rate")  # EU IEEPA floor rate (reciprocal tariffs)
eu_s232_floor_rate <- get_rate("eu_s232_floor_rate")    # EU S232 auto/MHDV floor rate

# Apply EU deal terms only if policy_date >= September 1, 2025
if (policy_date >= eu_deal_effective_date) {
  
  # Load tariff floor rates for EU (reuse same file, create separate object)
  floor_eu <- fread("data/tariff_floor_rates.csv", sep = ";")
  floor_eu[, hts8 := sprintf("%08s", hts8)]
  
  # Determine floor rate column
  floor_rate_col_eu <- if("floor_rate" %in% names(floor_eu)) {
    "floor_rate"
  } else if("new_eu_rate" %in% names(floor_eu)) {
    "new_eu_rate"
  } else {
    NA_character_
  }
  
  # Apply tariff floors (product-by-product) for EU
  # CORRECTED FORMULA: max(floor, mfn) instead of just floor
  # SCENARIO-AWARE: If eu_ieepa_floor_rate is 0, skip floor application (set ieepa_rate = 0)
  # COUNTRY-SPECIFIC: Check for ieepa_floor_override_XXX for individual EU member overrides
  unique_hs_eu <- unique(us_imports[un_code %in% eu_members]$hs_8digit)

  # Check if IEEPA floor is disabled by scenario (rate = 0)
  if (eu_ieepa_floor_rate == 0) {
    # IEEPA floor disabled - set ieepa_rate to 0 for all EU products in IEEPA scope
    us_imports[un_code %in% eu_members & rr_exception == 0 & ieepa_statute_exception == 0,
               ieepa_rate := 0]
    cat("    [SCENARIO] EU IEEPA floor disabled (rate = 0)\n")
  } else {
    # Build country-specific IEEPA floor override lookup
    eu_ieepa_floor_overrides <- list()
    override_countries <- c()
    for (eu_un in eu_members) {
      override_val <- get_country_ieepa_floor_override(eu_un, policy_date)
      if (!is.na(override_val)) {
        eu_ieepa_floor_overrides[[as.character(eu_un)]] <- override_val
        override_countries <- c(override_countries, eu_un)
      }
    }
    if (length(override_countries) > 0) {
      cat(sprintf("    [SCENARIO] IEEPA floor overrides for %d EU members: %s\n",
                  length(override_countries), paste(override_countries, collapse = ", ")))
    }

    # Apply product-specific floors from CSV, falling back to scenario parameter
    for(hs_code in unique_hs_eu) {
      floor_row <- floor_eu[hts8 == hs_code]

      if(nrow(floor_row) > 0 && !is.na(floor_rate_col_eu)) {
        floor_rate_val <- suppressWarnings(as.numeric(floor_row[[floor_rate_col_eu]][1]))
        if(!is.na(floor_rate_val)) {
          # CORRECTED: Apply max(floor, MFN), store as addition to MFN
          # Only apply to products not exempt from reciprocal rates
          us_imports[un_code %in% eu_members & hs_8digit == hs_code &
                     rr_exception == 0 & ieepa_statute_exception == 0,
                     ieepa_rate := pmax(floor_rate_val, hts_rate) - hts_rate]
        } else {
          # Default floor from parameter, CORRECTED: max(floor, mfn)
          # Only apply to products not exempt from reciprocal rates
          us_imports[un_code %in% eu_members & hs_8digit == hs_code &
                     rr_exception == 0 & ieepa_statute_exception == 0,
                     ieepa_rate := pmax(eu_ieepa_floor_rate, hts_rate) - hts_rate]
        }
      } else {
        # No floor data: use default from parameter, CORRECTED: max(floor, mfn)
        # Only apply to products not exempt from reciprocal rates
        us_imports[un_code %in% eu_members & hs_8digit == hs_code &
                   rr_exception == 0 & ieepa_statute_exception == 0,
                   ieepa_rate := pmax(eu_ieepa_floor_rate, hts_rate) - hts_rate]
      }
    }

    # Apply country-specific IEEPA floor overrides (override the default EU floor)
    for (eu_un_str in names(eu_ieepa_floor_overrides)) {
      eu_un <- as.integer(eu_un_str)
      override_floor <- eu_ieepa_floor_overrides[[eu_un_str]]
      us_imports[un_code == eu_un & rr_exception == 0 & ieepa_statute_exception == 0,
                 ieepa_rate := pmax(override_floor, hts_rate) - hts_rate]
    }
  }
  
  # EU UNCONDITIONAL EXCEPTIONS (from exceptions.csv)
  eu_exception_codes <- get_exceptions("eu_ieepa_floor_rate", "country_exception", policy_date)
  
  us_imports[un_code %in% eu_members &
             hs_8digit %in% eu_exception_codes, `:=`(
    ieepa_rate = 0,
    eu_exception = 1
  )]
  
  # NON-PATENTED PHARMACEUTICALS EXCEPTION - EU
  # Share of pharma imports not patented in US exempt from reciprocal rates
  # Uses unified pharma_hs_codes loaded at start of Section 7
  us_imports[un_code %in% eu_members &
             hs_8digit %in% pharma_hs_codes, `:=`(
    ieepa_rate = (1 - eu_nonpatented_pharma_share) * ieepa_rate,
    eu_pharma = 1
  )]
  
  eu_pharma_value <- sum(us_imports[un_code %in% eu_members & eu_pharma == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    EU non-patented pharma (%.0f%% exempt): $%.1f billion\n",
              eu_nonpatented_pharma_share * 100, eu_pharma_value))
  
  # CIVIL AIRCRAFT CONDITIONAL EXCEPTIONS - EU
  # Mark aircraft products; rate weighting applied in Section 8 using eu_civil_aircraft_share
  us_imports[un_code %in% eu_members & hs_8digit %in% aircraft_main, wto_aircraft := 1]
  # NOTE: Aircraft share weighting is applied in Section 8 rate calculation
  # Formula: (1 - eu_civil_aircraft_share) * [materials_s232 + ieepa + emergency]
  
  # S232 RATE CAPS FOR EU (floor treatment) - uses separate S232 floor rate
  # COUNTRY-SPECIFIC: Check for s232_floor_override_XXX for individual EU member overrides
  # Only apply to products not exempt from reciprocal rates

  # Build country-specific S232 floor override lookup
  eu_s232_floor_overrides <- list()
  s232_override_countries <- c()
  for (eu_un in eu_members) {
    override_val <- get_country_s232_floor_override(eu_un, policy_date)
    if (!is.na(override_val)) {
      eu_s232_floor_overrides[[as.character(eu_un)]] <- override_val
      s232_override_countries <- c(s232_override_countries, eu_un)
    }
  }
  if (length(s232_override_countries) > 0) {
    cat(sprintf("    [SCENARIO] S232 floor overrides for %d EU members: %s\n",
                length(s232_override_countries), paste(s232_override_countries, collapse = ", ")))
  }

  # Apply default EU S232 floor
  us_imports[un_code %in% eu_members &
             (s232_auto == 1 | s232_mhdv == 1) &
             rr_exception == 0 & ieepa_statute_exception == 0,
             s232_rate := pmax(eu_s232_floor_rate, hts_rate) - hts_rate]

  # Apply country-specific S232 floor overrides
  for (eu_un_str in names(eu_s232_floor_overrides)) {
    eu_un <- as.integer(eu_un_str)
    override_floor <- eu_s232_floor_overrides[[eu_un_str]]
    us_imports[un_code == eu_un &
               (s232_auto == 1 | s232_mhdv == 1) &
               rr_exception == 0 & ieepa_statute_exception == 0,
               s232_rate := pmax(override_floor, hts_rate) - hts_rate]
  }

  # Lumber derivatives - kitchen cabinets and upholstered furniture
  us_imports[un_code %in% eu_members &
             (s232_lumber_derivative == 1 | s232_upholstered_furniture == 1), `:=`(
    s232_rate = eu_lumber_derivative_rate,
    eu_lumber_derivative = 1
  )]
  
  eu_value <- sum(us_imports[un_code %in% eu_members]$us_imports_bn)
  cat(sprintf("    EU tariff floor applied to $%.1f billion\n", eu_value))
  
} else {
  cat(sprintf("    EU deal NOT active (effective %s, policy_date %s)\n",
              eu_deal_effective_date, policy_date))
}

# -----------------------------------------------------------------------------
# 7.6: Korea (Type B - Multiple Exception Types)
# -----------------------------------------------------------------------------

cat("\n  7.6 Korea (Multiple Exception Types)...\n")

# EXCEPTION 4: AUTO TARIFF FLOOR VIA S232 (NOT YET IMPLEMENTED)
# Same mechanism as Japan/EU auto/MHDV S232 floor, but Korea excludes MHDV
# Formula: s232_rate = pmax(floor, hts_rate) - hts_rate ensures total >= 15%
# NOTE: Korea deal not yet finalized - set to impossible future date
if(policy_date >= as.Date("2025-11-14")) {
  us_imports[un_code == korea_un_code & s232_auto == 1, `:=`(
    s232_rate = pmax(kor_s232_floor_rate, hts_rate) - hts_rate,
    kor_auto = 1
  )]

  auto_value <- sum(us_imports[un_code == korea_un_code & kor_auto == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Auto S232 floor (%.0f%%, Nov 1+): $%.1f billion\n",
              kor_s232_floor_rate, auto_value))
}

# EXCEPTIONS 1-3: NOT YET IMPLEMENTED
# NOTE: Korea deal not yet finalized - set to impossible future date
if(policy_date >= as.Date("2025-11-14")) {

  # EXCEPTION 2: LUMBER S232 RATE (Cabinets & Upholstered Furniture)
  # S232 rate set to 15% for specific lumber derivatives
  us_imports[un_code == korea_un_code &
             (s232_lumber_derivative == 1 | s232_upholstered_furniture == 1), `:=`(
    s232_rate = kor_lumber_derivative_rate,
    kor_lumber_derivative = 1
  )]

  lumber_value <- sum(us_imports[un_code == korea_un_code & kor_lumber_derivative == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Lumber S232 rate (%.0f%%, Nov 14+): $%.1f billion\n",
              kor_lumber_derivative_rate, lumber_value))

  # EXCEPTION 3: CIVIL AIRCRAFT
  # Mark aircraft products; rate weighting applied in Section 8 using kor_civil_aircraft_share
  us_imports[un_code == korea_un_code & hs_8digit %in% aircraft_main, `:=`(
    kor_aircraft = 1,
    wto_aircraft = 1
  )]

  aircraft_value <- sum(us_imports[un_code == korea_un_code & kor_aircraft == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Civil aircraft (%.0f%% share, Nov 14+): $%.1f billion\n",
              kor_civil_aircraft_share * 100, aircraft_value))

  # EXCEPTION 1: RECIPROCAL RATE FLOOR (Applied via IEEPA component)
  # Formula: ieepa_rate = pmax(floor, hts_rate) - hts_rate ensures total >= 15%
  # Applies ONLY to products in reciprocal rate scope AFTER general exceptions
  # Excludes: autos (kor_auto==1), Annex 2 (rr_exception==1), statutory IEEPA (ieepa_statute_exception==1)
  us_imports[un_code == korea_un_code &
             kor_auto == 0 &
             rr_exception == 0 &
             ieepa_statute_exception == 0, `:=`(
    ieepa_rate = pmax(kor_ieepa_floor_rate, hts_rate) - hts_rate,
    kor_floor = 1
  )]

  floor_value <- sum(us_imports[un_code == korea_un_code & kor_floor == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    IEEPA floor (%.0f%%, Nov 14+): $%.1f billion\n",
              kor_ieepa_floor_rate, floor_value))
}

# Summary statistics
korea_total <- sum(us_imports[un_code == korea_un_code]$us_imports_bn, na.rm = TRUE)
korea_excepted <- sum(us_imports[un_code == korea_un_code &
                                 (kor_floor == 1 | kor_lumber_derivative == 1 |
                                  kor_aircraft == 1 | kor_auto == 1)]$us_imports_bn, na.rm = TRUE)

cat(sprintf("    Korea total: $%.1f billion, Excepted: $%.1f billion (%.1f%%)\n",
            korea_total, korea_excepted, 100 * korea_excepted / korea_total))

# -----------------------------------------------------------------------------
# 7.7: Switzerland & Liechtenstein (Type B - Tariff Floor)
# -----------------------------------------------------------------------------

cat("\n  7.7 Switzerland & Liechtenstein (Tariff Floor)...\n")

# Switzerland/Liechtenstein deal effective date: November 14, 2025
che_deal_effective_date <- as.Date("2025-11-14")

# Apply Switzerland/Liechtenstein deal terms only if policy_date >= November 14, 2025
if (policy_date >= che_deal_effective_date) {

  # EXCEPTION 1: UNCONDITIONAL EXCEPTIONS (from exceptions.csv)
  # Products on this list receive complete IEEPA exemption (ieepa_rate = 0)
  che_exception_codes <- get_exceptions("che_ieepa_floor_rate", "country_exception",
                                        policy_date, "CHE")
  cat(sprintf("    Loading Switzerland/Liechtenstein exceptions (%d codes)\n",
              length(che_exception_codes)))

  us_imports[un_code %in% swiss_liechtenstein &
             hs_8digit %in% che_exception_codes, `:=`(
    ieepa_rate = 0,
    che_exception = 1
  )]

  exception_value <- sum(us_imports[un_code %in% swiss_liechtenstein & che_exception == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Unconditional exceptions: $%.1f billion\n", exception_value))

  # EXCEPTION 2: CIVIL AIRCRAFT
  # Mark aircraft products; rate weighting applied in Section 8 using che_civil_aircraft_share
  us_imports[un_code %in% swiss_liechtenstein & hs_8digit %in% aircraft_main, `:=`(
    che_aircraft = 1,
    wto_aircraft = 1
  )]

  aircraft_value <- sum(us_imports[un_code %in% swiss_liechtenstein & che_aircraft == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Civil aircraft (%.0f%% share): $%.1f billion\n",
              che_civil_aircraft_share * 100, aircraft_value))

  # EXCEPTION 3: NON-PATENTED PHARMACEUTICALS
  # Uses unified pharma_hs_codes loaded at start of Section 7
  # Mark pharma products and apply share-weighted IEEPA exemption
  # Formula: ieepa_rate = (1 - nonpatented_share) * ieepa_rate
  # With 100% non-patented share: ieepa_rate = 0
  us_imports[un_code %in% swiss_liechtenstein &
             hs_8digit %in% pharma_hs_codes, `:=`(
    ieepa_rate = (1 - che_nonpatented_pharma_share) * ieepa_rate,
    che_pharma = 1
  )]

  pharma_value <- sum(us_imports[un_code %in% swiss_liechtenstein & che_pharma == 1]$us_imports_bn, na.rm = TRUE)
  cat(sprintf("    Non-patented pharma (%.0f%% share): $%.1f billion\n",
              che_nonpatented_pharma_share * 100, pharma_value))

  # EXCEPTION 4: RECIPROCAL RATE FLOOR (Applied via IEEPA component)
  # Formula: ieepa_rate = pmax(floor, hts_rate) - hts_rate ensures total >= 15%
  # Applies ONLY to products in reciprocal rate scope
  # Excludes: Annex 2, statutory IEEPA, unconditional exceptions, pharma (already have share-weighted IEEPA)
  us_imports[un_code %in% swiss_liechtenstein &
             rr_exception == 0 &
             ieepa_statute_exception == 0 &
             che_exception == 0 &
             che_pharma == 0, `:=`(
    ieepa_rate = pmax(che_ieepa_floor_rate, hts_rate) - hts_rate,
    che_floor = 1
  )]

  # Summary statistics
  swiss_total <- sum(us_imports[un_code %in% swiss_liechtenstein]$us_imports_bn, na.rm = TRUE)
  swiss_floor_applied <- sum(us_imports[un_code %in% swiss_liechtenstein & che_floor == 1]$us_imports_bn, na.rm = TRUE)
  swiss_excepted <- sum(us_imports[un_code %in% swiss_liechtenstein &
                                   (rr_exception == 1 | ieepa_statute_exception == 1 | che_exception == 1 | che_pharma == 1)]$us_imports_bn, na.rm = TRUE)

  cat(sprintf("    IEEPA floor (%.0f%%): $%.1f billion\n", che_ieepa_floor_rate, swiss_floor_applied))
  cat(sprintf("    Excepted (Annex 2 + statutory + unconditional + pharma): $%.1f billion\n", swiss_excepted))
  cat(sprintf("    Switzerland & Liechtenstein total: $%.1f billion\n", swiss_total))

} else {
  cat(sprintf("    Switzerland/Liechtenstein deal NOT active (effective %s, policy_date %s)\n",
              che_deal_effective_date, policy_date))
}

# -----------------------------------------------------------------------------
# 7.99: COUNTRY-SPECIFIC SURCHARGES (Across-the-Board Tariff Additions)
# -----------------------------------------------------------------------------
# Apply additional tariffs that stack on top of everything else (like S301)
# Activated only when country_surcharge_XXX rates exist in rates.csv
# Supports positive scope (only specific products) or negative scope (all except)
# -----------------------------------------------------------------------------

cat("\n  7.99 Country-Specific Surcharges...\n")

# Discover which countries have surcharge rates defined for this scenario
surcharge_rate_types <- rates[grepl("^country_surcharge_", rate_type) &
                               scenario == SCENARIO_NAME,
                              unique(rate_type)]

if (length(surcharge_rate_types) == 0) {
  cat("    No country surcharges defined for this scenario\n")
} else {
  cat(sprintf("    Found %d country surcharge rate(s) defined\n",
              length(surcharge_rate_types)))

  for (rate_type_name in surcharge_rate_types) {
    # Extract UN code from rate_type (e.g., "country_surcharge_699" -> 699)
    un_code_str <- sub("^country_surcharge_", "", rate_type_name)
    target_un_code <- as.integer(un_code_str)

    # Get the surcharge rate (time-dependent via get_rate)
    surcharge_rate <- get_rate(rate_type_name)

    if (surcharge_rate == 0) {
      cat(sprintf("    %s: Rate is 0 or not yet effective (skipped)\n",
                  rate_type_name))
      next
    }

    # Determine scope mode
    # Priority: surcharge_scope (positive) > surcharge_exception (negative) > all products
    positive_scope_codes <- get_exceptions(rate_type_name, "surcharge_scope", policy_date)
    negative_scope_codes <- get_exceptions(rate_type_name, "surcharge_exception", policy_date)

    # Get country name for logging
    country_name <- us_imports[un_code == target_un_code, first(exporter)]
    if (is.na(country_name)) country_name <- paste0("UN:", target_un_code)

    if (length(positive_scope_codes) > 0) {
      # POSITIVE SCOPE: Apply ONLY to listed products
      us_imports[un_code == target_un_code & hs_8digit %in% positive_scope_codes,
                 country_surcharge_rate := surcharge_rate]

      affected_value <- sum(us_imports[un_code == target_un_code &
                                        hs_8digit %in% positive_scope_codes]$us_imports_bn,
                            na.rm = TRUE)
      cat(sprintf("    %s (%s): +%.0f%% on %d specific products ($%.1f billion)\n",
                  rate_type_name, country_name, surcharge_rate,
                  length(positive_scope_codes), affected_value))

    } else if (length(negative_scope_codes) > 0) {
      # NEGATIVE SCOPE: Apply to ALL products EXCEPT listed
      us_imports[un_code == target_un_code & !(hs_8digit %in% negative_scope_codes),
                 country_surcharge_rate := surcharge_rate]

      affected_rows <- us_imports[un_code == target_un_code &
                                   !(hs_8digit %in% negative_scope_codes), .N]
      affected_value <- sum(us_imports[un_code == target_un_code &
                                        !(hs_8digit %in% negative_scope_codes)]$us_imports_bn,
                            na.rm = TRUE)
      excepted_value <- sum(us_imports[un_code == target_un_code &
                                        hs_8digit %in% negative_scope_codes]$us_imports_bn,
                            na.rm = TRUE)
      cat(sprintf("    %s (%s): +%.0f%% on all except %d products ($%.1f billion affected, $%.1f billion excepted)\n",
                  rate_type_name, country_name, surcharge_rate,
                  length(negative_scope_codes), affected_value, excepted_value))

    } else {
      # NO SCOPE: Apply to ALL products from this origin
      us_imports[un_code == target_un_code,
                 country_surcharge_rate := surcharge_rate]

      affected_rows <- us_imports[un_code == target_un_code, .N]
      affected_value <- sum(us_imports[un_code == target_un_code]$us_imports_bn,
                            na.rm = TRUE)
      cat(sprintf("    %s (%s): +%.0f%% on ALL products (%d rows, $%.1f billion)\n",
                  rate_type_name, country_name, surcharge_rate,
                  affected_rows, affected_value))
    }
  }
}

# Summary statistics for country surcharges
surcharge_affected <- sum(us_imports$country_surcharge_rate > 0)
surcharge_value <- sum(us_imports[country_surcharge_rate > 0]$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Total: %d products with country surcharge ($%.1f billion)\n",
            surcharge_affected, surcharge_value))

cat("\n  Section 7 complete: Country exceptions applied\n")
cat("\n")

# =============================================================================
# SECTION 8: CALCULATE FINAL APPLIED RATE
# =============================================================================
# Calculate final rate using FOUR distinct formulas based on tariff applicability:
# NOTE: country_surcharge_rate (Section 7.99) is always added at the end (never weighted)
#
# 0. NO S232, NO IEEPA, NO EMERGENCY (most basic):
#    rate = hts_weighted + s301 + country_surcharge
#    (Only MFN/preferential HTS rate + China S301 + country surcharge if applicable)
#
# 1. TRANSPORT S232 (auto/MHDV/PV parts):
#    rate = hts_weighted + s232_weighted + emergency_weighted + s301 + country_surcharge
#    (NO IEEPA - transport S232 replaces IEEPA; emergency always stacks additively)
#
# 2. MATERIALS S232 (steel/alu/copper/lumber) with AIRCRAFT SHARE and EMERGENCY STACKING:
#    EMERGENCY STACKING RULES:
#    - China opioid emergency: ADDITIVE to ALL S232 (emergency_additive = 1)
#    - Canada/Mexico border emergency: ADDITIVE only to COPPER S232 (emergency_additive = 1)
#    - Other materials S232: emergency inside weighted bracket (emergency_additive = 0)
#
#    When emergency_additive = 1:
#      rate = hts_weighted +
#             (1 - aircraft_share) * [content_share × s232_weighted +
#                                     (1 - content_share) × ieepa_weighted] +
#             emergency_weighted + s301 + country_surcharge
#
#    When emergency_additive = 0:
#      rate = hts_weighted +
#             (1 - aircraft_share) * [content_share × s232_weighted +
#                                     (1 - content_share) × (ieepa_weighted + emergency_weighted)] +
#             s301 + country_surcharge
#
# 3. NO S232, but IEEPA and/or EMERGENCY present:
#    rate = hts_weighted + ieepa_weighted + emergency_weighted + s301 + country_surcharge
#    (Standard additive formula)
# =============================================================================

cat("SECTION 8: Calculate Final Applied Rates\n")
cat("------------------------------------------\n")

cat("  Four-formula approach for complete coverage:\n")
cat("  0. No new tariffs: hts + s301 + surcharge\n")
cat("  1. Transport S232: hts + s232 + emergency + s301 + surcharge (NO IEEPA)\n")
cat("  2. Materials S232: hts + (1-aircraft)*[share×s232 + (1-share)×ieepa] + emergency + s301 + surcharge\n")
cat("     (Emergency stacks additively for: China all S232, Canada/Mexico copper)\n")
cat("  3. No S232, IEEPA/emergency: hts + ieepa + emergency + s301 + surcharge\n\n")

# Step 1: Calculate hts_rate_weighted for USMCA countries
# USMCA partners get weighted HTS rates (preferential 0% for compliant imports)
# Uses product-level compliance rates from compliance_shares.csv
us_imports[, hts_rate_weighted := hts_rate]  # Default: no weighting

if (DISABLE_HTS_USMCA_WEIGHTING) {
  # Skip USMCA weighting - use observed rate as-is (for observational MFN sources)
  cat("  Step 1: USMCA HTS weighting DISABLED (observed rates used directly)\n")
  # hts_rate_weighted already set to hts_rate at line above
} else {
  # Apply USMCA preferential HTS weighting (product-level compliance)
  us_imports[un_code %in% c(canada_un_code, mexico_un_code),
             hts_rate_weighted := usmca_compliance * usmca_rate + (1 - usmca_compliance) * hts_rate]
  cat("  Step 1: Applied USMCA weighting to HTS rates (product-level compliance)\n")
}

# Step 1b: Calculate hts_rate_weighted for South Korea (KORUS)
# KORUS-compliant imports receive preferential (zero) HTS rate
# Uses flat country-level compliance rate based on observed 2024 US imports
if (DISABLE_HTS_KORUS_WEIGHTING) {
  # Skip KORUS weighting - use observed rate as-is (for observational MFN sources)
  cat("  Step 1b: KORUS HTS weighting DISABLED (observed rates used directly)\n")
  # hts_rate_weighted already set to hts_rate at initialization
} else {
  # Apply KORUS preferential HTS weighting (country-level compliance)
  us_imports[un_code == korea_un_code,
             hts_rate_weighted := korus_compliance_default * korus_rate + (1 - korus_compliance_default) * hts_rate]
  cat("  Step 1b: Applied KORUS weighting to HTS rates (50% compliance, observed 2024)\n")
}

# Step 2: Calculate emergency_rate_weighted for USMCA countries
# Canada/Mexico emergency rates are USMCA-weighted using product-level compliance
# Energy exception list determines base rate (10% vs 35%), NOT compliance
us_imports[, emergency_rate_weighted := emergency_rate]  # Default: no weighting

# Canadian energy (10% base rate): use product-level compliance
us_imports[can_northern_border_energy == 1,
           emergency_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * emergency_rate]

# Canada non-energy (35% base rate): use product-level compliance
us_imports[un_code == canada_un_code &
           can_northern_border == 1 &
           can_northern_border_energy == 0,
           emergency_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * emergency_rate]

# Mexico emergency (25% base rate): use product-level compliance
us_imports[un_code == mexico_un_code &
           mex_emergency == 1,
           emergency_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * emergency_rate]

cat("  Step 2: Applied USMCA weighting to emergency rates (product-level compliance)\n")

# Step 3: Calculate ieepa_rate_weighted for USMCA countries
# USMCA partners get weighted IEEPA rates using product-level compliance
us_imports[, ieepa_rate_weighted := ieepa_rate]  # Default: no weighting

# Apply USMCA weighting to IEEPA rates (product-level compliance)
us_imports[un_code %in% c(canada_un_code, mexico_un_code),
           ieepa_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * ieepa_rate]

cat("  Step 3: Applied USMCA weighting to IEEPA rates (product-level compliance)\n")

# Step 4: Calculate s232_rate_weighted for USMCA auto and MHDV products
# Uses product-level USMCA compliance
us_imports[, s232_rate_weighted := s232_rate]  # Default: no weighting

# AUTO S232 - Canada and Mexico (product-level compliance)
us_imports[s232_auto == 1 &
           un_code %in% c(canada_un_code, mexico_un_code),
           s232_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * s232_auto_rate]

# MHDV main - Canada and Mexico (product-level compliance)
us_imports[s232_mhdv == 1 &
           hs_8digit %in% mhdv_main &
           un_code %in% c(canada_un_code, mexico_un_code),
           s232_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * s232_mhdv_main_rate]

# MHDV parts - Canada and Mexico (product-level compliance)
us_imports[s232_mhdv == 1 &
           hs_8digit %in% parts_codes &
           un_code %in% c(canada_un_code, mexico_un_code),
           s232_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * s232_mhdv_parts_rate]

cat("  Step 4: Applied USMCA weighting to S232 auto and MHDV rates (product-level compliance)\n")

# Step 5: Create S232 type markers
# Distinguish transport S232 from materials S232
us_imports[, `:=`(
  transport_s232 = as.integer(s232_auto == 1 | s232_mhdv == 1 | s232_pv_parts == 1),
  materials_s232 = as.integer(s232_steel == 1 | s232_steel_derivative == 1 |
                              s232_alu == 1 | s232_alu_derivative == 1 |
                              s232_copper == 1 | s232_copper_derivative == 1 |
                              s232_lumber == 1)
)]

cat("  Step 5: Created S232 type markers (transport vs materials)\n")

# Step 6: Determine content_share for each product
# Effective incidence ratios from calibration (see memo_metal_ratio_assumptions.md)
# Main products: <100% reflects exclusion use, not physical content
# Derivatives: reflects both physical content and exclusions
us_imports[, content_share := 1.0]  # Default: products not in S232 scope

# S232 MAIN PRODUCTS: Apply exclusion-adjusted incidence ratios
us_imports[s232_steel == 1, content_share := steel_main_share]        # 85%
us_imports[s232_alu == 1, content_share := alu_main_share]            # 94%
us_imports[s232_copper == 1, content_share := copper_main_share]      # 70% (provisional)

# S232 DERIVATIVES: Apply derivative incidence ratios (from assumptions in Section 0)
us_imports[s232_steel_derivative == 1, content_share := steel_derivative_share]  # 40%
us_imports[s232_alu_derivative == 1, content_share := alu_derivative_share]      # 35%
# NOTE: No copper derivatives currently in S232 scope
us_imports[s232_lumber_derivative == 1, content_share := lumber_cabinet_share]   # 50%

# S232 LUMBER MAIN: Apply lumber main incidence ratio (from assumptions in Section 0)
us_imports[s232_lumber == 1, content_share := lumber_main_share]

# PV parts use end-use share (from assumptions in Section 0)
us_imports[s232_pv_parts == 1, content_share := pv_share_in_parts]

# MHDV parts use end-use share (from assumptions in Section 0)
us_imports[s232_mhdv == 1 & hs_8digit %in% parts_codes, content_share := mhdv_share_in_parts]

cat("  Step 6: Determined content_share (effective incidence) for each product\n")

# Step 7: Determine aircraft_share for each product (country-specific)
us_imports[, aircraft_share := 0]  # Default: no aircraft exception

# Apply country-specific aircraft shares (only for wto_aircraft products)
us_imports[wto_aircraft == 1 & un_code == brazil_un_code, aircraft_share := bra_civil_aircraft_share]
us_imports[wto_aircraft == 1 & un_code == japan_un_code, aircraft_share := jpn_civil_aircraft_share]
us_imports[wto_aircraft == 1 & un_code %in% eu_members, aircraft_share := eu_civil_aircraft_share]
us_imports[wto_aircraft == 1 & un_code == uk_un_code, aircraft_share := uk_civil_aircraft_share]
us_imports[wto_aircraft == 1 & un_code == korea_un_code, aircraft_share := kor_civil_aircraft_share]
us_imports[wto_aircraft == 1 & un_code %in% swiss_liechtenstein, aircraft_share := che_civil_aircraft_share]

cat("  Step 7: Determined aircraft_share for each product (country-specific)\n")

# Step 7b: Determine emergency_additive marker
# Identifies flows where emergency tariff stacks additively on S232 (not weighted)
# Policy: China opioid emergency is ADDITIVE to ALL S232; Canada/Mexico border is ADDITIVE only for copper
us_imports[, emergency_additive := 0L]

# China/HK: Emergency (opioid) is ADDITIVE to ALL S232 products
us_imports[un_code %in% china_hkg_codes & (transport_s232 == 1 | materials_s232 == 1), 
           emergency_additive := 1L]

# Canada/Mexico: Emergency (border security) is ADDITIVE only for COPPER S232
us_imports[un_code %in% c(canada_un_code, mexico_un_code) & 
           (s232_copper == 1 | s232_copper_derivative == 1),
           emergency_additive := 1L]

cat("  Step 7b: Determined emergency_additive marker (China all S232, Canada/Mexico copper)\n")

# Step 8: Calculate final rate using four-formula approach
us_imports[, rate := -100]  # Initialize to -100% to flag products with no rate calculated

# Formula 0: NO S232, NO IEEPA, NO EMERGENCY (most basic case)
# rate = hts_weighted + s301 + country_surcharge
# Only HTS base rate + China S301 (if applicable) + country surcharge (if applicable)
us_imports[transport_s232 == 0 & materials_s232 == 0 &
           ieepa_rate == 0 & emergency_rate == 0,
           rate := hts_rate_weighted + s301_rate + country_surcharge_rate]

# Formula 1: TRANSPORT S232
# rate = hts_weighted + s232_weighted + emergency_weighted + s301 + country_surcharge
# NO IEEPA (transport S232 replaces IEEPA), but emergency still applies
us_imports[transport_s232 == 1,
           rate := hts_rate_weighted + s232_rate_weighted + emergency_rate_weighted +
                   s301_rate + country_surcharge_rate]

# Formula 2: MATERIALS S232 (with emergency stacking for China and Canada/Mexico copper)
# For emergency_additive = 1 (China all S232, Canada/Mexico copper):
#   rate = hts_weighted + (1-aircraft) * [content×s232 + (1-content)×ieepa] + emergency + s301 + surcharge
# For emergency_additive = 0 (other materials S232):
#   rate = hts_weighted + (1-aircraft) * [content×s232 + (1-content)×(ieepa+emergency)] + s301 + surcharge
us_imports[materials_s232 == 1,
           rate := hts_rate_weighted +
                   (1 - aircraft_share) * (content_share * s232_rate_weighted +
                                           (1 - content_share) * (ieepa_rate_weighted +
                                                                  (1 - emergency_additive) * emergency_rate_weighted)) +
                   emergency_additive * emergency_rate_weighted +
                   s301_rate + country_surcharge_rate]

# Formula 3: NO S232, but IEEPA and/or EMERGENCY present
# rate = hts_weighted + ieepa_weighted + emergency_weighted + s301 + country_surcharge
# Standard additive formula for products subject to reciprocal/emergency tariffs
us_imports[transport_s232 == 0 & materials_s232 == 0 &
           (ieepa_rate > 0 | emergency_rate > 0),
           rate := hts_rate_weighted + ieepa_rate_weighted + emergency_rate_weighted +
                   s301_rate + country_surcharge_rate]

cat("  Step 8: Calculated main rate column using four-formula approach\n")

# Step 9: Create rate_formula column to explain aggregation logic
us_imports[, rate_formula := ""]

# Formula 0: HTS + S301
us_imports[transport_s232 == 0 & materials_s232 == 0 &
           ieepa_rate == 0 & emergency_rate == 0,
           rate_formula := "HTS + S301"]

# Formula 1: Transport S232 (determine specific type)
us_imports[transport_s232 == 1 & s232_auto == 1,
           rate_formula := "Transport S232: HTS + S232(auto) + Emergency + S301"]
us_imports[transport_s232 == 1 & s232_mhdv == 1,
           rate_formula := "Transport S232: HTS + S232(MHDV) + Emergency + S301"]
us_imports[transport_s232 == 1 & s232_pv_parts == 1,
           rate_formula := "Transport S232: HTS + S232(PV parts) + Emergency + S301"]

# Formula 2: Materials S232 (determine specific material)
# When emergency_additive = 1: Emergency stacks additively outside weighted bracket
# When emergency_additive = 0: Emergency is inside weighted bracket (weighted with IEEPA)

# Steel - with/without emergency stacking
us_imports[materials_s232 == 1 & (s232_steel == 1 | s232_steel_derivative == 1) & emergency_additive == 1,
           rate_formula := "Materials S232 (steel): HTS + weighted[S232, IEEPA] + Emergency + S301"]
us_imports[materials_s232 == 1 & (s232_steel == 1 | s232_steel_derivative == 1) & emergency_additive == 0,
           rate_formula := "Materials S232 (steel): HTS + weighted[S232, (IEEPA+Emergency)] + S301"]

# Aluminum - with/without emergency stacking
us_imports[materials_s232 == 1 & (s232_alu == 1 | s232_alu_derivative == 1) & emergency_additive == 1,
           rate_formula := "Materials S232 (aluminum): HTS + weighted[S232, IEEPA] + Emergency + S301"]
us_imports[materials_s232 == 1 & (s232_alu == 1 | s232_alu_derivative == 1) & emergency_additive == 0,
           rate_formula := "Materials S232 (aluminum): HTS + weighted[S232, (IEEPA+Emergency)] + S301"]

# Copper - with/without emergency stacking
us_imports[materials_s232 == 1 & (s232_copper == 1 | s232_copper_derivative == 1) & emergency_additive == 1,
           rate_formula := "Materials S232 (copper): HTS + weighted[S232, IEEPA] + Emergency + S301"]
us_imports[materials_s232 == 1 & (s232_copper == 1 | s232_copper_derivative == 1) & emergency_additive == 0,
           rate_formula := "Materials S232 (copper): HTS + weighted[S232, (IEEPA+Emergency)] + S301"]

# Lumber - with/without emergency stacking
us_imports[materials_s232 == 1 & s232_lumber == 1 & emergency_additive == 1,
           rate_formula := "Materials S232 (lumber): HTS + weighted[S232, IEEPA] + Emergency + S301"]
us_imports[materials_s232 == 1 & s232_lumber == 1 & emergency_additive == 0,
           rate_formula := "Materials S232 (lumber): HTS + weighted[S232, (IEEPA+Emergency)] + S301"]

# Formula 3: HTS + IEEPA + Emergency + S301
us_imports[transport_s232 == 0 & materials_s232 == 0 &
           (ieepa_rate > 0 | emergency_rate > 0),
           rate_formula := "HTS + IEEPA + Emergency + S301"]

# Add surcharge suffix to rate_formula where applicable
us_imports[country_surcharge_rate > 0,
           rate_formula := paste0(rate_formula, " + Surcharge")]

cat("  Step 9: Created rate_formula column to explain aggregation logic\n")

# Step 10: Create contribution columns (sum of contributions = rate)
# These show how each rate component contributes to the final rate after all weighting
us_imports[, `:=`(
  hts_contrib = 0,
  s232_contrib = 0,
  ieepa_contrib = 0,
  emergency_contrib = 0,
  s301_contrib = 0,
  country_surcharge_contrib = 0
)]

# Formula 0: HTS + S301 + Surcharge only
us_imports[transport_s232 == 0 & materials_s232 == 0 & ieepa_rate == 0 & emergency_rate == 0,
           `:=`(hts_contrib = hts_rate_weighted,
                s301_contrib = s301_rate,
                country_surcharge_contrib = country_surcharge_rate)]

# Formula 1: Transport S232 (simple sum)
us_imports[transport_s232 == 1,
           `:=`(hts_contrib = hts_rate_weighted,
                s232_contrib = s232_rate_weighted,
                emergency_contrib = emergency_rate_weighted,
                s301_contrib = s301_rate,
                country_surcharge_contrib = country_surcharge_rate)]

# Formula 2: Materials S232 (content/aircraft weighted)
# rate = hts + (1-aircraft) * [content×s232 + (1-content)×(ieepa + (1-emergency_additive)×emergency)]
#            + emergency_additive×emergency + s301 + country_surcharge
us_imports[materials_s232 == 1,
           `:=`(hts_contrib = hts_rate_weighted,
                s232_contrib = (1 - aircraft_share) * content_share * s232_rate_weighted,
                ieepa_contrib = (1 - aircraft_share) * (1 - content_share) * ieepa_rate_weighted,
                emergency_contrib = (1 - aircraft_share) * (1 - content_share) * (1 - emergency_additive) * emergency_rate_weighted +
                                    emergency_additive * emergency_rate_weighted,
                s301_contrib = s301_rate,
                country_surcharge_contrib = country_surcharge_rate)]

# Formula 3: HTS + IEEPA + Emergency + S301 + Surcharge (simple sum)
us_imports[transport_s232 == 0 & materials_s232 == 0 & (ieepa_rate > 0 | emergency_rate > 0),
           `:=`(hts_contrib = hts_rate_weighted,
                ieepa_contrib = ieepa_rate_weighted,
                emergency_contrib = emergency_rate_weighted,
                s301_contrib = s301_rate,
                country_surcharge_contrib = country_surcharge_rate)]

cat("  Step 10: Created contribution columns (hts/s232/ieepa/emergency/s301/country_surcharge_contrib)\n")

# Verify complete coverage (all rows should have a rate calculated)
rows_with_rate <- sum(!is.na(us_imports$rate) & us_imports$rate >= 0)
rows_missing_rate <- sum(us_imports$rate == -100, na.rm = TRUE)
total_rows <- nrow(us_imports)

if(rows_with_rate != total_rows) {
  cat(sprintf("    ❌ ERROR: %s rows missing rate calculation!\n", format(total_rows - rows_with_rate, big.mark=",")))
  
  # Identify which products have missing rates
  missing_rate_data <- us_imports[rate == -100, .(
    missing_count = .N,
    total_value = sum(value, na.rm = TRUE)
  ), by = .(country, hs6)]
  
  cat(sprintf("       Missing rates for %s unique country-HS6 combinations\n", 
              format(nrow(missing_rate_data), big.mark=",")))
  cat(sprintf("       Total trade value affected: $%s\n", 
              format(round(sum(missing_rate_data$total_value, na.rm = TRUE)), big.mark=",")))
  
  # Show sample of products with missing rates
  if(nrow(missing_rate_data) > 0) {
    cat("       Sample of products with missing rates:\n")
    sample_missing <- head(missing_rate_data[order(-total_value)], 10)
    for(i in seq_len(nrow(sample_missing))) {
      cat(sprintf("         - %s (HS6: %s): %s rows, $%s trade value\n",
                  sample_missing$country[i],
                  sample_missing$hs6[i],
                  format(sample_missing$missing_count[i], big.mark=","),
                  format(round(sample_missing$total_value[i]), big.mark=",")))
    }
  }
  
  stop("Rate calculation incomplete. All products must have a calculated rate.")
} else {
  cat(sprintf("    ✓ Complete coverage: %s rows calculated\n", format(total_rows, big.mark=",")))
}

# Validation checks
cat("\n  Validation checks...\n")

neg_rates <- sum(us_imports$rate < 0, na.rm = TRUE)
if(neg_rates > 0) {
  cat(sprintf("    WARNING: %d flows have negative rates\n", neg_rates))
}

high_rates <- sum(us_imports$rate > 200, na.rm = TRUE)
if(high_rates > 0) {
  cat(sprintf("    WARNING: %d flows have rates > 200%%\n", high_rates))
}

missing_rates <- sum(is.na(us_imports$rate))
if(missing_rates > 0) {
  cat(sprintf("    WARNING: %d flows have missing rates\n", missing_rates))
}

# Summary statistics
avg_rate <- weighted.mean(us_imports$rate, us_imports$us_imports_bn, na.rm = TRUE)
cat(sprintf("    Weighted average applied rate: %.2f%%\n", avg_rate))

cat("\n  Section 8 complete: Final rates calculated\n")
cat("\n")

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
        s232_auto, s232_mhdv, s232_pv_parts,
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
