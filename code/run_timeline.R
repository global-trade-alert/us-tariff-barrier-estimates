# =============================================================================
# TIMELINE RUNNER — Run tariff calculations across multiple policy dates
# =============================================================================
# Usage: Rscript code/run_timeline.R [options]
#   --start DATE        Start date (default: 2025-01-01)
#   --end DATE          End date (default: today)
#   --origins ISO,...   Filter output to specific origins (e.g., CHN,JPN,DEU)
#   --products HS,...   Filter output to specific HS8 codes or prefixes (e.g., 7206*,7207*)
#   --output-dir DIR    Output directory (default: results/timeline/)
#   --quiet             Suppress per-date progress output
#
# Efficiency: Loads import data ONCE, then re-runs rate assignment for each
# policy-change date. ~25s setup + ~5s per date.
# =============================================================================

library(data.table)
library(writexl)
library(dplyr)

# =============================================================================
# PARSE COMMAND-LINE ARGUMENTS
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(default)
  if (idx + 1 > length(args)) return(default)
  return(args[idx + 1])
}

has_flag <- function(flag) flag %in% args

start_date <- as.Date(parse_arg("--start", "2025-01-01"))
end_date <- as.Date(parse_arg("--end", as.character(Sys.Date())))
origins_arg <- parse_arg("--origins", NULL)
products_arg <- parse_arg("--products", NULL)
output_dir <- parse_arg("--output-dir", "results/timeline")
quiet <- has_flag("--quiet")

# Parse origins filter
origin_filter <- NULL
if (!is.null(origins_arg)) {
  origin_filter <- trimws(unlist(strsplit(origins_arg, ",")))
}

# Parse products filter (supports wildcards like 7206*)
product_filter <- NULL
product_prefixes <- NULL
if (!is.null(products_arg)) {
  raw_products <- trimws(unlist(strsplit(products_arg, ",")))
  # Separate exact codes from prefix patterns
  is_prefix <- grepl("\\*$", raw_products)
  product_prefixes <- sub("\\*$", "", raw_products[is_prefix])
  product_filter <- sprintf("%08s", raw_products[!is_prefix])
}

cat("=============================================================================\n")
cat("TIMELINE RUNNER — US Tariff Rate Evolution\n")
cat("=============================================================================\n")
cat(sprintf("  Date range: %s to %s\n", start_date, end_date))
if (!is.null(origin_filter)) cat(sprintf("  Origin filter: %s\n", paste(origin_filter, collapse = ", ")))
if (!is.null(product_filter) || !is.null(product_prefixes)) {
  cat(sprintf("  Product filter: %s\n", products_arg))
}
cat(sprintf("  Output directory: %s\n", output_dir))
cat("\n")

# =============================================================================
# PHASE 1: EXTRACT POLICY-CHANGE DATES
# =============================================================================

cat("Phase 1: Extracting policy-change dates...\n")

# Get scenarios directory
scenarios_dir <- "data/scenarios"

# Read rates.csv effective dates
rates_all <- fread(file.path(scenarios_dir, "rates.csv"))
rates_dates <- rates_all[scenario == "baseline" & !is.na(effective_date),
                         unique(as.Date(effective_date))]

# Read exceptions.csv effective dates
exceptions_all <- fread(file.path(scenarios_dir, "exceptions.csv"), sep = ";")
exceptions_dates <- unique(as.Date(exceptions_all$effective_date))

# Hardcoded deal dates from apply_rates.R
deal_dates <- as.Date(c(
  "2025-06-30",   # UK Economic Prosperity Deal
  "2025-09-01",   # EU deal
  "2025-09-16",   # Japan deal
  "2025-11-14"    # Switzerland/Liechtenstein deal
))

# Combine all dates, filter to range, remove far-future dates
all_dates <- unique(c(rates_dates, exceptions_dates, deal_dates, start_date))
all_dates <- all_dates[all_dates >= start_date & all_dates <= end_date & all_dates < as.Date("2090-01-01")]
policy_dates <- sort(all_dates)

cat(sprintf("  Found %d policy-change dates in [%s, %s]:\n", length(policy_dates), start_date, end_date))
# Print dates in rows of 5
for (i in seq(1, length(policy_dates), by = 5)) {
  batch <- policy_dates[i:min(i + 4, length(policy_dates))]
  cat(sprintf("    %s\n", paste(batch, collapse = ", ")))
}
cat("\n")

# =============================================================================
# PHASE 2: ONE-TIME SETUP (load import data, helpers, scenario config)
# =============================================================================

cat("Phase 2: One-time setup (loading import data)...\n")
setup_start <- Sys.time()

# --- Replicate lines 1-508 of us_tariff_calculation.R ---
# (scenario loading, helpers, country codes)

# Scenario config
if (!exists("SCENARIO_NAME") || is.null(SCENARIO_NAME) || SCENARIO_NAME == "") {
  env_scenario <- Sys.getenv("SCENARIO_NAME", unset = "")
  SCENARIO_NAME <- if (env_scenario != "") env_scenario else "baseline"
}

if (!exists("MFN_RATE_SOURCE") || is.null(MFN_RATE_SOURCE) || MFN_RATE_SOURCE == "") {
  env_mfn_source <- Sys.getenv("MFN_RATE_SOURCE", unset = "")
  MFN_RATE_SOURCE <- if (env_mfn_source != "") env_mfn_source else "hts_schedule"
}

if (!exists("DISABLE_HTS_USMCA_WEIGHTING") || is.null(DISABLE_HTS_USMCA_WEIGHTING)) {
  env_disable_usmca <- Sys.getenv("DISABLE_HTS_USMCA_WEIGHTING", unset = "")
  DISABLE_HTS_USMCA_WEIGHTING <- if (env_disable_usmca != "") toupper(env_disable_usmca) %in% c("TRUE", "1", "YES") else FALSE
}

if (!exists("DISABLE_HTS_KORUS_WEIGHTING") || is.null(DISABLE_HTS_KORUS_WEIGHTING)) {
  env_disable_korus <- Sys.getenv("DISABLE_HTS_KORUS_WEIGHTING", unset = "")
  DISABLE_HTS_KORUS_WEIGHTING <- if (env_disable_korus != "") toupper(env_disable_korus) %in% c("TRUE", "1", "YES") else FALSE
}

if (!exists("EXCLUDE_S301_TARIFFS") || is.null(EXCLUDE_S301_TARIFFS)) {
  env_exclude_s301 <- Sys.getenv("EXCLUDE_S301_TARIFFS", unset = "")
  EXCLUDE_S301_TARIFFS <- if (env_exclude_s301 != "") toupper(env_exclude_s301) %in% c("TRUE", "1", "YES") else FALSE
}

cat(sprintf("  Scenario: %s, MFN source: %s\n", SCENARIO_NAME, MFN_RATE_SOURCE))

# Load rate/share tables
rates_file <- file.path(scenarios_dir, "rates.csv")
shares_file <- file.path(scenarios_dir, "shares.csv")
all_rates <- fread(rates_file)
all_shares <- fread(shares_file)

# Merge scenario with baseline (same logic as main script)
baseline_rates <- all_rates[scenario == "baseline"]
baseline_shares <- all_shares[scenario == "baseline"]

if (SCENARIO_NAME == "baseline") {
  rates <- copy(baseline_rates)
  shares <- copy(baseline_shares)
} else {
  scenario_rates <- all_rates[scenario == SCENARIO_NAME]
  scenario_shares <- all_shares[scenario == SCENARIO_NAME]
  if (nrow(scenario_rates) > 0) {
    rates <- copy(scenario_rates)
    scenario_rate_types <- unique(scenario_rates$rate_type)
    baseline_to_add <- baseline_rates[!rate_type %in% scenario_rate_types]
    rates <- rbind(rates, baseline_to_add, fill = TRUE)
  } else {
    rates <- copy(baseline_rates)
  }
  if (nrow(scenario_shares) > 0) {
    shares <- copy(scenario_shares)
    baseline_to_add <- baseline_shares[!share_type %in% scenario_shares$share_type]
    shares <- rbind(shares, baseline_to_add, fill = TRUE)
  } else {
    shares <- copy(baseline_shares)
  }
}

# Helper functions (get_rate, get_share, get_country, etc.)
get_rate <- function(rate_type_name, as_of_date = NULL) {
  if (is.null(as_of_date)) {
    as_of_date <- if (exists("policy_date")) policy_date else as.Date("2099-12-31")
  }
  candidates <- rates[rate_type == rate_type_name]
  if (nrow(candidates) == 0) {
    if (grepl("^ieepa_topup_", rate_type_name)) return(0)
    if (grepl("^ieepa_floor_override_", rate_type_name)) return(0)
    if (grepl("^s232_floor_override_", rate_type_name)) return(0)
    if (grepl("^country_surcharge_", rate_type_name)) return(0)
    stop(paste("MISSING RATE:", rate_type_name))
  }
  if ("effective_date" %in% names(candidates)) {
    candidates[, eff_date := as.Date(effective_date)]
    candidates <- candidates[eff_date <= as_of_date]
    if (nrow(candidates) == 0) return(0)
    return(candidates[order(-eff_date)][1, rate])
  }
  return(candidates[1, rate])
}

get_country_topup <- function(un_code, as_of_date = NULL) {
  get_rate(paste0("ieepa_topup_", un_code), as_of_date)
}

get_country_ieepa_floor_override <- function(un_code, as_of_date = NULL) {
  rate_type_name <- paste0("ieepa_floor_override_", un_code)
  if (!rate_type_name %in% rates$rate_type) return(NA_real_)
  get_rate(rate_type_name, as_of_date)
}

get_country_s232_floor_override <- function(un_code, as_of_date = NULL) {
  rate_type_name <- paste0("s232_floor_override_", un_code)
  if (!rate_type_name %in% rates$rate_type) return(NA_real_)
  get_rate(rate_type_name, as_of_date)
}

get_share <- function(share_type_name) {
  val <- shares[share_type == share_type_name, share]
  if (length(val) == 0) stop(paste("MISSING SHARE:", share_type_name))
  return(val[1])
}

# Load reference databases
countries_file <- file.path(scenarios_dir, "countries.csv")
COUNTRIES_DB <- fread(countries_file, sep = ";")

country_groups_file <- file.path(scenarios_dir, "country_groups.csv")
COUNTRY_GROUPS_DB <- fread(country_groups_file, sep = ";")

mfn_rates_file <- file.path(scenarios_dir, "mfn_rates.csv")
MFN_RATES_DB <- fread(mfn_rates_file, colClasses = c(hs_8digit = "character"))
MFN_RATES_DB[, hs_8digit := sprintf("%08s", hs_8digit)]
MFN_RATES_DB[un_code == "", un_code := NA_character_]
MFN_RATES_DB[, un_code := as.integer(un_code)]

valid_mfn_sources <- c("hts_schedule", "mixed_schedule", "observed_product", "observed_bilateral", "baseline_boe")
if (!MFN_RATE_SOURCE %in% valid_mfn_sources) {
  stop(paste("Invalid MFN_RATE_SOURCE:", MFN_RATE_SOURCE))
}

get_mfn_rate <- function(hs_code, un_code_val = NULL, source = MFN_RATE_SOURCE) {
  hs_code <- sprintf("%08s", as.character(hs_code))
  bilateral_sources <- c("observed_bilateral", "baseline_boe")
  if (source %in% bilateral_sources && !is.null(un_code_val) && !is.na(un_code_val)) {
    rate <- MFN_RATES_DB[source_key == source & hs_8digit == hs_code & un_code == un_code_val, rate]
    if (length(rate) > 0 && !is.na(rate[1])) return(rate[1])
    if (source == "observed_bilateral") source <- "observed_product"
  }
  rate <- MFN_RATES_DB[source_key == source & hs_8digit == hs_code & is.na(un_code), rate]
  if (length(rate) > 0 && !is.na(rate[1])) return(rate[1])
  return(0)
}

get_country <- function(iso_3_code) {
  result <- COUNTRIES_DB[iso_3 == iso_3_code, un_code]
  if (length(result) == 0) stop(paste("Country not found:", iso_3_code))
  return(result[1])
}

get_country_group <- function(grp_name) {
  result <- COUNTRY_GROUPS_DB[group_name == grp_name, un_code]
  if (length(result) == 0) stop(paste("MISSING COUNTRY GROUP:", grp_name))
  return(result)
}

# Exceptions database
exceptions_file <- file.path(scenarios_dir, "exceptions.csv")
EXCEPTIONS_DB <- fread(exceptions_file, sep = ";")
EXCEPTIONS_DB[, hs_8digit := sprintf("%08s", hs_8digit)]
EXCEPTIONS_DB[, effective_date := as.Date(effective_date)]
EXCEPTIONS_DB[, end_date := as.Date(end_date)]

get_exceptions <- function(rate_type_name, exception_type = NULL, as_of_date = NULL, import_origin = NULL) {
  if (is.null(as_of_date)) {
    as_of_date <- if (exists("policy_date")) policy_date else as.Date("2099-12-31")
  }
  result <- copy(EXCEPTIONS_DB)
  result <- result[rate_type == rate_type_name]
  if (nrow(result) == 0) return(character())
  if (!is.null(exception_type)) {
    exc_type_filter <- exception_type
    result <- result[exception_type == exc_type_filter]
  }
  result <- result[effective_date <= as_of_date & (is.na(end_date) | end_date >= as_of_date)]
  if (!is.null(import_origin)) {
    origin_filter <- import_origin
    result <- result[import_origin %in% c(origin_filter, "ALL")]
  }
  return(unique(result$hs_8digit))
}

# Country code definitions (Section 0.1)
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

usmca_partners <- get_country_group("usmca")
china_hkg_codes <- get_country_group("china_pair")
swiss_liechtenstein <- get_country_group("swiss_pair")
eu_members <- get_country_group("eu_member")
sanctioned_countries <- get_country_group("sanctioned")

# --- Section 1: Load import data ---
us_imports <- fread("data/usitc_us_imports_2024.csv", sep = ";")

us_imports_2025_file <- "data/us_imports_2025.csv"
if (file.exists(us_imports_2025_file)) {
  us_imports_2025 <- fread(us_imports_2025_file, sep = ";")
  us_imports <- rbindlist(list(us_imports, us_imports_2025), fill = TRUE)
}

us_imports[, us_imports_bn := us_imports / 1e9]
us_imports[, hs_8digit := sprintf("%08s", hs_8digit)]

# Merge ISO codes
gta_jurisdictions <- fread("data/gta_jurisdiction_list.csv")
us_imports <- merge(us_imports, gta_jurisdictions[, .(un_code, iso_code)],
                    by = "un_code", all.x = TRUE)

# Load compliance shares
compliance_shares <- fread("data/compliance_shares.csv")
compliance_shares[, hs_8digit := sprintf("%08s", hs_8digit)]

# Scope limitations
us_imports <- us_imports[as.numeric(hs_8digit) < 99000000]
us_imports <- us_imports[!un_code %in% sanctioned_countries]

# Initialize rate columns
us_imports[, `:=`(
  s232_rate = 0, ieepa_rate = 0, emergency_rate = 0,
  s301_rate = 0, country_surcharge_rate = 0, rate = 0
)]

# Initialize boolean marker columns
us_imports[, `:=`(
  s232_auto = 0, s232_mhdv = 0, s232_pv_parts = 0,
  s232_steel = 0, s232_steel_derivative = 0, s232_steel_exempt = 0,
  s232_alu = 0, s232_alu_derivative = 0,
  s232_copper = 0, s232_copper_derivative = 0,
  s232_lumber = 0, s232_lumber_derivative = 0,
  rr_exception = 0, ieepa_statute_exception = 0, missing_mfn = 0,
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
  wto_aircraft = 0, chn_opioid = 0, sec301_tariff = 0,
  can_northern_border = 0, can_northern_border_energy = 0, mex_emergency = 0
)]

# Save clean checkpoint (before any rates applied)
us_imports_base <- copy(us_imports)

setup_time <- as.numeric(difftime(Sys.time(), setup_start, units = "secs"))
cat(sprintf("  Setup complete: %s rows, $%.1f billion (%.1fs)\n",
            format(nrow(us_imports_base), big.mark = ","),
            sum(us_imports_base$us_imports_bn, na.rm = TRUE),
            setup_time))
cat("\n")

# =============================================================================
# PHASE 3: LOOP OVER POLICY DATES
# =============================================================================

cat("Phase 3: Running rate calculations...\n")

results_list <- list()
loop_start <- Sys.time()

# NOTE: Use date_idx (not i) because apply_rates.R overwrites i in its Section 301 loop
for (date_idx in seq_along(policy_dates)) {
  date <- policy_dates[date_idx]
  iter_start <- Sys.time()

  # Restore clean state
  us_imports <- copy(us_imports_base)

  # Set policy date for this iteration
  policy_date <- date
  POLICY_DATE <- date

  # Suppress output during loop unless verbose
  if (quiet) {
    invisible(capture.output(source("code/apply_rates.R"), type = "output"))
  } else {
    cat(sprintf("\n--- Date %d/%d: %s ---\n", date_idx, length(policy_dates), date))
    source("code/apply_rates.R")
  }

  # Build country-level summary (trade-weighted averages)
  # Merge ISO codes for grouping (iso_code already present from setup)
  country_summary <- us_imports[, .(
    trade_value_bn = sum(us_imports_bn, na.rm = TRUE),
    avg_rate = weighted.mean(rate, us_imports_bn, na.rm = TRUE),
    avg_hts = weighted.mean(hts_rate, us_imports_bn, na.rm = TRUE),
    avg_ieepa = weighted.mean(ieepa_rate, us_imports_bn, na.rm = TRUE),
    avg_s232 = weighted.mean(s232_rate, us_imports_bn, na.rm = TRUE),
    avg_emergency = weighted.mean(emergency_rate, us_imports_bn, na.rm = TRUE),
    avg_s301 = weighted.mean(s301_rate, us_imports_bn, na.rm = TRUE),
    avg_surcharge = weighted.mean(country_surcharge_rate, us_imports_bn, na.rm = TRUE)
  ), by = .(iso3 = iso_code, country = exporter)]

  country_summary[, policy_date := date]

  results_list[[date_idx]] <- country_summary

  iter_time <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))
  avg_rate <- weighted.mean(us_imports$rate, us_imports$us_imports_bn, na.rm = TRUE)
  if (!quiet) {
    cat(sprintf("  Weighted avg rate: %.2f%% (%.1fs)\n", avg_rate, iter_time))
  } else {
    cat(sprintf("  [%d/%d] %s — avg rate: %.2f%% (%.1fs)\n",
                date_idx, length(policy_dates), date, avg_rate, iter_time))
  }
}

loop_time <- as.numeric(difftime(Sys.time(), loop_start, units = "secs"))
cat(sprintf("\n  Loop complete: %d dates in %.1fs (%.1fs/date)\n",
            length(policy_dates), loop_time, loop_time / length(policy_dates)))
cat("\n")

# =============================================================================
# PHASE 4: COMBINE AND EXPORT
# =============================================================================

cat("Phase 4: Combining results and exporting...\n")

timeline <- rbindlist(results_list)

# Reorder columns
setcolorder(timeline, c("policy_date", "iso3", "country", "trade_value_bn",
                         "avg_rate", "avg_hts", "avg_ieepa", "avg_s232",
                         "avg_emergency", "avg_s301", "avg_surcharge"))

# Apply origin filter
if (!is.null(origin_filter)) {
  timeline <- timeline[iso3 %in% origin_filter]
  cat(sprintf("  Filtered to origins: %s (%d rows)\n",
              paste(origin_filter, collapse = ", "), nrow(timeline)))
}

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Export by-origin summary
origin_file <- file.path(output_dir, "timeline_by_origin.csv")
fwrite(timeline, origin_file)
cat(sprintf("  Saved: %s (%d rows)\n", origin_file, nrow(timeline)))

# Export RData for programmatic access
rdata_file <- file.path(output_dir, "timeline.RData")
save(timeline, policy_dates, file = rdata_file)
cat(sprintf("  Saved: %s\n", rdata_file))

# If product filter specified, run last date with product-level detail
if (!is.null(product_filter) || !is.null(product_prefixes)) {
  cat("\n  Generating product-level detail for filtered products...\n")

  # Build product-level results across all dates
  product_results <- list()

  for (date_idx in seq_along(policy_dates)) {
    date <- policy_dates[date_idx]
    us_imports <- copy(us_imports_base)
    policy_date <- date
    POLICY_DATE <- date
    invisible(capture.output(source("code/apply_rates.R"), type = "output"))

    # Filter to matching products
    match_mask <- rep(FALSE, nrow(us_imports))
    if (!is.null(product_filter)) {
      match_mask <- match_mask | us_imports$hs_8digit %in% product_filter
    }
    if (!is.null(product_prefixes)) {
      for (prefix in product_prefixes) {
        match_mask <- match_mask | startsWith(us_imports$hs_8digit, prefix)
      }
    }

    if (sum(match_mask) > 0) {
      product_detail <- us_imports[match_mask, .(
        policy_date = date,
        hs_8digit, iso3 = iso_code,
        rate, hts_rate, ieepa_rate, s232_rate,
        emergency_rate, s301_rate, country_surcharge_rate,
        us_imports_bn
      )]
      product_results[[date_idx]] <- product_detail
    }
  }

  if (length(product_results) > 0) {
    product_timeline <- rbindlist(product_results)
    if (!is.null(origin_filter)) {
      product_timeline <- product_timeline[iso3 %in% origin_filter]
    }
    product_file <- file.path(output_dir, "timeline_by_product.csv")
    fwrite(product_timeline, product_file)
    cat(sprintf("  Saved: %s (%d rows)\n", product_file, nrow(product_timeline)))

    # Also save to RData
    save(timeline, product_timeline, policy_dates, file = rdata_file)
    cat(sprintf("  Updated: %s (added product_timeline)\n", rdata_file))
  }
}

# Print summary
cat("\n")
cat("=============================================================================\n")
cat("TIMELINE COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("  Dates: %d (%s to %s)\n", length(policy_dates),
            min(policy_dates), max(policy_dates)))
cat(sprintf("  Countries: %d\n", length(unique(timeline$iso3))))
if (!is.null(origin_filter)) {
  cat(sprintf("  Filtered origins: %s\n", paste(origin_filter, collapse = ", ")))
}
cat(sprintf("  Total runtime: %.1fs\n",
            as.numeric(difftime(Sys.time(), setup_start, units = "secs"))))
cat(sprintf("  Output: %s\n", output_dir))
cat("=============================================================================\n")
