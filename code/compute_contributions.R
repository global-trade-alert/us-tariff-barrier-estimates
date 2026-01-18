# =============================================================================
# COMPUTE TARIFF CONTRIBUTIONS
# =============================================================================
# Shared function to compute formula-based tariff contributions.
# Used by: tariff_composition.R, generate_scenario_report.R
#
# The contributions are calculated so they SUM to the total rate:
#   hts_contribution + s232_contribution + ieepa_contribution +
#   emergency_contribution + s301_contribution = rate
#
# Formula Types:
#   0: No S232, no IEEPA, no emergency (Annex 2 products)
#   1: Transport S232 (auto/MHDV/PV parts) - IEEPA excluded
#   2: Materials S232 (steel/alu/copper/lumber) - with content_share/aircraft_share
#   3: No S232, but IEEPA and/or emergency present
# =============================================================================

library(data.table)

#' Compute tariff contributions for each flow
#'
#' @param data data.table with tariff rate columns
#' @param ieepa_baseline_rate IEEPA baseline rate (default 10%)
#' @param verbose Print progress messages (default FALSE)
#' @return data.table with contribution columns added
compute_contributions <- function(data, ieepa_baseline_rate = 10, verbose = FALSE) {

  if (verbose) cat("   Computing tariff contributions (formula-based)...\n")

  # Ensure data is a data.table
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }

  # ---------------------------------------------------------------------------
  # Ensure required columns exist with defaults
  # ---------------------------------------------------------------------------
  if (!"hts_rate_weighted" %in% names(data)) {
    data[, hts_rate_weighted := hts_rate]
  }
  if (!"s232_rate_weighted" %in% names(data)) {
    data[, s232_rate_weighted := s232_rate]
  }
  if (!"ieepa_rate_weighted" %in% names(data)) {
    data[, ieepa_rate_weighted := ieepa_rate]
  }
  if (!"emergency_rate_weighted" %in% names(data)) {
    data[, emergency_rate_weighted := emergency_rate]
  }
  if (!"content_share" %in% names(data)) {
    data[, content_share := 1.0]
  }
  if (!"aircraft_share" %in% names(data)) {
    data[, aircraft_share := 0.0]
  }

  # Ensure S232 flag columns exist (default to 0)
  s232_flags <- c("s232_auto", "s232_mhdv", "s232_pv_parts",
                  "s232_steel", "s232_steel_derivative",
                  "s232_alu", "s232_alu_derivative",
                  "s232_copper", "s232_copper_derivative",
                  "s232_lumber", "s232_lumber_derivative")
  for (flag in s232_flags) {
    if (!flag %in% names(data)) {
      data[, (flag) := 0L]
    }
  }

  # ---------------------------------------------------------------------------
  # Create helper flags for S232 product types
  # ---------------------------------------------------------------------------
  data[, `:=`(
    # Transport S232 (auto, MHDV, PV parts) - IEEPA excluded
    is_transport_s232 = (s232_auto == 1 | s232_mhdv == 1 | s232_pv_parts == 1),
    # Materials S232 (steel/alu/copper/lumber, including derivatives)
    is_materials_s232 = (s232_steel == 1 | s232_steel_derivative == 1 |
                           s232_alu == 1 | s232_alu_derivative == 1 |
                           s232_copper == 1 | s232_copper_derivative == 1 |
                           s232_lumber == 1 | s232_lumber_derivative == 1)
  )]

  # ---------------------------------------------------------------------------
  # Determine emergency_additive marker
  # Policy: China opioid emergency is ADDITIVE to ALL S232;
  #         Canada/Mexico border emergency is ADDITIVE only for copper
  # ---------------------------------------------------------------------------
  data[, emergency_additive := 0L]
  data[iso_code %in% c("CHN", "HKG") & (is_transport_s232 | is_materials_s232),
       emergency_additive := 1L]
  data[iso_code %in% c("CAN", "MEX") & (s232_copper == 1 | s232_copper_derivative == 1),
       emergency_additive := 1L]

  # ---------------------------------------------------------------------------
  # Split IEEPA into baseline (10%) and top-up components using WEIGHTED rates
  # ---------------------------------------------------------------------------
  data[, `:=`(
    ieepa_baseline_weighted = pmin(ieepa_rate_weighted, ieepa_baseline_rate *
                                     fifelse(ieepa_rate > 0, ieepa_rate_weighted / ieepa_rate, 0)),
    ieepa_topup_weighted = pmax(0, ieepa_rate_weighted -
                                  pmin(ieepa_rate_weighted, ieepa_baseline_rate *
                                         fifelse(ieepa_rate > 0, ieepa_rate_weighted / ieepa_rate, 0)))
  )]

  # ---------------------------------------------------------------------------
  # Initialize all contributions to 0
  # ---------------------------------------------------------------------------
  data[, `:=`(
    hts_contribution = 0.0,
    s232_contribution = 0.0,
    ieepa_baseline_contribution = 0.0,
    ieepa_topup_contribution = 0.0,
    emergency_contribution = 0.0,
    s301_contribution = 0.0
  )]

  if (verbose) cat("   Calculating contributions by formula type...\n")

  # ---------------------------------------------------------------------------
  # FORMULA 0: NO S232, NO IEEPA, NO EMERGENCY (most basic case)
  # rate = hts_weighted + s301
  # ---------------------------------------------------------------------------
  data[is_transport_s232 == FALSE & is_materials_s232 == FALSE &
       ieepa_rate == 0 & emergency_rate == 0, `:=`(
    hts_contribution = hts_rate_weighted,
    s301_contribution = s301_rate
  )]

  # ---------------------------------------------------------------------------
  # FORMULA 1: TRANSPORT S232 (auto/MHDV/PV parts)
  # rate = hts_weighted + s232_weighted + emergency_weighted + s301
  # NO IEEPA - transport S232 replaces IEEPA
  # ---------------------------------------------------------------------------
  data[is_transport_s232 == TRUE, `:=`(
    hts_contribution = hts_rate_weighted,
    s232_contribution = s232_rate_weighted,
    emergency_contribution = emergency_rate_weighted,
    s301_contribution = s301_rate
  )]

  # ---------------------------------------------------------------------------
  # FORMULA 2: MATERIALS S232 (steel/alu/copper/lumber)
  # When emergency_additive = 1 (China all S232, Canada/Mexico copper):
  #   rate = hts_weighted + (1-aircraft) * [content*s232_weighted +
  #                                         (1-content)*ieepa_weighted] +
  #          emergency_weighted + s301
  # When emergency_additive = 0 (other materials S232):
  #   rate = hts_weighted + (1-aircraft) * [content*s232_weighted +
  #                                         (1-content)*(ieepa_weighted+emergency_weighted)] +
  #          s301
  # ---------------------------------------------------------------------------

  # Materials S232 with emergency_additive = 1
  data[is_materials_s232 == TRUE & emergency_additive == 1L, `:=`(
    hts_contribution = hts_rate_weighted,
    s232_contribution = (1 - aircraft_share) * content_share * s232_rate_weighted,
    ieepa_baseline_contribution = (1 - aircraft_share) * (1 - content_share) * ieepa_baseline_weighted,
    ieepa_topup_contribution = (1 - aircraft_share) * (1 - content_share) * ieepa_topup_weighted,
    emergency_contribution = emergency_rate_weighted,
    s301_contribution = s301_rate
  )]

  # Materials S232 with emergency_additive = 0
  data[is_materials_s232 == TRUE & emergency_additive == 0L, `:=`(
    hts_contribution = hts_rate_weighted,
    s232_contribution = (1 - aircraft_share) * content_share * s232_rate_weighted,
    ieepa_baseline_contribution = (1 - aircraft_share) * (1 - content_share) * ieepa_baseline_weighted,
    ieepa_topup_contribution = (1 - aircraft_share) * (1 - content_share) * ieepa_topup_weighted,
    emergency_contribution = (1 - aircraft_share) * (1 - content_share) * emergency_rate_weighted,
    s301_contribution = s301_rate
  )]

  # ---------------------------------------------------------------------------
  # FORMULA 3: NO S232, but IEEPA and/or EMERGENCY present
  # rate = hts_weighted + ieepa_weighted + emergency_weighted + s301
  # ---------------------------------------------------------------------------
  data[is_transport_s232 == FALSE & is_materials_s232 == FALSE &
       (ieepa_rate > 0 | emergency_rate > 0), `:=`(
    hts_contribution = hts_rate_weighted,
    ieepa_baseline_contribution = ieepa_baseline_weighted,
    ieepa_topup_contribution = ieepa_topup_weighted,
    emergency_contribution = emergency_rate_weighted,
    s301_contribution = s301_rate
  )]

  # ---------------------------------------------------------------------------
  # Combined IEEPA contribution for aggregation
  # ---------------------------------------------------------------------------
  data[, ieepa_contribution := ieepa_baseline_contribution + ieepa_topup_contribution]

  if (verbose) cat("   Calculated contributions directly from rate formulas\n")

  return(data)
}


#' Validate that contributions sum to total rate
#'
#' @param data data.table with contribution columns
#' @param tolerance Maximum allowed difference (default 0.01 pp)
#' @param verbose Print validation results (default TRUE)
#' @return Maximum contribution difference
validate_contributions <- function(data, tolerance = 0.01, verbose = TRUE) {

  # Calculate sum of contributions
  data[, sum_contributions := hts_contribution + ieepa_contribution +
         s232_contribution + s301_contribution + emergency_contribution]
  data[, contribution_diff := abs(sum_contributions - rate)]

  max_diff <- max(data$contribution_diff, na.rm = TRUE)

  if (verbose) {
    if (max_diff > tolerance) {
      cat(sprintf("   WARNING: Max contribution difference from rate: %.4f pp\n", max_diff))
      problem_flows <- data[contribution_diff > tolerance, .(
        n_flows = .N,
        total_trade = sum(us_imports_bn)
      )]
      cat(sprintf("   Affected: %d flows, $%.2f billion trade\n",
                  problem_flows$n_flows, problem_flows$total_trade))
    } else {
      cat(sprintf("   Validation passed: contributions sum to rate (max diff: %.6f pp)\n", max_diff))
    }
  }

  return(max_diff)
}


#' Aggregate contributions to global weighted averages
#'
#' @param data data.table with contribution columns and us_imports_bn
#' @return list with global contribution averages
aggregate_contributions <- function(data) {
  total_imports <- sum(data$us_imports_bn, na.rm = TRUE)

  list(
    hts_layer = sum(data$hts_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    s232_layer = sum(data$s232_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    ieepa_baseline_layer = sum(data$ieepa_baseline_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    ieepa_topup_layer = sum(data$ieepa_topup_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    emergency_layer = sum(data$emergency_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    s301_layer = sum(data$s301_contribution * data$us_imports_bn, na.rm = TRUE) / total_imports,
    total_rate = sum(data$rate * data$us_imports_bn, na.rm = TRUE) / total_imports,
    total_imports_bn = total_imports
  )
}


#' Aggregate S232 contributions by sub-type (mutually exclusive)
#'
#' Creates mutually exclusive categories to avoid double-counting.
#' Priority order: Auto > MHDV/PV > Steel > Aluminum > Copper > Lumber
#' Note: Some products may be flagged as both steel and aluminum derivatives;
#'       these are attributed to steel to avoid double-counting.
#'
#' @param data data.table with contribution columns and S232 flag columns
#' @param total_imports Total imports for denominator (if NULL, calculates from data)
#' @return list with S232 sub-type contributions
aggregate_s232_subtypes <- function(data, total_imports = NULL) {
  if (is.null(total_imports)) {
    total_imports <- sum(data$us_imports_bn, na.rm = TRUE)
  }

  # Create mutually exclusive category flags (in priority order)
  # This avoids double-counting products flagged in multiple categories
  data[, `:=`(
    s232_cat_auto = (s232_auto == 1),
    s232_cat_mhdv = (s232_mhdv == 1 | s232_pv_parts == 1) & (s232_auto != 1),
    s232_cat_steel = (s232_steel == 1 | s232_steel_derivative == 1) &
                     (s232_auto != 1) & (s232_mhdv != 1) & (s232_pv_parts != 1),
    s232_cat_alu = (s232_alu == 1 | s232_alu_derivative == 1) &
                   (s232_auto != 1) & (s232_mhdv != 1) & (s232_pv_parts != 1) &
                   (s232_steel != 1) & (s232_steel_derivative != 1),
    s232_cat_copper = (s232_copper == 1 | s232_copper_derivative == 1) &
                      (s232_auto != 1) & (s232_mhdv != 1) & (s232_pv_parts != 1) &
                      (s232_steel != 1) & (s232_steel_derivative != 1) &
                      (s232_alu != 1) & (s232_alu_derivative != 1),
    s232_cat_lumber = (s232_lumber == 1 | s232_lumber_derivative == 1) &
                      (s232_auto != 1) & (s232_mhdv != 1) & (s232_pv_parts != 1) &
                      (s232_steel != 1) & (s232_steel_derivative != 1) &
                      (s232_alu != 1) & (s232_alu_derivative != 1) &
                      (s232_copper != 1) & (s232_copper_derivative != 1)
  )]

  # Helper function for calculating weighted contribution
  calc_subtype <- function(filter_expr) {
    subset <- data[eval(filter_expr)]
    if (nrow(subset) == 0) return(0)
    sum(subset$s232_contribution * subset$us_imports_bn, na.rm = TRUE) / total_imports
  }

  result <- list(
    steel = calc_subtype(quote(s232_cat_steel == TRUE)),
    aluminum = calc_subtype(quote(s232_cat_alu == TRUE)),
    copper = calc_subtype(quote(s232_cat_copper == TRUE)),
    lumber = calc_subtype(quote(s232_cat_lumber == TRUE)),
    auto = calc_subtype(quote(s232_cat_auto == TRUE)),
    mhdv_parts = calc_subtype(quote(s232_cat_mhdv == TRUE))
  )

  # Clean up temporary columns
  data[, c("s232_cat_auto", "s232_cat_mhdv", "s232_cat_steel",
           "s232_cat_alu", "s232_cat_copper", "s232_cat_lumber") := NULL]

  return(result)
}
