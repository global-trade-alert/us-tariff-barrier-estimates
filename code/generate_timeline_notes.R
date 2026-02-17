# =============================================================================
# TIMELINE NARRATIVE ANNOTATION — Generate per-country policy narratives
# =============================================================================
# Usage: Rscript code/generate_timeline_notes.R [options]
#   --input FILE          Timeline CSV (default: results/timeline/timeline_by_origin.csv)
#   --output FILE         Output xlsx path (default: adds _annotated to input name)
#   --event-registry FILE Path to event registry CSV (default: data/scenarios/event_registry.csv)
#
# Reads a timeline CSV produced by run_timeline.R, matches each (date, country)
# row against a hand-curated event registry, computes rate-component deltas,
# and generates short narratives explaining what changed and why.
# =============================================================================

library(data.table)
library(writexl)

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

input_file <- parse_arg("--input", "results/timeline/timeline_by_origin.csv")
output_file <- parse_arg("--output", NULL)
registry_file <- parse_arg("--event-registry", "data/scenarios/event_registry.csv")

# Default output: replace .csv with _annotated.xlsx or append _annotated.xlsx
if (is.null(output_file)) {
  output_file <- sub("\\.csv$", "_annotated.xlsx", input_file)
  if (output_file == input_file) {
    output_file <- paste0(input_file, "_annotated.xlsx")
  }
}

cat("=============================================================================\n")
cat("TIMELINE NARRATIVE ANNOTATION\n")
cat("=============================================================================\n")
cat(sprintf("  Input:          %s\n", input_file))
cat(sprintf("  Event registry: %s\n", registry_file))
cat(sprintf("  Output:         %s\n", output_file))
cat("\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading timeline data...\n")
timeline <- fread(input_file)
timeline[, policy_date := as.Date(policy_date)]
setorder(timeline, iso3, policy_date)

cat(sprintf("  %d rows, %d countries, %d dates\n",
            nrow(timeline),
            length(unique(timeline$iso3)),
            length(unique(timeline$policy_date))))

cat("Loading event registry...\n")
registry <- fread(registry_file)
registry[, effective_date := as.Date(effective_date)]

cat(sprintf("  %d events across %d dates\n",
            nrow(registry),
            length(unique(registry$effective_date))))
cat("\n")

# Load country group memberships for EU_MEMBERS matching
# Try to load from the same scenarios directory as registry
scenarios_dir <- dirname(registry_file)
country_groups_file <- file.path(scenarios_dir, "country_groups.csv")
countries_file <- file.path(scenarios_dir, "countries.csv")

eu_iso3 <- character(0)
iso3_to_un <- list()

if (file.exists(country_groups_file) && file.exists(countries_file)) {
  cg <- fread(country_groups_file, sep = ";")
  cc <- fread(countries_file, sep = ";")
  eu_un_codes <- cg[group_name == "eu_member", un_code]
  eu_iso3 <- cc[un_code %in% eu_un_codes, iso_3]
  # Build un_code -> iso3 lookup
  for (r in seq_len(nrow(cc))) {
    iso3_to_un[[cc$iso_3[r]]] <- cc$un_code[r]
  }
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Rate component columns in the timeline
RATE_COMPONENTS <- c(
  ieepa = "avg_ieepa",
  s232 = "avg_s232",
  emergency = "avg_emergency",
  s301 = "avg_s301",
  surcharge = "avg_surcharge",
  hts = "avg_hts"
)

#' Check if a country is affected by an event
#'
#' @param affected_str The affected_countries field from event registry
#' @param iso3 The country's ISO3 code
#' @return TRUE if the country is affected
country_is_affected <- function(affected_str, iso3) {
  if (is.na(affected_str) || affected_str == "") return(FALSE)
  if (affected_str == "ALL") return(TRUE)

  # Handle ALL_EXCEPT:XXX:YYY
  if (startsWith(affected_str, "ALL_EXCEPT:")) {
    excluded <- unlist(strsplit(sub("^ALL_EXCEPT:", "", affected_str), ":"))
    return(!iso3 %in% excluded)
  }

  # Handle EU_MEMBERS
  if (affected_str == "EU_MEMBERS" || grepl("EU_MEMBERS", affected_str)) {
    parts <- unlist(strsplit(affected_str, ":"))
    if ("EU_MEMBERS" %in% parts && iso3 %in% eu_iso3) return(TRUE)
    if (iso3 %in% parts) return(TRUE)
    return(FALSE)
  }

  # Explicit list: GBR:JPN:EU_MEMBERS or CHN:HKG
  parts <- unlist(strsplit(affected_str, ":"))
  if (iso3 %in% parts) return(TRUE)
  if ("EU_MEMBERS" %in% parts && iso3 %in% eu_iso3) return(TRUE)

  return(FALSE)
}

#' Classify impact magnitude
#'
#' @param delta Absolute percentage-point change
#' @param threshold_major Above this is "major"
#' @param threshold_minor Above this is "minor", below is "negligible"
#' @return Character: "major", "minor", or "negligible"
classify_impact <- function(delta, threshold_major = 0.1, threshold_minor = 0.01) {
  abs_d <- abs(delta)
  if (abs_d >= threshold_major) return("major")
  if (abs_d >= threshold_minor) return("minor")
  return("negligible")
}

#' Format a rate delta for display
#'
#' @param delta Numeric delta in percentage points
#' @return Formatted string like "+2.3pp" or "-0.5pp"
fmt_delta <- function(delta) {
  if (is.na(delta) || abs(delta) < 0.001) return("0.0pp")
  sign_str <- if (delta > 0) "+" else ""
  sprintf("%s%.1fpp", sign_str, delta)
}

#' Get the friendly component name
component_label <- function(comp) {
  labels <- c(
    ieepa = "IEEPA",
    s232 = "Section 232",
    emergency = "emergency",
    s301 = "Section 301",
    surcharge = "surcharge",
    hts = "HTS/MFN"
  )
  if (comp %in% names(labels)) return(labels[comp])
  return(comp)
}

#' Compose narrative for a single (country, date) pair
#'
#' Groups events by rate component to avoid double-counting deltas when
#' multiple events affect the same component on the same date.
#'
#' @param events_dt Data.table of registry events for this date
#' @param deltas Named numeric vector of rate-component deltas
#' @param total_delta Total avg_rate delta
#' @param iso3 Country ISO3 code
#' @param date The policy date
#' @param is_first TRUE if this is the first date (baseline)
#' @return Character string narrative
compose_narrative <- function(events_dt, deltas, total_delta, iso3, date, is_first = FALSE) {

  if (is_first) {
    # Baseline: describe pre-existing state
    active_parts <- character(0)
    for (evt_i in seq_len(nrow(events_dt))) {
      evt <- events_dt[evt_i]
      if (country_is_affected(evt$affected_countries, iso3)) {
        active_parts <- c(active_parts, evt$description)
      }
    }
    if (length(active_parts) == 0) {
      return("Baseline date. Pre-existing HTS/MFN tariff rates apply.")
    }
    return(paste0("Baseline: ", paste(active_parts, collapse = " ")))
  }

  if (nrow(events_dt) == 0) {
    if (abs(total_delta) < 0.005) {
      return("No registered policy events on this date. Rates unchanged.")
    }
    return(sprintf("No registered policy events. Rate change of %s on avg_rate (likely from exception scope change).",
                   fmt_delta(total_delta)))
  }

  # Separate events into relevant (affects this country) vs irrelevant
  relevant_events <- list()
  irrelevant_events <- list()

  for (evt_i in seq_len(nrow(events_dt))) {
    evt <- events_dt[evt_i]
    if (country_is_affected(evt$affected_countries, iso3)) {
      relevant_events <- c(relevant_events, list(evt))
    } else {
      irrelevant_events <- c(irrelevant_events, list(evt))
    }
  }

  # Group relevant events by their primary rate component to avoid double-counting.
  # Each component's delta is reported once, attributed to all events touching it.
  # Events with multi-component (e.g. "ieepa:s232") are placed in their own group.
  comp_groups <- list()  # component_key -> list(events, components, delta)

  for (evt in relevant_events) {
    evt_components <- sort(trimws(unlist(strsplit(evt$rate_component, ":"))))
    comp_key <- paste(evt_components, collapse = ":")

    if (is.null(comp_groups[[comp_key]])) {
      # Compute delta for this component group (sum of component deltas, each counted once)
      group_delta <- 0
      group_details <- character(0)
      for (comp in evt_components) {
        if (comp %in% names(deltas)) {
          d <- deltas[[comp]]
          if (abs(d) > 0.001) {
            group_delta <- group_delta + d
            group_details <- c(group_details,
                               sprintf("%s on %s", fmt_delta(d), component_label(comp)))
          }
        }
      }
      comp_groups[[comp_key]] <- list(
        events = list(evt),
        components = evt_components,
        delta = group_delta,
        details = group_details
      )
    } else {
      comp_groups[[comp_key]]$events <- c(comp_groups[[comp_key]]$events, list(evt))
    }
  }

  # Sort groups by absolute delta (largest impact first)
  if (length(comp_groups) > 0) {
    abs_deltas <- sapply(comp_groups, function(g) abs(g$delta))
    comp_groups <- comp_groups[order(-abs_deltas)]
  }

  # Build narrative parts
  parts <- character(0)

  for (group in comp_groups) {
    group_impact <- classify_impact(group$delta)
    events <- group$events
    n_events <- length(events)

    if (group_impact == "major") {
      if (n_events == 1) {
        evt <- events[[1]]
        detail_str <- paste(group$details, collapse = ", ")
        parts <- c(parts, sprintf("%s: %s Impact: %s on avg_rate (via %s).",
                                  evt$event_name, evt$description,
                                  fmt_delta(group$delta), detail_str))
      } else {
        # Multiple events on same component — lead with the most significant
        # then list others as contributing
        lead_evt <- events[[1]]
        detail_str <- paste(group$details, collapse = ", ")
        other_names <- sapply(events[-1], function(e) e$event_name)
        parts <- c(parts, sprintf(
          "%s: %s Combined impact with %s: %s on avg_rate (via %s).",
          lead_evt$event_name, lead_evt$description,
          paste(other_names, collapse = ", "),
          fmt_delta(group$delta), detail_str))
      }
    } else if (group_impact == "minor") {
      event_names <- paste(sapply(events, function(e) e$event_name), collapse = " + ")
      parts <- c(parts, sprintf("%s: minor impact (%s).", event_names, fmt_delta(group$delta)))
    } else {
      # Negligible — mention each event briefly
      for (evt in events) {
        parts <- c(parts, sprintf("%s — negligible trade-weighted impact (<0.01pp).",
                                  evt$event_name))
      }
    }
  }

  # Mention major irrelevant events briefly
  headline_ids <- c("ieepa_baseline_10pct", "ieepa_reciprocal_topups",
                    "s232_steel_50pct", "s232_alu_50pct", "eu_deal", "jpn_deal",
                    "chn_ieepa_topup_115pp", "s232_auto_25pct")
  for (evt in irrelevant_events) {
    if (evt$event_id %in% headline_ids) {
      parts <- c(parts, sprintf("Also on this date: %s — does not apply to this country.",
                                evt$event_name))
    }
  }

  # Handle case where events exist but all are negligible/irrelevant with zero total delta
  if (length(parts) == 0) {
    if (abs(total_delta) < 0.005) {
      event_names <- paste(events_dt$event_name, collapse = "; ")
      return(sprintf("Policy events on this date (%s) do not affect this country's rates.",
                     event_names))
    }
    return(sprintf("Rate change of %s on avg_rate from scope adjustments.",
                   fmt_delta(total_delta)))
  }

  # Add total avg_rate summary when component deltas differ from total
  # (due to tariff stacking, precedence rules, or offsetting movements)
  if (abs(total_delta) >= 0.005) {
    sum_comp_deltas <- sum(sapply(comp_groups, function(g) g$delta))
    if (abs(sum_comp_deltas - total_delta) > 0.05) {
      parts <- c(parts, sprintf("Net effect on avg_rate: %s (component interactions and stacking rules apply).",
                                 fmt_delta(total_delta)))
    }
  }

  return(paste(parts, collapse = " "))
}

# =============================================================================
# GENERATE NARRATIVES
# =============================================================================

cat("Generating narratives...\n")

countries <- sort(unique(timeline$iso3))
all_dates <- sort(unique(timeline$policy_date))

# Pre-compute: for each date, get matching events
events_by_date <- split(registry, registry$effective_date)

# Storage for all narratives
narrative_rows <- list()
row_idx <- 0

for (ctry in countries) {
  ctry_data <- timeline[iso3 == ctry]
  setorder(ctry_data, policy_date)
  dates <- ctry_data$policy_date

  for (d_idx in seq_along(dates)) {
    current_date <- dates[d_idx]
    current_row <- ctry_data[d_idx]
    is_first <- (d_idx == 1)

    # Compute deltas from previous date
    deltas <- setNames(rep(0, length(RATE_COMPONENTS)), names(RATE_COMPONENTS))
    total_delta <- 0

    if (!is_first) {
      prev_row <- ctry_data[d_idx - 1]
      for (comp_name in names(RATE_COMPONENTS)) {
        col <- RATE_COMPONENTS[comp_name]
        d <- as.numeric(current_row[[col]]) - as.numeric(prev_row[[col]])
        deltas[comp_name] <- d
      }
      total_delta <- as.numeric(current_row$avg_rate) - as.numeric(prev_row$avg_rate)
    }

    # Get events for this date
    date_str <- as.character(current_date)
    events_dt <- if (date_str %in% names(events_by_date)) {
      as.data.table(events_by_date[[date_str]])
    } else {
      data.table()
    }

    # Compose narrative
    narrative <- compose_narrative(events_dt, deltas, total_delta,
                                  ctry, current_date, is_first)

    row_idx <- row_idx + 1
    narrative_rows[[row_idx]] <- data.table(
      policy_date = current_date,
      iso3 = ctry,
      country = current_row$country,
      avg_rate = round(as.numeric(current_row$avg_rate), 2),
      delta_avg_rate = round(total_delta, 2),
      narrative = narrative
    )
  }
}

narratives <- rbindlist(narrative_rows)
cat(sprintf("  Generated %d narratives for %d countries\n",
            nrow(narratives), length(countries)))

# =============================================================================
# EXPORT
# =============================================================================

cat("Exporting annotated xlsx...\n")

# Build per-country sheets
sheets <- list()

for (ctry in countries) {
  ctry_data <- timeline[iso3 == ctry]
  ctry_notes <- narratives[iso3 == ctry, .(policy_date, narrative)]
  setnames(ctry_notes, "narrative", "narrative")

  # Merge narrative into timeline data
  merged <- merge(ctry_data, ctry_notes, by = "policy_date", all.x = TRUE)
  setorder(merged, policy_date)

  # Round numeric columns for readability
  rate_cols <- c("avg_rate", "avg_hts", "avg_ieepa", "avg_s232",
                 "avg_emergency", "avg_s301", "avg_surcharge")
  for (col in rate_cols) {
    if (col %in% names(merged)) {
      merged[[col]] <- round(as.numeric(merged[[col]]), 2)
    }
  }

  # Use country name as sheet name (truncate to 31 chars for Excel)
  sheet_name <- substr(unique(ctry_data$country)[1], 1, 31)
  if (is.na(sheet_name) || sheet_name == "") sheet_name <- ctry
  sheets[[sheet_name]] <- merged
}

# Add Notes sheet with all narratives in long format
notes <- narratives[, .(policy_date, iso3, country, avg_rate, delta_avg_rate, narrative)]
setorder(notes, iso3, policy_date)
sheets[["Notes"]] <- notes

# Write xlsx
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write_xlsx(sheets, output_file)
cat(sprintf("  Saved: %s (%d sheets)\n", output_file, length(sheets)))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("ANNOTATION COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("  Countries: %s\n", paste(countries, collapse = ", ")))
cat(sprintf("  Dates: %d (%s to %s)\n",
            length(all_dates), min(all_dates), max(all_dates)))
cat(sprintf("  Output: %s\n", output_file))

# Print sample narratives for verification
cat("\n  Sample narratives:\n")
for (ctry in head(countries, 3)) {
  cat(sprintf("\n  --- %s ---\n", ctry))
  ctry_notes <- narratives[iso3 == ctry]
  # Show first, a major change, and last
  show_idx <- unique(c(1, which.max(abs(ctry_notes$delta_avg_rate)), nrow(ctry_notes)))
  for (idx in show_idx) {
    row <- ctry_notes[idx]
    cat(sprintf("  [%s] avg_rate=%.2f (%s):\n    %s\n",
                row$policy_date, row$avg_rate, fmt_delta(row$delta_avg_rate),
                substr(row$narrative, 1, 200)))
  }
}

cat("\n=============================================================================\n")
