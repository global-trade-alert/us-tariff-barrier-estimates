# Section 122 Implementation

**Source:** SGEPT US Tariff Barrier Estimates - Section 122 Integration (February 2026)

## Overview

Section 122 of the Trade Act of 1974 is modelled as an **additive parallel tariff layer**. It operates through its own column (`s122_rate`) and follows the same four-formula aggregation logic as IEEPA, but with a distinct exception list and FTA preference structure.

Key parameters:

| Parameter | Value |
|-----------|-------|
| Baseline rate | 10% ad valorem |
| Duration | 150 days (24 Feb - 24 Jul 2026) |
| Scope | All imports, all origins |
| Legal authority | Section 122, Trade Act of 1974 |
| S232 interaction | Non-additive (S232 primacy via content/aircraft shares) |

Both `ieepa_rate` and `s122_rate` columns exist simultaneously in the model. The baseline scenario uses date-based rate loading to reflect the policy transition: `ieepa_baseline_rate = 10` from 5 April 2025, overridden to 0 from 20 February 2026 (line 136 of `rates.csv`); `s122_baseline_rate = 10` from 24 February 2026. For any policy date after the SCOTUS ruling, the baseline gives IEEPA = 0 and S122 = 10, consistent with S122 replacing the struck-down IEEPA tariffs.

For the policy narrative, see `s122-explainer.md` in the repository root.

## Data configuration

S122 parameters are distributed across four configuration files in `data/scenarios/`.

### rates.csv

```
s122_baseline_rate,10,baseline,4b.1,Section 122 baseline rate,2026-02-24
```

Loaded at line 901 of `apply_rates.R` via `get_rate("s122_baseline_rate")`. Returns 0 if the rate is not defined in the active scenario or if the policy date precedes the effective date, which disables the entire Section 4b pipeline.

The corresponding IEEPA zeroing entry ensures the two layers do not stack in the baseline scenario:

```
ieepa_baseline_rate,0,baseline,4.1,IEEPA struck down (court ruling 2026-02-20),2026-02-20
```

`get_rate()` resolves date conflicts by selecting the most recent entry for a given rate key on or before the policy date. For dates after 20 February 2026, this yields IEEPA = 0 and S122 = 10.

### exceptions.csv

S122 exceptions use `s122_baseline_rate` as the rate key. Four exception types are defined:

| Type | Count | Description |
|------|-------|-------------|
| `annex2` | 1,109 | Annex II product exceptions (minerals, energy, chemicals, pharmaceuticals) |
| `cafta_textiles` | 1,610 | CAFTA-DR textile/apparel products eligible for compliance weighting |
| `aircraft_generalisation` | 19 | Codes removed from aircraft list (titanium, computers) |
| `drone` | 11 | Drone codes (HS 8806.xx) added to aircraft list |

Format: `hs_8digit;s122_baseline_rate;ALL;{type};2026-02-24;;description`

Statutory exceptions reuse the IEEPA statutory list (`ieepa_statute_codes`) rather than defining a separate S122 list.

### shares.csv

```
cafta_textile_compliance,0.95,baseline,4b,CAFTA-DR textile compliance rate (default for all members)
```

Single uniform compliance rate applied to all CAFTA-DR members. Used in Step 3c for weighting.

### country_groups.csv

CAFTA-DR membership is defined under the `cafta` group:

| UN code | Country |
|---------|---------|
| 188 | Costa Rica |
| 214 | Dominican Republic |
| 222 | El Salvador |
| 320 | Guatemala |
| 340 | Honduras |
| 558 | Nicaragua |

Loaded at line 520 of `us_tariff_calculation.R` via `get_country_group("cafta")`.

## Processing pipeline

Section 4b of `apply_rates.R` (lines 873-1008) runs five sequential steps. The entire pipeline is gated on `s122_baseline_rate > 0`.

### Step 4b.1: Baseline assignment (lines 896-911)

```r
s122_baseline_rate <- tryCatch(get_rate("s122_baseline_rate"), error = function(e) 0)

if (s122_baseline_rate > 0) {
  us_imports[, s122_rate := s122_baseline_rate]
}
```

Applies the baseline rate to **all** rows unconditionally. Subsequent steps zero out specific products.

### Step 4b.2: Chapter 98 exclusion (lines 920-930)

```r
us_imports[as.numeric(hs_8digit) >= 98000000 &
           !hs_8digit %in% chapter_98_exceptions,
           s122_rate := 0]
```

Reuses the same Chapter 98 exception logic as IEEPA (Section 4.3). Products in HS Chapter 98 are zeroed unless they appear in the `chapter_98_exceptions` list.

### Step 4b.3: Annex II exceptions (lines 936-950)

```r
s122_annex2_codes <- get_exceptions("s122_baseline_rate", "annex2", policy_date)

us_imports[hs_8digit %in% s122_annex2_codes, `:=`(
  s122_rate = 0,
  s122_exception = 1
)]
```

Loads 1,109 S122-specific Annex II codes and zeroes their S122 rate. Sets the `s122_exception` marker for reporting.

### Step 4b.4: Statutory exceptions (lines 956-967)

```r
us_imports[hs_8digit %in% ieepa_statute_codes, `:=`(
  s122_rate = 0,
  s122_statute_exception = 1
)]
```

Reuses the IEEPA statutory exception list (loaded in Section 4.5) rather than maintaining a separate S122 list. Sets `s122_statute_exception` marker.

### Step 4b.5: Aircraft list construction (lines 974-1001)

```r
s122_drone_codes <- get_exceptions("s122_baseline_rate", "drone", policy_date)
s122_generalisation_codes <- get_exceptions("s122_baseline_rate", "aircraft_generalisation", policy_date)

s122_aircraft_codes <- setdiff(
  unique(c(aircraft_main, s122_drone_codes)),
  s122_generalisation_codes
)

all_aircraft_codes <- unique(c(aircraft_main, s122_aircraft_codes))
```

This step does **not** zero `s122_rate` for aircraft products. Instead, it builds the unified aircraft code list used by deal blocks (Section 7) to set `wto_aircraft = 1` and by the final formula to apply `aircraft_share` weighting.

## Aircraft list construction

The S122 aircraft list differs from the IEEPA list in two ways:

**Additions (11 codes):** Drone codes (HS 8806.xx) are included in the S122 aircraft exceptions but were not in the IEEPA list.

**Removals (19 codes):** Generalisation codes (titanium HS 8108, computers HS 8471) are removed from the aircraft list. These codes appeared on the IEEPA aircraft list because they could be used in aircraft, but they have broader non-aircraft applications. Under S122, these codes are instead exempted via Annex II, meaning they are exempt from the surcharge regardless of end use.

The composition formula is:

```
S122 aircraft = (IEEPA aircraft_main + drone codes) - generalisation codes
```

The unified list (`all_aircraft_codes`) is the union of both lists:

```
all_aircraft_codes = IEEPA aircraft_main ∪ S122 aircraft codes
```

This unified list is used in all deal blocks to mark `wto_aircraft = 1`.

## FTA weighting

Two FTA preference mechanisms apply to S122, both in the weighting section of `apply_rates.R`.

### Step 3b: USMCA weighting (lines 2405-2413)

```r
us_imports[, s122_rate_weighted := s122_rate]  # Default: no weighting

us_imports[un_code %in% c(canada_un_code, mexico_un_code),
           s122_rate_weighted := usmca_compliance * 0 + (1 - usmca_compliance) * s122_rate]
```

For Canada and Mexico, S122 is weighted identically to IEEPA using product-level USMCA compliance shares. USMCA-compliant trade faces 0%; non-compliant trade faces the full `s122_rate`.

The formula simplifies to:

```
s122_rate_weighted = (1 - usmca_compliance) × s122_rate
```

Product-level compliance data is documented in `usmca-compliance.md`.

### Step 3c: CAFTA-DR textile weighting (lines 2415-2436)

```r
cafta_textile_codes <- get_exceptions("s122_baseline_rate", "cafta_textiles", policy_date)
cafta_compliance <- get_share("cafta_textile_compliance")

us_imports[un_code %in% cafta_partners & hs_8digit %in% cafta_textile_codes,
           s122_rate_weighted := cafta_compliance * 0 + (1 - cafta_compliance) * s122_rate]
```

CAFTA-DR members receive compliance-weighted S122 rates on 1,610 textile/apparel products. The compliance rate defaults to 95% for all members, meaning only 5% of CAFTA textile trade faces the full surcharge.

The formula simplifies to:

```
s122_rate_weighted = (1 - 0.95) × s122_rate = 0.05 × s122_rate
```

This is a new structural feature of S122 with no IEEPA equivalent. Under IEEPA, bilateral deals with El Salvador and Guatemala achieved similar outcomes, but CAFTA-DR textile preferences are now treaty-based.

Note: CAFTA weighting applies **only** to `s122_rate_weighted`, not to `ieepa_rate_weighted`. IEEPA has no CAFTA textile provision.

## Final rate formulas

The four-formula approach (Step 8, lines 2540-2578) determines the final `rate` column. S122 enters each formula as `s122_rate_weighted`.

### Formula 0: No new tariffs

```
rate = hts_rate_weighted + s301_rate + country_surcharge_rate
```

**Condition:** `transport_s232 == 0 & materials_s232 == 0 & ieepa_rate == 0 & s122_rate == 0 & emergency_rate == 0`

S122 does not participate. This formula applies only when `s122_rate == 0` (excepted products).

### Formula 1: Transport S232

```
rate = hts_rate_weighted + s232_rate_weighted + emergency_rate_weighted + s301_rate + country_surcharge_rate
```

**Condition:** `transport_s232 == 1`

S122 is **excluded**. Transport S232 (autos, auto parts, MHDV, PV parts) replaces both IEEPA and S122. This is the S232 primacy rule for transport products.

### Formula 2: Materials S232

```
rate = hts + (1 - aircraft_share) × [content_share × s232 + (1 - content_share) × (ieepa + s122 + (1 - emergency_additive) × emergency)] + emergency_additive × emergency + s301 + surcharge
```

**Condition:** `materials_s232 == 1`

S122 sits inside the content-share bracket alongside IEEPA. For the portion of a product not covered by S232 content (the `1 - content_share` fraction), S122 and IEEPA stack additively. The `aircraft_share` factor further reduces the effective rate for WTO civil aircraft codes.

The `emergency_additive` flag determines whether emergency rates stack inside or outside the weighted bracket:
- `emergency_additive = 1` (China all S232, Canada/Mexico copper): emergency stacks additively outside
- `emergency_additive = 0` (all other): emergency is inside the weighted bracket with IEEPA and S122

### Formula 3: No S232, with reciprocal/emergency tariffs

```
rate = hts_rate_weighted + ieepa_rate_weighted + s122_rate_weighted + emergency_rate_weighted + s301_rate + country_surcharge_rate
```

**Condition:** `transport_s232 == 0 & materials_s232 == 0 & (ieepa_rate > 0 | s122_rate > 0 | emergency_rate > 0)`

Standard additive formula. S122 stacks on top of all other layers without any S232 interaction.

## Contribution decomposition

Step 10 (lines 2639-2689) decomposes the final rate into per-layer contributions. The `s122_contrib` column shows how much of the final rate is attributable to S122 after all weighting.

### Formula 0

```r
s122_contrib = 0  # (not set; remains at initialised value)
```

### Formula 1 (Transport S232)

```r
s122_contrib = 0  # S232 replaces S122
```

### Formula 2 (Materials S232)

```r
s122_contrib = (1 - aircraft_share) * (1 - content_share) * s122_rate_weighted
```

S122 contributes only to the non-S232, non-aircraft portion.

### Formula 3 (No S232)

```r
s122_contrib = s122_rate_weighted
```

Full weighted rate passes through as contribution.

The contributions satisfy: `sum(all_contrib) = rate` for every row.

## Interaction matrix

How S122 interacts with every other tariff layer in the model:

| Layer | Interaction with S122 |
|-------|----------------------|
| **HTS (MFN)** | Additive. S122 always stacks on top of HTS base rates. |
| **S232 transport** | S232 replaces S122 entirely (Formula 1). |
| **S232 materials** | S232 primacy via content share. S122 applies to `(1 - content_share)` portion only, further reduced by `(1 - aircraft_share)` (Formula 2). |
| **IEEPA** | Additive in all formulas where both are present. In the baseline scenario, the IEEPA rate is zeroed from 20 February 2026 (SCOTUS ruling), so S122 effectively replaces IEEPA. Alternative scenarios (e.g. `s122_ieepa_stack`) model S122 as a standalone layer with IEEPA explicitly zeroed. |
| **Emergency** | Additive in Formulas 0 and 3. In Formula 2, both sit inside the content-share bracket (unless `emergency_additive = 1`). |
| **S301 (China)** | Additive. S122 and S301 always stack without interaction. |
| **Country surcharge** | Additive. Independent layer. |
| **USMCA** | Compliance-weighted. USMCA-compliant trade from Canada/Mexico faces 0% S122. |
| **CAFTA-DR textiles** | Compliance-weighted (95%). CAFTA textile products face 5% of the S122 rate. |
| **Deal blocks** | Deal-specific IEEPA floor rates do not affect S122. S122 rate is independent of bilateral deal mechanics. Aircraft share (`wto_aircraft`) uses the unified aircraft list including S122 codes. |

## Configuration reference

All S122-related variables, their source files, and code locations.

### Columns initialised in `us_tariff_calculation.R`

| Variable | Type | Line | Description |
|----------|------|------|-------------|
| `s122_rate` | numeric | 694 | Raw S122 rate after exceptions |
| `s122_exception` | boolean | 710 | Annex II product exception flag |
| `s122_statute_exception` | boolean | 711 | Statutory exception flag |

### Columns created in `apply_rates.R`

| Variable | Type | Line | Description |
|----------|------|------|-------------|
| `s122_rate_weighted` | numeric | 2407 | S122 rate after USMCA/CAFTA weighting |
| `s122_contrib` | numeric | 2645 | S122 contribution to final rate |

### Processing steps in `apply_rates.R`

| Step | Lines | Operation |
|------|-------|-----------|
| 4b.0 | 884-893 | Load unified aircraft list |
| 4b.1 | 896-911 | Apply S122 baseline rate |
| 4b.2 | 917-930 | Chapter 98 exclusion |
| 4b.3 | 933-950 | Annex II exceptions |
| 4b.4 | 953-967 | Statutory exceptions (reuses IEEPA list) |
| 4b.5 | 970-1001 | Aircraft list construction |
| 3b | 2405-2413 | USMCA weighting |
| 3c | 2415-2436 | CAFTA-DR textile weighting |
| 8 | 2543-2578 | Four-formula final rate |
| 10 | 2639-2689 | Contribution decomposition |

### Summary statistics in `us_tariff_calculation.R`

| Line | Output |
|------|--------|
| 769-771 | S122 applied trade value and share |

### Data files

| File | Content |
|------|---------|
| `data/scenarios/rates.csv` (line 23) | `s122_baseline_rate` definition (10%, from 2026-02-24) |
| `data/scenarios/rates.csv` (line 136) | `ieepa_baseline_rate` override to 0 (from 2026-02-20) |
| `data/scenarios/exceptions.csv` | 2,749 S122 exception entries across 4 types |
| `data/scenarios/shares.csv` (line 64) | `cafta_textile_compliance` rate |
| `data/scenarios/country_groups.csv` (lines 39-44) | `cafta` group membership |
