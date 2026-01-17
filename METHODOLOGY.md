# Methodology for US Tariff Calculations

**Last Updated:** December 9, 2025
**Script:** `code/us_tariff_calculation.R`
**Author:** Johannes Fritz

---

## Executive Summary

This document provides a comprehensive explanation of how we calculate the final tariff rates applied to US imports under the Trump 2.0 tariff policy framework. The methodology uses a **layered approach** where different tariff instruments (MFN rates, Section 232 tariffs, reciprocal rates, etc.) are calculated separately and then combined according to specific precedence rules.

**Key Principle:** The calculation follows a logical sequence where:
1. Base rates are established (MFN tariffs)
2. Policy-specific rates are calculated (Section 232, reciprocal rates)
3. Country-specific exceptions are applied
4. Final rates are calculated using explicit precedence rules

This approach ensures transparency and makes all assumptions explicit.

**Quick Reference:** For a concise summary of key assumptions and parameter values, see [assumptions_summary.md](assumptions_summary.md).

---

## Table of Contents

1. [Data Sources](#i-data-sources)
2. [Analysis Scope](#ii-analysis-scope)
3. [Tariff Calculation Overview](#iii-tariff-calculation-overview)
4. [Step-by-Step Methodology](#iv-step-by-step-methodology)
5. [Key Assumptions](#v-key-assumptions)
6. [Precedence Rules](#vi-precedence-rules)
7. [Country-Specific Treatments](#vii-country-specific-treatments)
8. [Technical Notes](#viii-technical-notes)

---

## I. Data Sources

### Primary Trade Data

**US Import Data (2024):**
- **Source:** USITC DataWeb (https://dataweb.usitc.gov/)
- **Coverage:** Full-year 2024 US imports at HS 8-digit level
- **Records:** 235,792 country-product flows
- **Total Value:** $3,267.4 billion
- **Contents:**
  - Import values in USD
  - Country of origin (UN statistical codes)
  - HS 8-digit product codes and descriptions

**MFN Tariff Rate Database:**
- **File:** `data/scenarios/mfn_rates.csv`
- **Coverage:** 258,566 rate observations across four source options
- **Sources:**
  - `hts_schedule`: Statutory MFN rates from US HTS (11,262 product codes)
  - `mixed_schedule`: Mode-based empirical rates with MFN fallbacks (10,873 codes)
  - `observed_product`: Trade-weighted observed rates per product (10,873 codes)
  - `observed_bilateral`: Origin-specific observed rates (225,558 product × country)
- **Configuration:** Set `MFN_RATE_SOURCE` variable to select rate source (default: `hts_schedule`)
- **Schema:** Normalized table with composite key `(source_key, hs_8digit, un_code)`
- **See Section 2.1** for detailed rate source descriptions

### Policy Data Sources

All tariff policy data derived from official government sources compiled by the Global Trade Alert (GTA) US Tariff Measure Inventory:

1. **Section 232 Investigations** (National Security):
   - `sec232_auto.csv` - Automobiles and light trucks (231 codes)
   - `sec232_mhdv.csv` - Medium/heavy-duty vehicles (201 codes)
   - `sec232_steel.csv` - Steel and steel derivatives (1,495 codes)
   - `sec232_alu.csv` - Aluminum and aluminum derivatives (580 codes)
   - `sec232_copper.csv` - Copper and copper derivatives (219 codes)
   - `s232_lumber.csv` - Lumber products (20 codes)

2. **Reciprocal Tariff Framework** (IEEPA):
   - `reciprocal_country_specific_topup.csv` - Country-specific rates (204 countries)
   - `reciprocal_exceptions_251113.csv` - Annex 2 product exceptions (1,092 codes, November 13 version)
   - `reciprocal_exceptions_S232_lumber_removal.csv` - Lumber codes transitioned to S232
   - `reciprocal_exceptions_ieepa_statute.csv` - Statutory IEEPA exceptions (39 codes)

3. **Country-Specific Exceptions:**
   - `bra_exceptions.csv` - Brazil unconditional exceptions
   - `bra_passenger_vehicle_exceptions.csv` - Brazil conditional exceptions
   - `eu_exceptions.csv` - EU unconditional exceptions
   - `chli_exceptions.csv` - Switzerland/Liechtenstein unconditional exceptions (321 codes)
   - `pharma_exceptions.csv` - Non-patented pharmaceutical exceptions (519 codes) - Switzerland
   - `tariff_floor_rates.csv` - EU/Japan tariff floor schedules
   - `uk_auto_parts.csv` - UK Economic Prosperity Deal
   - `civil_aircraft_exceptions_wto.csv` - WTO civil aircraft agreement (554 codes) - shared by Brazil, UK, Japan, EU, Korea, Switzerland/Liechtenstein

4. **Emergency Actions:**
   - `can_northern_border_exception.csv` - Canada northern border security (413 codes)
   - Synthetic opioid tariff (China/Hong Kong) - applies to all products except reciprocal exceptions

5. **Section 301 (China):**
   - `sec301.csv` - China-specific tariffs (10,396 codes)

---

## II. Analysis Scope

### Included in Analysis

**Geographic Coverage:** 203 countries/territories (UN statistical codes)

**Product Coverage:** HS Chapters 1-97 (standard merchandise)
- **Total:** 234,846 country-product flows
- **Value:** $3,239.9 billion (99.2% of total US imports)

### Excluded from Analysis

**1. HS Chapter 99 (Special Classification Provisions):**
- **Excluded:** 228 flows ($24.5 billion, 0.7% of total)
- **Rationale:** Chapter 99 contains:
  - Temporary entries and re-imports
  - Special administrative codes
  - Non-merchandise transactions
  - Products not subject to standard tariff treatments
- **Industry Standard:** Chapter 99 exclusion is standard practice in trade policy analysis

**2. Sanctioned Countries:**
- **Excluded:** Russia (UN 643), Belarus (UN 112), Cuba (UN 192), North Korea (UN 408)
- **Flows:** 718 ($3.0 billion, 0.1% of total)
- **Rationale:** These countries face comprehensive sanctions separate from tariff policy. Including them would conflate sanctions analysis with tariff analysis. Their minimal/zero trade volumes reflect sanctions rather than tariff effects.

**3. HS Chapter 98 (Partial Exclusion from Reciprocal Rates):**
- **General Rule:** Chapter 98 products excluded from reciprocal tariff framework
- **Exceptions:** Four specific codes remain subject to reciprocal rates:
  - 98020080, 98020040, 98020050, 98020060
- **Rationale:** Policy interpretation based on the nature of Chapter 98 products (personal effects, temporary imports, special provisions)

---

## III. Tariff Calculation Overview

### Rate Column Architecture

The methodology calculates **six separate rate columns** that are then combined to produce the final applied rate:

| Column | Description |
|--------|-------------|
| **hts_rate** | Rate from US Harmonized Tariff Schedule (HTS) - includes both MFN rates (for non-preferential countries) and preferential rates (e.g., 0% for USMCA-compliant imports) |
| **s232_rate** | Rate from applicable Section 232 investigation |
| **ieepa_rate** | IEEPA baseline (10%) + country-specific reciprocal top-up |
| **emergency_rate** | Country-specific emergency security actions |
| **s301_rate** | Section 301 rate (China only) |
| **rate** | **Final applied rate** (calculated using weighted sum formula) |

### Calculation Sequence

The methodology follows this logical sequence:

```
SECTION 0: Set all parameters and assumptions
SECTION 1: Load import data and initialize all rate columns to 0
SECTION 2: Calculate HTS rates (US Harmonized Tariff Schedule - includes MFN rates for non-preferential countries and preferential rates for USMCA partners)
SECTION 3: Calculate Section 232 rates (based on product eligibility)
SECTION 4: Calculate IEEPA reciprocal rates (baseline + country top-ups + exceptions + statutory exclusions)
SECTION 5: Calculate emergency rates (China opioid, Canada border, Mexico)
SECTION 6: Calculate Section 301 rates (China only)
SECTION 7: Apply country-specific exceptions (modify s232_rate and/or ieepa_rate)
SECTION 8: Calculate final rate using precedence rules
SECTION 9: Export results
```

**Important:** Each section operates on separate rate columns. Country-specific exceptions in Section 7 may **modify** the rates calculated in earlier sections, but they do not create new rate columns.

---

## IV. Step-by-Step Methodology

### SECTION 0: Parameters and Assumptions

All assumptions are centralized in this section for transparency and adjustability. These parameters control the behavior of subsequent calculations.

#### Country Code Definitions

- **USMCA Partners:** Canada (UN 124), Mexico (UN 484)
- **EU Members:** 27 countries (Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden)
- **Major Trading Partners:** UK (826), Japan (392), China (156), Hong Kong (344), Brazil (76), India (699)
- **Sanctioned Countries:** Russia (643), Belarus (112), Cuba (192), North Korea (408)

#### Section 232 Effective Incidence Ratios

These parameters represent **effective tariff incidence**, not physical metal content. A 40% ratio means observed duties equal 40% of what full S232 application would yield. The gap reflects: (a) physical content below 100% for derivatives, (b) administrative exclusions (GAE, PSE), and (c) measurement factors. These components cannot be separated with available data.

**Calibration:** Ratios are calibrated from USITC 2025 trade data (January-September for steel/aluminum, August-September for copper). See `memo_metal_ratio_assumptions.md` for full methodology.

**Derivative Products:**

| Metal | Effective Incidence | Rationale |
|-------|---------------------|-----------|
| Steel derivatives | 40% | Trade-weighted average; captures structural relationship smoothing transitional volatility |
| Aluminum derivatives | 35% | Stable across periods; reflects physical content and exclusion patterns |
| Copper derivatives | N/A | No copper derivatives currently in S232 scope |

**Main Products:**

| Metal | Effective Incidence | Rationale |
|-------|---------------------|-----------|
| Steel main | 85% | Physical content = 100%; ratio <100% reflects exclusion utilisation (~15% of trade benefits from exclusions) |
| Aluminum main | 94% | Physical content = 100%; limited exclusion use in aluminum sector |
| Copper main | 70% | Provisional estimate based on 2 months of data; will be revised with Q4 2025 data |

**Lumber shares** (unchanged, no empirical calibration):
- **Lumber cabinet share:** 50% (kitchen cabinets/vanities: HS 94034090, 94036080, 94039100)

**Application Example:** A steel derivative product facing a 50% S232 rate would have an effective rate of: HTS rate (applies to 100%) + 20% S232 (40% incidence × 50% tariff = 20% effective S232 rate). A steel main product would have: HTS rate + 42.5% S232 (85% incidence × 50% tariff).

#### Section 232 Share Parameters (End-Use)

These parameters control how tariffs apply to **parts** that can be used in multiple applications:

- **MHDV share in parts:** 100%
  - **Meaning:** 100% of parts subject to MHDV tariffs are assumed to be destined for medium/heavy-duty vehicle assembly
  - **Application:** A 25% MHDV tariff applies fully to parts (100% × 25% = 25%)

- **PV share in parts:** 100%
  - **Meaning:** 100% of parts subject to passenger vehicle tariffs are assumed to be destined for passenger vehicle/light truck assembly

**Rationale:** Parts classified under MHDV or PV S232 are assumed to be destined for their respective end-use categories. The 100% share reflects that tariff scope already accounts for end-use classification.

**Example:** Brake pads subject to MHDV tariffs are assumed to be used in medium/heavy-duty vehicle assembly. A 25% MHDV tariff applies fully (100% × 25% = 25%).

#### Section 232 Tariff Rate Parameters

These parameters define the tariff rates applied under Section 232 investigations:

**S232 Auto Rates:**
- **s232_auto_rate:** 25% (automobiles and light trucks)

**S232 MHDV Rates:**
- **s232_mhdv_main_rate:** 25% (medium/heavy-duty vehicles - main products like trucks, tractors)
- **s232_mhdv_bus_rate:** 10% (buses)
- **s232_mhdv_parts_rate:** 25% (parts for MHDV assembly)

**USMCA Preferential Rate:**
- **s232_usmca_rate:** 0% (rate applied to USMCA-compliant products receiving preferential treatment)
  - **Application:** Used in weighted average formula for Canada/Mexico auto and MHDV products
  - **Formula:** `s232_weighted = compliance × 0% + (1-compliance) × standard_rate`

**Rationale:** Centralizing tariff rates as parameters allows for:
- Consistent application across all calculations
- Clear documentation of policy assumptions

#### USMCA Compliance Parameters (Product-Level)

**Key Concept:** USMCA compliance rates are now calibrated at the **product level** using empirical data from USITC DataWeb. This replaces the previous country-level aggregate assumptions (100% for Canada, 100% for Mexico).

**Data Source:**
- **File:** `data/compliance_shares.csv`
- **Source:** USITC DataWeb monthly import statistics
- **Calibration Period:** July-September 2025
- **Coverage:** 11,572 product-country combinations (HS 8-digit × origin)

**Calibration Rationale:**
Observed USMCA compliance rates increased sharply in July 2025, likely reflecting a behavioral response to evolving tariff policies. The July-September 2025 period represents steady-state compliance behavior under current conditions, with trade-weighted compliance averaging approximately 90% for both Canada and Mexico.

**Default for Unmatched Products:**
Products without observed compliance data receive a **90% default** compliance rate. This reflects the aggregate trade-weighted compliance observed in the calibration period.

**Formula Application:**
All USMCA benefits use the same product-level compliance rate:

**HTS Rate (Section 2):**
```
hts_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × standard_mfn
```

**IEEPA Rate Weighting (Section 8):**
```
ieepa_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × ieepa_rate
```

**S232 MHDV Parts (Section 8):**
```
s232_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × s232_rate
```

**Emergency Rate Weighting (Section 8):**
```
emergency_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × emergency_rate
```

**Note on Canadian Energy Products:**
The northern border energy exception list (`can_northern_border_energy`) determines which products are subject to the 10% emergency rate (vs 35% for other Canadian products). However, all products—including energy—use their empirical product-level compliance rates. The energy list only affects the base rate, not the compliance assumption.

**Example:**
- A Canadian machinery product (HS 84) with 12% observed compliance:
  - `hts_rate_weighted = 0.12 × 0% + 0.88 × 2.5% = 2.2%` (if MFN = 2.5%)
  - `emergency_rate_weighted = 0.12 × 0% + 0.88 × 35% = 30.8%`
- A Canadian energy product with 95% observed compliance:
  - `emergency_rate_weighted = 0.95 × 0% + 0.05 × 10% = 0.5%`

#### Conditional Exception Share Parameters

These parameters control weighted average calculations for country-specific **conditional exceptions** (exceptions that apply only to certain uses of a product):

**Civil Aircraft Shares (Country-Specific):**

These parameters determine the share of aircraft/parts used for civil (not military) purposes. They are applied in Section 8 rate calculation using a **nested formula**.

- **Brazil:** `bra_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from Brazil are assumed to be for civil (not military) use
  - **Application:** Section 8 formula applies `(1 - aircraft_share)` weight to entire tariff bracket

- **Japan:** `jpn_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from Japan are assumed to be for civil use under WTO civil aircraft agreement

- **EU:** `eu_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from EU are assumed to be for civil use under WTO civil aircraft agreement

- **UK:** `uk_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from UK are assumed to be for civil use under WTO civil aircraft agreement

- **Korea:** `kor_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from Korea are assumed to be for civil use under WTO civil aircraft agreement

- **Switzerland/Liechtenstein:** `che_civil_aircraft_share = 0.9` (90%)
  - **Meaning:** 90% of aircraft parts from Switzerland/Liechtenstein are assumed to be for civil use under WTO civil aircraft agreement

**KEY RULE:** Aircraft share applies ONLY to **materials S232** (steel/aluminum/copper/lumber), NOT to transport S232 (auto/MHDV/PV parts).

**Implementation (Section 8):**
- **Section 7:** Products are marked as `wto_aircraft = 1` (no rate modification)
- **Section 7b:** Products are marked with `emergency_additive = 1` if emergency stacks additively
- **Section 8:** Aircraft share is applied via nested formula (emergency may be additive):
  ```
  # When emergency_additive = 0:
  rate = hts + (1 - aircraft_share) * [content_share × s232 + (1-content_share) × (ieepa + emergency)] + s301
  
  # When emergency_additive = 1 (China all S232, Canada/Mexico copper):
  rate = hts + (1 - aircraft_share) * [content_share × s232 + (1-content_share) × ieepa] + emergency + s301
  ```

**Effect with 90% civil aircraft share (emergency_additive = 0):**
```
rate = hts + (1 - 0.9) * [materials_s232 + ieepa + emergency] + s301
     = hts + 0.1 × [materials_s232 + ieepa + emergency] + s301
(10% of materials S232, IEEPA, and emergency apply; 90% exempt as civil aircraft)
```

**Effect with 90% civil aircraft share (emergency_additive = 1):**
```
rate = hts + (1 - 0.9) * [materials_s232 + ieepa] + emergency + s301
     = hts + 0.1 × [materials_s232 + ieepa] + emergency + s301
(10% of materials S232 and IEEPA apply, emergency still fully applies)
```

**Example of Partial Exception (90% civil aircraft share):**
```
rate = 2% (hts) + (1 - 0.9) * [0.5 × 50% + 0.5 × 50%] + 0% (s301)
     = 2% + 0.1 × 50% = 7%
(10% of weighted tariff bracket applies)
```

**Other Conditional Exceptions:**

- **Brazil passenger vehicle share:** 90%
  - **Meaning:** 90% of auto parts from Brazil are assumed to be for passenger vehicles/light trucks
  - **Effect:** 90% of passenger vehicle parts receive exception (0% IEEPA rate); 10% face standard rates

**Rationale:** These shares reflect estimated civil/passenger vehicle use proportions rather than full exceptions.

#### Policy Configuration

- **Annex 2 version:** "nov13"
  - **Options:** "apr2" (April 2, 2025 initial), "semiconductors" (April 9 with semiconductor expansion), "aug1" (August 1, 2025 revised), "nov13" (November 13, 2025 latest)
  - **Current setting:** November 13, 2025 revision (most recent)
  - **Purpose:** Allows reconstruction of policy at different time points for historical analysis

- **Policy date:** November 21, 2025
  - **Purpose:** Documents the reference date for policy snapshot

#### Complete Parameter Reference Table

All parameters centralized in Section 0 of the R code for transparency:

| Category | Parameter | Value | Code Line | Sections Used | Purpose |
|----------|-----------|-------|-----------|---------------|---------|
| **S232 Incidence (Derivatives)** | `steel_derivative_share` | 0.40 | 0.2 | 8.6 | Steel derivatives effective incidence |
| | `alu_derivative_share` | 0.35 | 0.2 | 8.6 | Aluminum derivatives effective incidence |
| **S232 Incidence (Main)** | `steel_main_share` | 0.85 | 0.2 | 8.6 | Steel main products (exclusion-adjusted) |
| | `alu_main_share` | 0.94 | 0.2 | 8.6 | Aluminum main products (exclusion-adjusted) |
| | `copper_main_share` | 0.70 | 0.2 | 8.6 | Copper main products (provisional) |
| **S232 Share (Lumber)** | `lumber_cabinet_share` | 0.50 | 0.2 | 8.6 | Timber content in cabinets/vanities |
| **S232 Share (End-Use)** | `mhdv_share_in_parts` | 1.0 | 119 | 8.6 | Share of parts for MHDV assembly |
| | `pv_share_in_parts` | 1.0 | 120 | 8.6 | Share of parts for PV assembly |
| **USMCA Compliance** | `compliance_shares.csv` | Product-level | 2.4 | 8.1, 8.2, 8.3, 8.4 | Product-level compliance rates (USITC Jul-Sep 2025) |
| | Default for unmatched | 0.90 | 2.4 | 8.1, 8.2, 8.3, 8.4 | 90% default for products without observations |
| | `usmca_rate` | 0.0 | 146 | 8.1 | USMCA preferential HTS rate |
| **Civil Aircraft Shares** | `bra_civil_aircraft_share` | 0.9 | 170 | 8.7, 8.8 | Brazil civil aircraft share |
| | `jpn_civil_aircraft_share` | 0.9 | 171 | 8.7, 8.8 | Japan civil aircraft share |
| | `eu_civil_aircraft_share` | 0.9 | 172 | 8.7, 8.8 | EU civil aircraft share |
| | `uk_civil_aircraft_share` | 0.9 | 173 | 8.7, 8.8 | UK civil aircraft share |
| | `kor_civil_aircraft_share` | 0.9 | 174 | 8.7, 8.8 | Korea civil aircraft share |
| | `che_civil_aircraft_share` | 0.9 | 175 | 8.7, 8.8 | Switzerland/Liechtenstein civil aircraft share |
| **Other Conditional** | `bra_passenger_vehicle_share` | 0.9 | 172 | 7.2 | Brazil PV parts share |
| | `uk_car_parts_uk_built_share` | 0.75 | 173 | 7.3 | UK parts for UK-built cars |
| | `uk_steel_alu_quota_share` | 1.0 | 174 | 7.3 | UK steel/alu within quota |
| **S232 Auto Rates** | `s232_auto_rate` | 25% | 431 | 3.1, 8.4 | Auto and light truck rate |
| **S232 MHDV Rates** | `s232_mhdv_main_rate` | 25% | 455 | 3.2, 8.4 | MHDV main products |
| | `s232_mhdv_bus_rate` | 10% | 456 | 3.2 | Buses |
| | `s232_mhdv_parts_rate` | 25% | 457 | 3.2, 8.4 | MHDV parts |
| **S232 PV Parts** | `s232_pv_parts_rate` | 25% | 502 | 3.3 | Passenger vehicle parts |
| **S232 Materials** | `s232_steel_rate` | 50% | 538 | 3.4 | Steel and derivatives |
| | `s232_alu_rate` | 50% | 577 | 3.5 | Aluminum and derivatives |
| | `s232_copper_rate` | 50% | 609 | 3.6 | Copper and derivatives |
| **IEEPA** | `ieepa_baseline_rate` | 10% | 712 | 4.1, 8.3 | Universal IEEPA baseline |
| **India/Russia** | `india_russia_topup` | 25% | 1084 | 7.1 | Additional top-up |
| **Brazil** | `bra_topup` | 40% | 1109 | 7.2 | Brazil-specific top-up |
| **UK Rates** | `uk_ceiling_rate` | 10% | 1239 | 7.3 | Auto/PV parts ceiling |
| | `uk_steel_alu_cap` | 25% | 1241 | 7.3 | Steel/alu cap (in quota) |
| | `uk_lumber_derivative_rate` | 10% | 1242 | 7.3 | Cabinets/upholstered |
| **Japan** | `jpn_ieepa_floor_rate` | 15% | 1312 | 7.4 | IEEPA floor rate (reciprocal tariffs) |
| | `jpn_s232_floor_rate` | 15% | 1312 | 7.4 | S232 auto/MHDV floor rate |
| | `jpn_lumber_derivative_rate` | 15% | 1243 | 7.4 | Cabinets/upholstered |
| **EU** | `eu_ieepa_floor_rate` | 15% | 1386 | 7.5 | IEEPA floor rate (reciprocal tariffs) |
| | `eu_s232_floor_rate` | 15% | 1386 | 7.5 | S232 auto/MHDV floor rate |
| | `eu_lumber_derivative_rate` | 15% | 1244 | 7.5 | Cabinets/upholstered |
| **Emergency Rates** | `china_emergency_rate` | 10% | 907 | 5.1 | China/HK opioid (time-varying) |
| | `can_emergency_energy_rate` | 10% | 942 | 5.2, 8.2 | Canada energy products |
| | `can_emergency_general_rate` | 35% | 943 | 5.2, 8.2 | Canada non-energy |
| | `mex_emergency_rate` | 25% | 997 | 5.3, 8.2 | Mexico emergency |
| **Policy** | `annex2_version` | "nov13" | 187 | 4.4 | Annex 2 version selection |
| | `policy_date` | "2025-11-21" | 190 | 5.1, 7.2 | Policy snapshot date |

**Notes:**
- All share parameters (0.0 to 1.0) represent percentages as decimals
- All rate parameters are percentages (10 = 10%)
- Code Line references are to us_tariff_calculation.R
- Time-varying: China emergency rate = 10% (Feb 4-Mar 3, Nov 10+), 20% (Mar 4-Nov 9)

---

### SECTION 1: Load and Scope Data

#### 1.1 Load US Import Data

**File:** `data/usitc_us_imports_2024.csv`
- **Rows loaded:** 235,792 country-product flows
- **Total value:** $3,267.4 billion
- **Processing:** Import values converted to billions for easier handling

#### 1.2 Apply Analysis Scope

**Exclusions Applied:**
1. **Chapter 99 products**
   - Excluded: 228 rows ($24.5 billion)
   - Remaining: Products in HS Chapters 1-97

2. **Sanctioned countries**
   - Excluded: 718 rows ($3.0 billion)
   - Countries: Russia, Belarus, Cuba, North Korea

**Final Analysis Scope:**
- **Rows:** 234,846 (99.1% of original)
- **Value:** $3,239.9 billion (99.2% of original)
- **Coverage:** 10,905 unique HS 8-digit codes across 203 countries

#### 1.3 Initialize Rate Columns

All rate and marker columns are initialized to ensure clean starting state:

**Rate columns** (all initialized to 0):
- hts_rate, s232_rate, ieepa_rate, emergency_rate, s301_rate, rate
- Composite rates: rate_s25, rate_s50, rate_s75, rate_mhdv_25, rate_mhdv_50, rate_mhdv_75, rate_mhdv_100

**Boolean marker columns** (all initialized to 0):
- 20 marker columns tracking which policies apply to each flow
- Examples: s232_auto, s232_steel, rr_exception, chn_opioid, sec301_tariff, can_northern_border, mex_emergency

---

### SECTION 2: HTS Rates (Populate hts_rate column)

**What is the HTS Rate?**
The US Harmonized Tariff Schedule (HTS) contains tariff rates that vary by product and trading partner relationship:
- **MFN (Most Favored Nation) rates:** Standard rates that apply to countries without preferential trade agreements
- **Preferential rates:** Reduced or zero rates for countries with trade agreements (e.g., USMCA partners receive 0% on qualifying imports)

The `hts_rate` column captures the applicable tariff rate from the HTS schedule, which may be either an MFN rate or a preferential rate depending on the country and compliance status.

#### 2.1 Load MFN Tariff Rates

**Configuration:** `MFN_RATE_SOURCE` (default: `hts_schedule`)

The MFN rate source can be configured at runtime to select from four different base tariff rate estimates. All rate data is stored in a normalized database table (`data/scenarios/mfn_rates.csv`) with composite key `(source_key, hs_8digit, un_code)`.

**Available Rate Sources:**

| Source Key | Level | Rows | Description |
|------------|-------|------|-------------|
| `hts_schedule` | Product | 11,262 | Statutory MFN rates from US Harmonized Tariff Schedule (2025) |
| `mixed_schedule` | Product | 10,873 | Mode-based empirical rates with MFN fallbacks (2024 observed) |
| `observed_product` | Product | 10,873 | Trade-weighted observed rates per product (2024) |
| `observed_bilateral` | Product × Origin | 225,558 | Origin-specific observed rates (2024) |

**Source Selection:**

```r
# Set via R variable before running script
MFN_RATE_SOURCE <- "mixed_schedule"

# Or via environment variable
Sys.setenv(MFN_RATE_SOURCE = "observed_bilateral")
```

**Rate Source Descriptions:**

1. **HTS Schedule (default):** Applies the statutory Most Favoured Nation rate from the US Harmonized Tariff Schedule. This estimate reflects the legal rate applicable to WTO members without preferential arrangements. It does not account for preferential programmes, trade defence measures, or non-ad valorem rate conversions.

2. **Mixed Schedule:** Applies a selection algorithm: use the empirical mode (rounded to 0.5 pp) where it falls within 5 pp of the MFN schedule; fall back to the MFN rate for high-variance products; fall back to the median for S232 products. This estimate approximates the statutory rate schedule inclusive of non-ad valorem equivalents.

3. **Observed Product:** Provides a single trade-weighted effective rate per product, reflecting actual 2024 collection patterns. This estimate is suitable for aggregate calibration but obscures origin-specific tariff heterogeneity.

4. **Observed Bilateral:** Captures the full origin-specific variation in effective rates, including preferential programmes (USMCA, GSP, FTAs), trade defence measures (AD/CVD), and programme-specific tariffs (S301 China). For missing product-country combinations, falls back to `observed_product` rates.

**Methodology Reference:** See `../251212 US effective tariff rates/results/effective_rate_2024/memo_effective_rate_2024.md` for detailed methodology on empirical rate estimation.

**Database Schema:**

```
Table: data/scenarios/mfn_rates.csv
Columns: source_key, hs_8digit, un_code (nullable), rate
- un_code is NULL for product-level sources
- un_code is populated for bilateral source
```

**Observational MFN Rate Source Options:**

When using observational MFN rate sources (`observed_bilateral`, `observed_product`, or `baseline_boe`), the rates already reflect certain policy measures that were in effect during 2024. To avoid double-counting these measures, two additional configuration options are available:

| Option | Environment Variable | Default | Description |
|--------|---------------------|---------|-------------|
| Disable HTS USMCA weighting | `DISABLE_HTS_USMCA_WEIGHTING` | `FALSE` | When TRUE, skip USMCA compliance weighting for HTS rates only (Section 8.1) |
| Exclude Section 301 tariffs | `EXCLUDE_S301_TARIFFS` | `FALSE` | When TRUE, skip Section 301 tariff loading entirely (Section 6) |

**Why These Options Exist:**

1. **USMCA Double-counting:** Observational 2024 rates for Canada and Mexico already reflect USMCA preferential treatment. Applying USMCA compliance weighting again would undercount the effective rate.

2. **Section 301 Double-counting:** Observational 2024 rates for China already include Section 301 tariffs (7.5–25% additional duties on certain products). Adding Section 301 rates again would overcount the effective rate.

**Usage:**

```r
# Observational source with both adjustments disabled
Sys.setenv(
  MFN_RATE_SOURCE = "baseline_boe",
  DISABLE_HTS_USMCA_WEIGHTING = "TRUE",
  EXCLUDE_S301_TARIFFS = "TRUE"
)

# Or via command line
# DISABLE_HTS_USMCA_WEIGHTING=TRUE EXCLUDE_S301_TARIFFS=TRUE Rscript code/us_tariff_calculation_db.R
```

**Output Files:**

When these options are enabled, the output filename includes indicators:

| Options | Output File Example |
|---------|---------------------|
| Neither | `processed_us_imports_with_rates_boe24_baseline.csv` |
| USMCA disabled | `processed_us_imports_with_rates_boe24_nousmca_baseline.csv` |
| S301 excluded | `processed_us_imports_with_rates_boe24_nos301_baseline.csv` |
| Both | `processed_us_imports_with_rates_boe24_nousmca_nos301_baseline.csv` |

**Important Notes:**

- These options are **independent** — user must explicitly enable each one
- Other USMCA compliance adjustments (emergency rates, IEEPA rates, S232 rates in Steps 2-4) remain unchanged
- With `EXCLUDE_S301_TARIFFS=TRUE`, the `s301_rate` column remains 0 for all rows including China

#### 2.2 Handle Missing MFN Codes

**Issue:** 173 HS 8-digit codes in import data are not in the MFN schedule
- **Value affected:** $110.86 billion (3.42% of total imports)

**ASSUMPTION #1: Missing HTS Rates**
- **Treatment:** Products without HTS rates are assigned **0% rate**
- **Rationale:** Missing codes typically represent:
  - New product codes added after HTS schedule published
  - Administrative or special classification codes
  - Products with suspended duties
- **Documentation:** All missing codes logged to `results/mfn_missing_codes_log.csv`
- **Transparency:** The boolean marker `missing_mfn` is set to 1 for these products

**Verification:** After merge, 0 products have NA hts_rate values (all missing codes assigned 0%)

#### 2.3 Store Standard HTS Rates for USMCA Partners

**Note:** USMCA weighting is applied in **Section 8** for consistency with other weighting operations (emergency rates, IEEPA rates, S232 MHDV rates). Section 2 only stores the standard HTS rates.

**USMCA Preferential HTS Rates - Overview:**

USMCA-compliant imports from Canada and Mexico receive **preferential rates** (0%) in the US Harmonized Tariff Schedule. This is **separate from** the standard MFN rates that apply to countries without preferential trade agreements.

**Storage (Section 2):**
- Standard HTS rates are stored in the `hts_rate_standard` column for USMCA partners
- The `hts_rate` column contains the standard MFN rate at this stage
- **Countries:** Canada (UN 124), Mexico (UN 484)
- **Value:** $913.0 billion

**Weighting Application (Section 8):**

The USMCA compliance weighting is applied in Section 8 (see Section 8.1 for details) using product-level compliance rates.

**Formula (Applied in Section 8):**
```
hts_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × hts_rate
```

Where `usmca_compliance` is the product-level compliance rate from `compliance_shares.csv` (default 90% for unmatched products).

**Example (with 90% product-level compliance):**
```
hts_rate_weighted = 0.90 × 0% + 0.10 × hts_rate = 10% of standard rate
```

---

## Understanding Rate Column Architecture

Before diving into the rate calculations (Sections 3-8), it's important to understand the **two-column architecture** used throughout this methodology:

### Base Rate Columns vs. Weighted Rate Columns

**Base Rate Columns (Set in Sections 2-6):**
- `hts_rate` - Harmonized Tariff Schedule rate (Section 2)
- `s232_rate` - Section 232 national security tariff (Section 3)
- `ieepa_rate` - IEEPA reciprocal tariff (Section 4)
- `emergency_rate` - Country-specific emergency actions (Section 5)
- `s301_rate` - Section 301 China tariffs (Section 6)

These columns store the **standard rates** that would apply if there were no special treatments or exemptions.

**Weighted Rate Columns (Created in Section 8):**
- `hts_rate_weighted` - HTS rate after USMCA weighting
- `s232_rate_weighted` - S232 rate after USMCA auto/MHDV weighting
- `ieepa_rate_weighted` - IEEPA rate after USMCA weighting
- `emergency_rate_weighted` - Emergency rate after USMCA weighting

These columns store the **effective rates** after applying country-specific compliance weighting (primarily for USMCA partners).

### Why Separate Base and Weighted Columns?

**Architectural Clarity:**
1. **Sections 2-6** focus on **policy identification** - which tariffs apply to which products
2. **Section 7** handles **country-specific modifications** - adjusting rates for specific countries
3. **Section 8** handles **compliance weighting** - applying share-based reductions (USMCA, aircraft, derivatives)

This separation makes the code easier to understand, maintain, and audit.

**Example - Canadian Auto Product:**

```
Section 3 (Policy Identification):
  s232_auto = 1              ← Product identified as subject to auto S232
  s232_rate = 25%            ← Standard rate stored

Section 8 (USMCA Weighting):
  s232_rate_weighted = usmca_compliance × 0% + (1-usmca_compliance) × 25%
                     = 0.90 × 0% + 0.10 × 25%
                     = 2.5%              ← Effective rate after weighting (if 90% compliance)

Final Formula (Section 8):
  rate = hts_rate_weighted + s232_rate_weighted + s301_rate
       = 0% (USMCA) + 0% (USMCA auto) + 0% (no S301)
       = 0%
```

### When Does Weighting Happen?

**NEVER in Sections 2-7:** These sections calculate and store standard rates without weighting.

**ALWAYS in Section 8:** All weighting operations are centralized here:
- USMCA compliance weighting (Steps 1-4)
- Content share weighting for derivatives (Step 6)
- Aircraft share weighting (Step 7)

**Rationale:** Centralizing all weighting logic in one place prevents inconsistencies and makes the methodology transparent.

---

### SECTION 3: Section 232 Rates (Populate s232_rate column)

Section 232 of the Trade Expansion Act of 1962 authorizes tariffs on imports that threaten national security. Multiple investigations cover different products.

#### Key Principles

**Precedence Hierarchy:**
1. **AUTO S232** (highest precedence) - products subject to auto tariffs are exempt from other S232 tariffs
2. **MHDV S232** (second precedence) - products subject to MHDV tariffs are exempt from lower-priority S232 tariffs
3. **PV Parts S232** (third precedence) - passenger vehicle parts exempt from metal/lumber S232
4. **Steel, Aluminum, Copper, Lumber S232** (lower precedence) - only if not subject to higher-precedence S232

**Main Products vs. Derivatives:**
- **Main products:** Effective incidence ratio reflects exclusion utilisation (not physical content)
  - Example: Steel ingots face 85% × 50% = 42.5% effective tariff (15% benefits from exclusions)
- **Derivatives:** Effective incidence ratio reflects both physical content and exclusions
  - Example: Steel-containing machinery faces 40% × 50% = 20% effective tariff

**Note on Effective Incidence Ratios:**
These ratios are calibrated from 2025 USITC trade data and represent effective tariff incidence, not physical metal content. A 40% ratio means observed duties equal 40% of what full S232 application would yield. The gap reflects: (a) physical content below 100% for derivatives, (b) administrative exclusions (GAE, PSE), and (c) measurement factors. See `memo_metal_ratio_assumptions.md` for full calibration methodology.

**CRITICAL: Two-Stage Share Application**

Understanding how effective incidence ratios are stored vs. applied:

**Stage 1 - Product Identification (Section 3):**
- Section 3 stores the **FULL S232 rate** in the `s232_rate` column for ALL products (both main and derivatives)
- For steel derivatives: `s232_rate = 50%` (full rate stored)
- The marker `s232_steel_derivative = 1` identifies it as a derivative
- **Rationale:** Separates product identification from incidence calculation

**Stage 2 - Incidence Application (Section 8):**
- Section 8 determines `content_share` (effective incidence ratio) based on product markers:
  - Steel main products: `content_share = 0.85` (85% effective incidence)
  - Steel derivatives: `content_share = 0.40` (40% effective incidence)
  - Aluminum main products: `content_share = 0.94`
  - Aluminum derivatives: `content_share = 0.35`
  - Copper main products: `content_share = 0.70` (provisional)
- Section 8 formulas then apply: `content_share × s232_rate`
- For steel derivative: `0.40 × 50% = 20%` (effective rate)
- For steel main: `0.85 × 50% = 42.5%` (effective rate)

**Example - Steel Derivative Product:**
```
Section 3 (Identification):
  s232_steel_derivative = 1
  s232_rate = 50%          ← Full rate stored

Section 8 (Application):
  content_share = 0.40     ← Effective incidence from calibration
  Effective rate = 0.40 × 50% = 20%   ← Applied in formula
```

**Why This Approach?**
1. **Architectural clarity:** Product identification (Section 3) separate from incidence logic (Section 8)
2. **Centralized weighting:** ALL weighting operations happen in Section 8 (USMCA, incidence ratios, aircraft share)
3. **Flexibility:** Easy to adjust calibrated parameters without modifying Section 3 product lists

**Country Exceptions:** Certain countries receive special treatment (UK, EU, Japan), but these are applied in **Section 7**, not here. In this section, all countries receive the same standard rates.

#### 3.1 Auto S232 (Highest Precedence)

**File:** `data/sec232_auto.csv` (231 HS 8-digit codes)

**Standard Rate:** 25%
**Special Rates (applied in Section 7):**
- Japan/EU: 15% floor (maximum of 15% or MFN rate)
- UK: Various rates (7.5% for autos, 10% for parts)

**Application:**
- **Value:** $611.2 billion
- **Flows:** 8,819 country-product flows
- **Formula:** `s232_rate = 25%` for all countries
- **Marker:** `s232_auto = 1`

**Products Covered:**
- Passenger automobiles
- Light trucks (under 10,000 lbs GVWR)
- Parts and components for the above

#### 3.2 MHDV S232 (Second Precedence)

**File:** `data/sec232_mhdv.csv` (201 HS 8-digit codes)

**Product Categories:**
1. **MHDV (Medium/Heavy-Duty Vehicles):** Trucks over 10,000 lbs GVWR
2. **Buses:** All bus categories
3. **Parts of MHDV:** Parts and components

**Standard Rates (Applied to ALL countries in Section 3.2):**
- **MHDV main:** 25%
- **Buses:** 10%
- **Parts:** 25%

**Country-Specific Treatments:**

Section 3.2 applies standard rates uniformly to all countries. Country-specific adjustments are then made:

- **USMCA (Canada & Mexico):** Weighting applied in **Section 8.6**
  - MHDV main: Weighted average between 0% (USMCA rate) and 25% (standard) using product-level compliance
  - Buses: 10% (no USMCA weighting - buses excluded from USMCA auto provisions)
  - Parts: Weighted average with nested formula (MHDV share × product-level USMCA compliance)
  - **Product-level compliance:** Rates from `compliance_shares.csv` (90% default for unmatched)

- **UK:** Modified in **Section 7.3**
  - All MHDV products: Set to 10% ceiling rate (Economic Prosperity Deal)

- **Japan:** Modified in **Section 7.4**
  - MHDV products: Subject to 15% tariff floor via `ieepa_rate` adjustment

- **EU:** Modified in **Section 7.5**
  - MHDV products: Subject to 15% tariff floor via `ieepa_rate` adjustment

**Architectural Note:** Section 3 identifies S232-eligible products with standard rates. **Section 7** handles country-specific rate modifications, while **Section 8** handles USMCA compliance weighting.

**Application:**
- **Value:** $25.5 billion
- **Flows:** 670

#### 3.3 Passenger Vehicle Parts S232 (Third Precedence)

**File:** `data/sec232_passenger_vehicle_parts.csv` (currently empty)

**Planned Rate:** 25% (when HS codes are populated)
**End-Use Share:** 100% (pv_share_in_parts)

**Exclusions:**
- Products in HS Chapters 72, 73, 76 (steel, iron, aluminum)
- Products covered by higher-precedence S232 (auto, MHDV)

**Current Status:** Empty HS code list (structure ready for future implementation)

**When Implemented:**
```
s232_rate = pv_share_in_parts × 25% = 1.0 × 25% = 25%
```

#### 3.4 Steel S232

**File:** `data/sec232_steel.csv` (1,495 HS 8-digit codes)

**Categories:**
1. **Main steel products (322 codes):** Steel ingots, sheets, bars, etc.
   - **Statutory rate:** 50%
   - **Effective rate:** 50% × 85% = 42.5% (calibrated incidence ratio reflects exclusion use)

2. **Steel derivatives (1,173 codes):** Products containing steel (machinery, tools, etc.)
   - **Statutory rate:** 50%
   - **Effective rate:** 50% × 40% = 20% (calibrated incidence ratio)

**IMPORTANT - Effective Incidence Application:**

In this section, the **FULL S232 rate** is stored:
```
s232_rate = 50%  (full statutory rate for all steel products)
```

The **effective incidence ratio** is applied in Section 8:
```
content_share = 0.85 (steel main) or 0.40 (steel derivative)
Effective rate = content_share × s232_rate
```

**Rationale:** The incidence ratios are calibrated from observed 2025 trade data. For derivatives, the ratio reflects both physical metal content and exclusion patterns. For main products, the ratio reflects exclusion utilisation (physical content = 100%).

**UK Special Treatment (applied in Section 7):**
- Steel S232 capped at 25% (down from 50%)

**Application:**
- **Value:** $372.0 billion
- **Main products:** 3,749 flows
- **Derivatives:** 17,734 flows

#### 3.5-3.7 Aluminum, Copper, Lumber S232

**Similar structure to Steel:**

**Aluminum S232:**
- **File:** `data/sec232_alu.csv` (580 codes)
- **Statutory rate:** 50%
- **Effective rates:** Main = 50% × 94% = 47%; Derivatives = 50% × 35% = 17.5%
- **Value:** $272.7 billion (1,012 main + 8,838 derivatives)
- **UK cap:** 25%

**Copper S232:**
- **File:** `data/sec232_copper.csv` (219 codes)
- **Statutory rate:** 50%
- **Effective rate:** 50% × 70% = 35% (main products only; provisional estimate)
- **Value:** $15.5 billion (1,095 main + 757 derivatives)
- **Note:** No copper derivatives currently in S232 scope

**Lumber S232:**
- **File:** `data/s232_lumber.csv` (20 codes)
- **Rates:** Variable by product (10-25%)
- **Value:** $24.7 billion (670 flows)
- **UK cap:** 10%
- **Note:** These products were removed from reciprocal exceptions on October 14, 2025
- **Special Treatment - Kitchen Cabinets/Vanities:**
  - **HS Codes:** 94034090, 94036080, 94039100
  - **Mixed Use:** These codes contain both completed kitchen cabinets/vanities (subject to S232) and other furniture (subject to MFN only)
  - **Content Share:** 50% lumber content assumed (lumber_cabinet_share parameter)
  - **Marker:** `s232_lumber_derivative = 1`
  - **Rate Application:** Full S232 lumber rate stored in Section 3, then weighted by 50% share in Section 8

- **Special Treatment - Upholstered Furniture:**
  - **HS Codes:** 94016140, 94016160
  - **Product:** Upholstered seats with wooden frames
  - **Treatment:** Classified as lumber derivatives due to wooden frame component
  - **Content Share:** 50% lumber content assumed (same as other lumber derivatives)
  - **Marker:** `s232_upholstered_furniture = 1`
  - **UK-Specific:** Receives 10% lumber derivative rate cap (Section 7.3)
  - **Rationale:** Wooden frames represent substantial portion of product value
  - **Code Reference:** us_tariff_calculation.R lines 652-659

**TOTAL S232 Coverage:** $1,096.1 billion (33.8% of imports)

---

### SECTION 4: IEEPA Rates (Populate ieepa_rate column)

IEEPA (International Emergency Economic Powers Act) provides the legal authority for reciprocal tariffs. This section calculates the IEEPA component before country-specific modifications in Section 7.

#### 4.1 IEEPA Baseline (10% Universal)

**Application:** Add 10% to all products from all countries
- **Value affected:** $3,239.9 billion (100% of imports)
- **Formula:** `ieepa_rate = 10%`

**Rationale:** Establishes minimum reciprocal tariff level above existing MFN rates. Represents baseline reciprocal treatment regardless of country-specific circumstances.

#### 4.2 Country-Specific Reciprocal Top-Ups

**File:** `data/reciprocal_country_specific_topup.csv`
- **Countries:** 204
- **Column used:** `country_rate_aug1` (August 1, 2025 iteration)
- **Implementation note:** Country rates are converted to numeric with warning suppression to handle any formatting issues in source data

**Formula:**
```
ieepa_rate = 10% + country_specific_topup
```

**Rate Distribution (August 1st):**
- **0% top-up:** 114 countries (total IEEPA = 10%)
- **5% top-up:** 64 countries (total IEEPA = 15%)
- **8% top-up:** 1 country (total IEEPA = 18%)
- **9% top-up:** 6 countries (total IEEPA = 19%)
- **10% top-up:** 4 countries (total IEEPA = 20%)
- **Other rates:** Various (ranging up to 40%)

**Application:**
- **Flows affected:** 233,112 (99.3% of dataset)
- **Value affected:** $3,235.9 billion (99.9% of imports)

**ASSUMPTION #2: Country Rate Interpretation**
- **Treatment:** Country rates are **added to** the 10% baseline (not replacing it)
- **Example:** Country with 15% reciprocal rate receives 10% + 15% = 25% total IEEPA rate
- **Rationale:** Policy structure indicates baseline plus country-specific adjustments

#### 4.3 Product Scope - Chapter 98 Exclusion

**General Rule:** HS Chapter 98 products are excluded from reciprocal tariffs
- **Excluded:** 988 flows
- **Effect:** `ieepa_rate = 0` for these products

**Exceptions (Remain Subject to Reciprocal Rates):**
- HS codes: 98020080, 98020040, 98020050, 98020060
- **Rationale:** Specific policy designation for these particular codes

**ASSUMPTION #3: Chapter 98 Treatment**
- **Interpretation:** Chapter 98 exclusion reflects special classification nature
- **Basis:** Chapter 98 contains special provisions, temporary entries, personal effects
- **Documentation:** Four exception codes explicitly maintained

#### 4.4 Product Scope - Annex 2 Exceptions (VERSIONED)

Annex 2 lists products **excepted from reciprocal tariffs**. These products have their IEEPA rate reset to 0%, effectively exempting them from the reciprocal framework.

**Versioned Approach:**

The analysis uses **versioned CSV files** to enable policy reconstruction at different time points:

1. **April 2, 2025** (`reciprocal_exceptions_apr2.csv`): Initial Annex 2
2. **April 9, 2025**: Semiconductor expansion - **combines** `reciprocal_exceptions_apr2.csv` + `reciprocal_exceptions_semiconductors.csv`
3. **August 1, 2025** (`reciprocal_exceptions_aug1.csv`): Revised Annex 2
4. **November 13, 2025** (`reciprocal_exceptions_251113.csv`): Latest revision (**currently used**)

**Current Setting:** November 13, 2025 version

**Annex 2 Evolution:**
```
November 13 load:    1,092 HS 8-digit codes
Lumber removal:      Applied (lumber codes transitioned to S232 in October 2025)
```

**Application:**
- **Effect:** `ieepa_rate = 0` for these products
- **Marker:** `rr_exception = 1`

**ASSUMPTION #4: Annex 2 Timing**
- **Effective Date:** November 13, 2025 version used as current policy
- **Lumber Transition:** October 14, 2025 lumber removal already applied
- **Rationale:** Reflects most recent policy iteration
- **Flexibility:** Earlier versions available for historical analysis

**Product Categories in Annex 2:**
- Semiconductors and electronics
- Pharmaceutical inputs
- Critical minerals and materials
- Specialized industrial equipment
- Other products deemed critical for supply chain security

#### 4.5 Statutory IEEPA Exceptions

**File:** `data/reciprocal_exceptions_ieepa_statute.csv` (39 HS 8-digit codes)

**Scope:** Products **statutorily excluded** from the scope of IEEPA by the International Emergency Economic Powers Act itself

**Product Categories:**
- Photographic and cinematographic goods (HS 37)
- Books, newspapers, and periodicals (HS 49)
- Recorded media (HS 85.23)
- Art and antiques (HS 97.01, 97.02)

**Treatment:**
```
ieepa_rate = 0
emergency_rate = 0
rr_exception = 1
```

**Key Implementation Detail:**

Setting `rr_exception = 1` prevents these products from being subject to country-specific emergency rates in Section 5:
- Section 5.1 (China/Hong Kong opioid): Checks `rr_exception == 0` before applying
- Section 5.2 (Canada border): Checks `rr_exception == 0` for Categories 2 & 3 before applying
- Section 5.3 (Mexico emergency): Checks `rr_exception == 0` for Category 2 before applying

**Rationale:**
- These products are explicitly excluded from IEEPA by statute (50 U.S.C. § 1702(b))
- This statutory exemption extends to all emergency orders issued under IEEPA authority
- The `rr_exception` marker ensures consistency across all sections without redundant checks

**Application:**
- **Flows affected:** 1,095 country-product flows
- **Value affected:** $6.3 billion
- **Effect:** Complete exemption from both IEEPA baseline/reciprocal rates AND all emergency tariffs

**IEEPA Weighted Average (after all exceptions):** ~8%

---

### SECTION 5: Emergency Rates (Populate emergency_rate column)

Emergency rates are country-specific security actions that add to the final rate calculation.

#### 5.1 China & Hong Kong - Synthetic Opioid Tariff (TIME-VARYING POLICY)

**TIME-VARYING POLICY:**
- **Rate:** Depends on policy_date (set in Section 0.6)
  - **Feb 4, 2025 to Mar 3, 2025:** 10%
  - **Mar 4, 2025 to Nov 9, 2025:** 20%
  - **Nov 10, 2025 onwards:** 10%
  - **Before Feb 4, 2025:** 0% (tariff not yet in effect)
- **Scope:** ALL products from China (UN 156) and Hong Kong (UN 344)
- **Product Scope Exclusions:** Only statutory IEEPA exceptions (`ieepa_statute_exception == 1`) are excluded from the emergency order
- **Annex 2 reciprocal exceptions** (`rr_exception == 1`) **remain subject** to this tariff

**Formula:**
```
# Determine rate based on policy_date
IF policy_date >= 2025-11-10:
    china_emergency_rate = 10%
ELSE IF policy_date >= 2025-03-04:
    china_emergency_rate = 20%
ELSE IF policy_date >= 2025-02-04:
    china_emergency_rate = 10%
ELSE:
    china_emergency_rate = 0%

# Apply to China/Hong Kong (except statutory IEEPA exceptions)
IF country is China or Hong Kong AND ieepa_statute_exception = 0:
    emergency_rate = china_emergency_rate
ELSE:
    emergency_rate = 0%
```

**Rationale:** Negative list approach (applies to all products except statutory IEEPA exceptions) rather than positive list (applies only to specified products). Rate varies over time based on policy changes.

**Application (as of policy_date = 2025-11-17):**
- **Current rate:** 10% (Nov 10, 2025 onwards period)
- **Value affected:** $438.6 billion
- **Flows affected:** Includes Annex 2 products
- **Marker:** `chn_opioid = 1`

**ASSUMPTION #5: Opioid Tariff Time Structure**
- **Initial Implementation (Feb 4):** 10% tariff introduced
- **Escalation (Mar 4):** Rate increased to 20%
- **De-escalation (Nov 10):** Rate reduced back to 10%
- **Policy Reconstruction:** Rate is determined by `policy_date` parameter in Section 0.6
- **Exclusion:** Only products statutorily excluded from IEEPA (e.g., books, films, art) are exempt
- **Scope Change:** Expanded from positive list (specific HS codes) to negative list (all products except exceptions)

**ASSUMPTION #5b: Opioid Tariff S232 Stacking**
- **Treatment:** China opioid emergency is ADDITIVE to ALL S232 products (auto, MHDV, steel, aluminum, copper, lumber)
- **Implementation:** Products from China with any S232 marker are flagged with `emergency_additive = 1`
- **Formula:** For materials S232, emergency is added OUTSIDE the weighted bracket (not weighted by content_share)
- **Rationale:** The opioid emergency action is a separate security measure that applies in addition to S232 tariffs, not as an alternative
- **Example:** Chinese main steel (HS 73239300) = 2% HTS + 50% S232 + 20% emergency = 72% (not 52%)

#### 5.2 Canada - Northern Border Security

**File:** `data/can_northern_border_exception.csv` (413 HS 8-digit codes)

**Scope:** Canada only (UN 124)
**Marker:** `can_northern_border = 1`

**Product Scope Exclusions:**
- **Only statutory IEEPA exceptions** (`ieepa_statute_exception == 1`) are excluded from the emergency order
- **Annex 2 reciprocal exceptions** (`rr_exception == 1`) **remain subject** to this tariff
- **No product-level exclusions** for S232 Auto or MHDV products in this section
- **Tariff stacking rules in Section 8** provide relief for transport S232 products (auto, MHDV)

Canada faces differentiated emergency rate treatment based on product category:

**Category 1: Statutory IEEPA Exception Goods**
- **Treatment:** NO emergency rate (statutorily excluded from IEEPA authority)
- **Scope:** Only products statutorily excluded from IEEPA (e.g., books, films, art)
- **Emergency rate:** 0%
- **Note:** Annex 2 reciprocal exceptions NOT included in this category

**Category 2: Energy Products (Northern Border List)**
- **File Reference:** Products in `can_northern_border_exception.csv`
- **Rate:** 10% base emergency tariff
- **USMCA Compliance:** Product-level rates from `compliance_shares.csv` (default 90%)
- **Weighted Calculation:** `usmca_compliance × 0% + (1 - usmca_compliance) × 10%`
- **Note:** Energy products use same product-level compliance as other products

**Category 3: Other Products (Non-Statutory IEEPA, Non-Energy)**
- **Rate:** 35% base emergency tariff
- **USMCA Compliance:** Product-level rates from `compliance_shares.csv` (default 90%)
- **Weighted Calculation:** `usmca_compliance × 0% + (1 - usmca_compliance) × 35%`
- **Effect:** Emergency tariff reduced proportional to USMCA compliance
- **Note:** Includes Annex 2 reciprocal exceptions, S232 Auto, and MHDV products - tariff stacking relief applied in Section 8

**Application:**
- **Value affected:** $127.4 billion (energy products, Category 2)
- **Flows affected:** 203 (energy products, Category 2)
- **Category 3 value:** $282.2 billion (includes Annex 2 products, weighted by product-level compliance)

**ASSUMPTION #6: Northern Border Treatment**
- **Category 1 (Statutory IEEPA goods):** Only $0.4B statutorily excluded from IEEPA authority
- **Category 2 (Energy):** 10% base rate with product-level USMCA compliance weighting
- **Category 3 (Other):** 35% base rate with product-level USMCA compliance weighting
- **Tariff Stacking (Section 8):** Transport S232 products receive relief through max() precedence rule
- **Compliance source:** Product-level rates from `compliance_shares.csv` (default 90% for unmatched)
- **Coverage:** Approximately 30% of Canadian imports by value (energy category)

**ASSUMPTION #6b: Northern Border S232 Copper Stacking**
- **Treatment:** Canada northern border emergency is ADDITIVE to S232 COPPER products only
- **Implementation:** Canadian copper products (s232_copper or s232_copper_derivative) are flagged with `emergency_additive = 1`
- **Formula:** For copper S232, emergency is added OUTSIDE the weighted bracket
- **Rationale:** Border security emergency applies fully on top of copper S232, not as weighted alternative
- **Other Materials:** Steel, aluminum, and lumber from Canada use weighted formula (emergency inside bracket)

**Emergency Weighted Average:** 2.36%

#### 5.3 Mexico - Emergency Tariff

**Scope:** Mexico only (UN 484)
**Marker:** `mex_emergency = 1`

**Product Scope Exclusions:**
- **Only statutory IEEPA exceptions** (`ieepa_statute_exception == 1`) are excluded from the emergency order
- **Annex 2 reciprocal exceptions** (`rr_exception == 1`) **remain subject** to this tariff
- **No product-level exclusions** for S232 Auto or MHDV products in this section
- **Tariff stacking rules in Section 8** provide relief for transport S232 products (auto, MHDV)

Mexico faces similar differentiated emergency rate treatment:

**Category 1: Statutory IEEPA Exception Goods**
- **Treatment:** NO emergency rate (statutorily excluded from IEEPA authority)
- **Scope:** Only products statutorily excluded from IEEPA (e.g., books, films, art)
- **Emergency rate:** 0%
- **Note:** Annex 2 reciprocal exceptions NOT included in this category

**Category 2: Other Products (Non-Statutory IEEPA)**
- **Rate:** 25% base emergency tariff
- **USMCA Compliance:** Product-level rates from `compliance_shares.csv` (default 90%)
- **Weighted Calculation:** `usmca_compliance × 0% + (1 - usmca_compliance) × 25%`
- **Effect:** Emergency tariff reduced proportional to USMCA compliance
- **Note:** Includes Annex 2 reciprocal exceptions, S232 Auto, and MHDV products - tariff stacking relief applied in Section 8

**Application:**
- **Value affected:** Reduced by product-level USMCA compliance weighting
- **Effect:** Mexican products face emergency rate proportional to non-compliance
- **Category 2 value:** $502.7 billion (includes Annex 2 products)

**ASSUMPTION #7: Mexico Emergency Treatment**
- **Category 1 (Statutory IEEPA goods):** Only $0.4B statutorily excluded from IEEPA authority
- **Category 2 (Other):** 25% base rate with product-level USMCA compliance weighting
- **Tariff Stacking (Section 8):** Transport S232 products receive relief through max() precedence rule
- **Compliance source:** Product-level rates from `compliance_shares.csv` (default 90% for unmatched)
- **Coverage:** Applies to all non-statutory IEEPA Mexican imports

**ASSUMPTION #7b: Mexico Emergency S232 Copper Stacking**
- **Treatment:** Mexico southern border emergency is ADDITIVE to S232 COPPER products only
- **Implementation:** Mexican copper products (s232_copper or s232_copper_derivative) are flagged with `emergency_additive = 1`
- **Formula:** For copper S232, emergency is added OUTSIDE the weighted bracket
- **Rationale:** Border security emergency applies fully on top of copper S232, not as weighted alternative
- **Other Materials:** Steel, aluminum, and lumber from Mexico use weighted formula (emergency inside bracket)

**Emergency Weighted Average:** 0.00%

---

### SECTION 6: Section 301 Rates (Populate s301_rate column)

Section 301 tariffs target China for intellectual property and technology transfer practices.

**File:** `data/sec301.csv` (10,396 HS 8-digit codes)

**Application:**
- **Target:** China only (UN 156)
- **Rates:** Variable by product (ranging from 7.5% to 100%)
- **Value affected:** $242.1 billion
- **Flows affected:** 7,572
- **Marker:** `sec301_tariff = 1`

**Product Categories:**
- Industrial machinery and equipment
- Electrical machinery and electronics
- High-technology products
- Other products identified in Section 301 investigation

**ASSUMPTION #8: Section 301 Additivity**
- **Treatment:** Section 301 rates **add to** the final rate (not replace other tariffs)
- **Rationale:** Section 301 is a separate legal instrument addressing different policy concerns (IP theft vs. national security vs. reciprocal trade)
- **Effect:** Chinese products can face multiple layers of tariffs

**S301 Weighted Average:** 1.49%

---

### SECTION 7: Country-Specific Exceptions

This section applies **special bilateral deals and agreements** that modify the rate columns calculated in Sections 2-6. Unlike earlier sections which calculate new columns, this section **overwrites** s232_rate and/or ieepa_rate values calculated in those sections.

**Two Types of Exceptions:**
1. **Type A (Blanket):** Applies to all products from a country (India)
2. **Type B (Scope-Specific):** Applies only to certain products (Brazil, UK, EU, Japan, USMCA)

#### Unified Civil Aircraft List

**File:** `data/civil_aircraft_exceptions_wto.csv` (554 codes)
- **Coverage:** 554 HS 8-digit codes for complete aircraft
- **Note:** No distinction between main aircraft and parts/components

**Use:** Shared by Brazil, Japan, EU, UK for civil aircraft exceptions
- **Basis:** WTO Civil Aircraft Agreement
- **Treatment:** Complete exemption from tariffs (both IEEPA and S232 set to 0)

#### 7.1 India (Type A - Scope-Specific Rate Change)

**Treatment:** Products from India (UN 699) that are subject to IEEPA reciprocal framework receive modified IEEPA rate

**IEEPA-Eligible Products (not in Annex 2):**
- **Formula:** `ieepa_rate = 10% + 40% = 50%`
- **Scope:** Only products subject to IEEPA reciprocal rates
- **Effect:** Indian products not excepted from reciprocal framework face 50% IEEPA rate

**Annex 2 Exception Products:**
- **Treatment:** `ieepa_rate = 0%` (same as other countries)
- **Scope:** Products in reciprocal exceptions list
- **Effect:** Exception products maintain 0% IEEPA rate (not subject to India surcharge)

**Application:**
- **Value affected:** Varies based on Annex 2 coverage
- **Key distinction:** India 40% surcharge applies only within IEEPA scope, not universally

**ASSUMPTION #9: India Rate Structure**
- **Treatment:** 10% baseline + 40% India-specific = 50% total (additive to MFN)
- **Scope Limitation:** Only applies to IEEPA-eligible products (not Annex 2 exceptions)
- **Rationale:** India surcharge is structured as addition to reciprocal framework, not separate universal tariff
- **Example:** Indian semiconductors in Annex 2 face ieepa_rate = 0%, not 50%

**Example (Indian IEEPA-eligible product with 5% standard MFN):**
```
hts_rate:       5% (standard MFN - India does not have preferential agreement)
ieepa_rate:     50% (10% baseline + 40% India-specific)
rr_exception:   0 (not in Annex 2)
Final rate:     5% + 50% = 55%
```

**Example (Indian Annex 2 product with 5% standard MFN):**
```
hts_rate:       5% (standard MFN)
ieepa_rate:     0% (Annex 2 exception)
rr_exception:   1 (in Annex 2)
Final rate:     5% + 0% = 5%
```

#### 7.2 Brazil (Type B - Scope-Specific Top-Up)

**Treatment:** Brazil receives a special top-up rate structure applied through the IEEPA component.

**Formula:** `ieepa_rate = 10% + 40%` for products subject to reciprocal rates

**Scope:** Only applies to products NOT excepted by:
- Statutory IEEPA exceptions
- Brazil-specific exceptions (`bra_exceptions.csv`)
- Civil aircraft exceptions

**Total Value Affected:** $42.2 billion

**ASSUMPTION #11: Brazil Rate Additivity**
- **Interpretation:** Brazil's "10 + 40" structure represents additions to the MFN base rate
- **Policy Language:** "Additional duty of 10 (baseline) + 40 (country-specific)"
- **Implementation:** `ieepa_rate := 10 + 40`, which adds to `hts_rate` in final calculation
- **Word "Additional":** Indicates these are added on top of existing MFN rates

**Brazil Exceptions (Three Categories):**

**1. Unconditional Exceptions:**
- **File:** `data/bra_exceptions.csv`
- **Treatment:** `ieepa_rate = 0` (revert to MFN only)
- **Effect:** Complete exemption from reciprocal framework

**2. Conditional Exception - Civil Aircraft:**
- **File:** `data/civil_aircraft_exceptions_wto.csv` (shared list)
- **Treatment:** `ieepa_rate = 0` (100% excepted)
- **Coverage:** 554 HS 8-digit codes
- **Note:** No distinction between main aircraft and parts/components in WTO list

**3. Conditional Exception - Passenger Vehicles:**
- **File:** `data/bra_passenger_vehicle_exceptions.csv`
- **Main vehicles:** `ieepa_rate = 0` (100% excepted)
- **Parts:**
  ```
  ieepa_rate = passenger_vehicle_share × 0% + (1 - passenger_vehicle_share) × (10 + 40)

  With current assumption (90% passenger):
  ieepa_rate = 0.9 × 0% + 0.1 × 50% = 5%
  ```

**ASSUMPTION #13: Brazil Passenger Vehicle Share**
- **Current setting:** 90% (most vehicle parts assumed for passenger vehicles/light trucks)
- **Rationale:** Light trucks dominate Brazilian auto parts exports to US; 10% assumed mixed/non-passenger use

**Total Brazil Exceptions:** $19.0 billion (45% of Brazil imports)
- Markers: `bra_exception`, `bra_aircraft`, `bra_vehicle`

#### 7.3 UK (Type B - Economic Prosperity Deal)

**Treatment:** Multiple specific provisions modifying both S232 and IEEPA rates

**Value Affected:** $67.4 billion

**1. Civil Aircraft (from unified list):**
- **Main aircraft:** `ieepa_rate = 0` AND `s232_rate = 0`
- **Parts:** Weighted average by civil aircraft share
  ```
  ieepa_rate = uk_civil_aircraft_share × 0% + (1 - share) × ieepa_rate_standard

  With current assumption (90% civil):
  ieepa_rate = 0.9 × 0% + 0.1 × standard = 10% of standard rate
  ```
- **Value:** $13.9 billion (514 flows)
- **Marker:** `uk_aircraft = 1`

**2. Auto S232 Products (UK-Specific):**
- **Scope:** ALL products with `s232_auto == 1` (defined in Section 3.1)
- **Formula:** `s232_rate = max(0, 10 - hts_rate)`
- **Treatment:** S232 rate adjusts so that `hts_rate + s232_rate = exactly 10%` (ceiling rate)
  - If `hts_rate >= 10%`: `s232_rate = max(0, 10 - 10) = 0` (HTS rate alone meets/exceeds ceiling)
  - If `hts_rate < 10%`: `s232_rate = 10 - hts_rate` (fills gap to reach 10% ceiling)
  - Example: `hts_rate = 2.5%` → `s232_rate = 10% - 2.5% = 7.5%` → total = 10%
- **Note:** IEEPA baseline (10%) does NOT apply to UK auto products - the S232 rate replaces it entirely
- **Marker:** `uk_auto = 1`

**ASSUMPTION #14: UK Auto Rate Formula**
- **Treatment:** UK auto products use complementary formula where S232 fills gap to reach 10% ceiling
- **Formula:** `s232_rate = max(0, 10 - hts_rate)`
- **Scope:** Applies to ALL auto S232 products (not limited to specific file list)
- **Rationale:** UK Economic Prosperity Deal caps total tariff rate at 10% for auto products
- **Effect:** S232 rate dynamically adjusts to ensure total (HTS + S232) = 10%, with IEEPA rate not applying
- **Code Reference:** us_tariff_calculation.R lines 1183-1186

**3. S232 Rate Caps and Overrides (UK-Specific):**

**Steel and Aluminum (Quota-Based Treatment):**

**ASSUMPTION #16: UK Steel/Aluminum Quantity Quota**
- **Product Scope:** Steel and aluminum products (`s232_steel == 1` OR `s232_alu == 1`)
- **Share Parameter:** `uk_steel_alu_quota_share = 1.0` (100% of UK steel/aluminum exports fall within quantity quota)
- **Formula:**
  ```
  s232_rate = uk_steel_alu_quota_share × min(s232_rate, 25%) +
              (1 - uk_steel_alu_quota_share) × s232_rate
  ```
- **With current assumption (share = 1.0):**
  ```
  s232_rate = 1.0 × min(s232_rate, 25%) + 0 × s232_rate
  s232_rate = min(s232_rate, 25%)
  ```
- **Interpretation:**
  - Within quota (share): 25% cap applies
  - Outside quota (1-share): Standard S232 rate applies (no cap)
  - Current 100% assumption means all UK steel/aluminum receives 25% cap
- **Rationale:** UK Economic Prosperity Deal provides quantity quotas for steel/aluminum with reduced rates for within-quota exports
- **Effect:**
  - Steel main (statutory 50%) → capped to 25% (before incidence ratio applied)
  - Steel derivatives (statutory 50%) → capped to 25% (before incidence ratio applied)
  - Aluminum main (statutory 50%) → capped to 25% (before incidence ratio applied)
  - Note: Effective incidence ratios are applied after the cap in Section 8
- **Marker:** `uk_steel_alu = 1`

**NOTE:** The share parameter (`uk_steel_alu_quota_share`) defaults to 1.0 (100% within-quota). If set to values less than 1.0, the formula creates a weighted average between within-quota treatment (25% cap) and outside-quota treatment (full standard S232 rate).

**MHDV:**
- **Treatment:** UK receives **NO** special treatment for MHDV products
- **Rate:** Standard S232 MHDV rates apply (25% base)
- **Rationale:** UK Economic Prosperity Deal does not provide MHDV exceptions

**Lumber Derivatives:**
```
IF s232_lumber_derivative = 1 OR s232_upholstered_furniture = 1:
    s232_rate = 10%    # Kitchen cabinets and upholstered furniture
```

**Effect:**
- Kitchen cabinets (`s232_lumber_derivative == 1`) → 10%
- Upholstered furniture (`s232_upholstered_furniture == 1`) → 10%
- Regular lumber products receive standard S232 rates (no special UK treatment)

**4. Passenger Vehicle Parts (UK-Specific - Share-Based Treatment):**

**ASSUMPTION #17: UK Passenger Vehicle Parts Treatment**
- **Product Scope:** Passenger vehicle parts defined in Section 3.3 (`s232_pv_parts == 1`)
- **Share Parameter:** `uk_car_parts_uk_built_share = 0.75` (75% of UK PV parts exported to USA are used in UK-built cars)
- **Formula:**
  ```
  s232_rate = uk_car_parts_uk_built_share × max(0, 10 - hts_rate) +
              (1 - uk_car_parts_uk_built_share) × s232_rate_standard
  ```
- **With current assumption (share = 0.75):**
  ```
  s232_rate = 0.75 × max(0, 10 - hts_rate) + 0.25 × s232_rate_standard
  ```
- **Interpretation:**
  - For parts used in UK-built cars (75%): Same formula as UK auto parts (10% ceiling)
  - For parts used in other cars (25%): Standard S232 treatment applies
  - Current 75% assumption means most UK PV parts receive preferential 10% ceiling treatment, with remainder facing standard rates
- **Code Reference:** us_tariff_calculation.R lines 1214-1218
- **Rationale:** UK Economic Prosperity Deal extends preferential treatment to parts that will be incorporated into UK-manufactured vehicles
- **Effect:** UK PV parts for UK-built cars receive reduced S232 rates, maintaining complementary relationship with MFN and IEEPA (10%)
- **Marker:** `uk_pv_parts = 1`

**NOTE:** The share parameter (`uk_car_parts_uk_built_share`) is set to 0.75 (75% UK-built). The formula creates a weighted average between preferential treatment (for UK-destined parts) and standard treatment (for parts used in non-UK vehicles).

#### 7.4 Japan (Type B - Tariff Floors)

**Treatment:** Japan products face tariff floors applied to IEEPA and S232 components separately. Each component has its own floor rate parameter, enabling independent scenario modeling (e.g., IEEPA struck down while S232 floors remain).

**Floor Rate Parameters (Decoupled):**
- `jpn_ieepa_floor_rate`: Floor for reciprocal tariffs (IEEPA component)
- `jpn_s232_floor_rate`: Floor for S232 auto/MHDV tariffs

**Formula:** `ieepa_rate = max(jpn_ieepa_floor_rate, hts_rate) - hts_rate`
**Formula:** `s232_rate = max(jpn_s232_floor_rate, hts_rate) - hts_rate` (for auto/MHDV)

**ASSUMPTION #18: Japan Tariff Floor Implementation**
- **Principle:** Floor establishes minimum rate but does not reduce existing higher rates
- **Method:** Calculate `max(floor_rate, hts_rate)` to ensure neither floor nor MFN is violated
- **Storage:** Store as addition to MFN: `ieepa_rate = max(floor, hts) - hts`
- **Effect:** If MFN > floor, `ieepa_rate = 0` (MFN alone satisfies floor)
- **Effect:** If floor > MFN, `ieepa_rate = floor - MFN` (brings total up to floor)
- **Decoupling:** IEEPA and S232 floors are separate parameters, allowing independent scenario modeling
- **Applies to products NOT exempt from reciprocal rates:** `rr_exception == 0 & ieepa_statute_exception == 0`

**Formula Derivation:**

The final rate formula is: `rate = hts_rate + ieepa_rate + other_components`

For a floor to work properly, we need: `rate >= floor_rate` AND `rate >= hts_rate`

This means the effective rate should be: `max(floor_rate, hts_rate)`

Solving for `ieepa_rate`:
```
hts_rate + ieepa_rate = max(floor_rate, hts_rate)
ieepa_rate = max(floor_rate, hts_rate) - hts_rate
```

**Examples:**
- **Case 1:** floor = 15%, hts = 5%
  - `ieepa_rate = max(15%, 5%) - 5% = 10%`
  - `final = 5% + 10% = 15%` (floor achieved)

- **Case 2:** floor = 15%, hts = 20%
  - `ieepa_rate = max(15%, 20%) - 20% = 0%`
  - `final = 20% + 0% = 20%` (MFN preserved, exceeds floor)

**Code Reference:** us_tariff_calculation.R lines 1254-1267 (Japan), 1314-1327 (EU)

**Floor Rate Application (Japan):**

**Data Object:** `floor_jpn` (loaded from `data/tariff_floor_rates.csv`)
- **Coverage:** Product-by-product floor rates for Japan
- **Column:** `floor_rate` or `new_eu_rate` (depending on file version)
- **Default:** 15% floor for products without specific floor data

**Formula (Product-by-Product):**
```
FOR Japan (UN code 392):
  IF floor_rate exists for product:
      ieepa_rate = max(floor_rate, hts_rate) - hts_rate
  ELSE:
      ieepa_rate = max(15%, hts_rate) - hts_rate
```

**Civil Aircraft (Japan via WTO Agreement):**
- **Main aircraft:** `ieepa_rate = 0` AND `s232_rate = 0`
- **Parts:** Weighted by Japan civil aircraft share (90% → near-full exemption)
- **Marker:** `wto_aircraft = 1`

**S232 Rate Caps for Japan:**
```
IF s232_auto = 1 OR s232_mhdv = 1:
    s232_rate = max(15%, hts_rate) - hts_rate
```
- **Effect:** Auto and MHDV S232 products receive 15% floor treatment (same as general IEEPA floor)

**Lumber Derivatives (Japan):**
```
IF s232_lumber_derivative = 1 OR s232_upholstered_furniture = 1:
    s232_rate = 15%    # Kitchen cabinets and upholstered furniture
```
- **Effect:** Kitchen cabinets and upholstered furniture receive 15% S232 rate
- **Marker:** `jpn_lumber_derivative = 1`

**Japan Coverage:**
- **Value:** $146.9 billion
- **Effect:** Ensures minimum 15% rate (or MFN if higher)

**Verification:**
- Japan average IEEPA rate: 12.65% (reflects floor logic correctly)

#### 7.5 EU (Type B - Tariff Floors + Pharma)

**Treatment:** EU products face tariff floors applied to IEEPA and S232 components separately (decoupled), similar to Japan, plus a non-patented pharmaceuticals exception.

**Floor Rate Parameters (Decoupled):**
- `eu_ieepa_floor_rate`: Floor for reciprocal tariffs (IEEPA component)
- `eu_s232_floor_rate`: Floor for S232 auto/MHDV tariffs

**Floor Rate Application (EU):**

**Data Object:** `floor_eu` (loaded from `data/tariff_floor_rates.csv`)
- **Coverage:** Product-by-product floor rates for EU members
- **Column:** `floor_rate` or `new_eu_rate` (depending on file version)
- **Default:** 15% floor for products without specific floor data

**Formula (Product-by-Product):**
```
FOR EU members (27 countries):
  IF floor_rate exists for product:
      ieepa_rate = max(eu_ieepa_floor_rate, hts_rate) - hts_rate
  ELSE:
      ieepa_rate = max(15%, hts_rate) - hts_rate
  
  FOR S232 auto/MHDV:
      s232_rate = max(eu_s232_floor_rate, hts_rate) - hts_rate
```

**EU Unconditional Exceptions:**
- **File:** `data/eu_exceptions.csv`
- **Implementation note:** HS-8 codes are zero-padded to 8 digits using sprintf formatting to ensure consistent matching
- **Treatment:** `ieepa_rate = 0` (revert to MFN only)
- **Value:** $169.2 billion (4,315 flows)
- **Marker:** `eu_exception = 1`

**Non-Patented Pharmaceuticals Exception (EU):**

**ASSUMPTION #33: EU Non-Patented Pharma Share**
- **Product Scope:** Pharmaceutical products list (`pharma_exceptions.csv`, 519 codes)
- **Share Parameter:** `eu_nonpatented_pharma_share = 0.4` (40% non-patented in US)
- **Treatment:** Share of pharma imports not patented in US is exempt from IEEPA reciprocal rates
- **Formula:** Applied directly to IEEPA rate in Section 7.5
  ```
  ieepa_rate = (1 - nonpatented_share) × ieepa_rate

  With share = 0.4 (40% non-patented):
  ieepa_rate = 0.6 × ieepa_rate
  ```
- **Rationale:** Products not under US patent protection receive reduced/eliminated reciprocal tariffs
- **Code Reference:** us_tariff_calculation.R lines ~1461-1474
- **Similar to:** Switzerland pharma exception (`che_nonpatented_pharma_share`)
- **Marker:** `eu_pharma = 1`

**Civil Aircraft (EU via WTO Agreement):**
- **Main aircraft:** `ieepa_rate = 0` AND `s232_rate = 0`
- **Parts:** Weighted by EU civil aircraft share (90% → near-full exemption)
- **Marker:** `wto_aircraft = 1`

**S232 Rate Caps for EU:**
```
IF s232_auto = 1 OR s232_mhdv = 1:
    s232_rate = max(15%, hts_rate) - hts_rate
```
- **Effect:** Auto and MHDV S232 products receive 15% floor treatment (same as general IEEPA floor)

**Lumber Derivatives (EU):**
```
IF s232_lumber_derivative = 1 OR s232_upholstered_furniture = 1:
    s232_rate = 15%    # Kitchen cabinets and upholstered furniture
```
- **Effect:** Kitchen cabinets and upholstered furniture receive 15% S232 rate
- **Marker:** `eu_lumber_derivative = 1`

**EU Coverage:**
- **Value:** $600.8 billion
- **Effect:** Ensures minimum floor rate (varies by product, default 15%)

**Verification:**
- EU average IEEPA rate: 10.28% (reflects floor logic correctly)

**Combined Japan & EU Civil Aircraft:**
- **Total value:** $102.5 billion (8,770 flows across both Japan and EU)

**S232 Rate Caps (Japan & EU Combined):**

**Auto and MHDV S232:**
```
IF s232_auto = 1 OR s232_mhdv = 1:
    s232_rate = max(15%, hts_rate) - hts_rate    # 15% floor treatment
```

**Effect:** Auto/MHDV tariffs reduced from standard 25% to 15% floor (or MFN if higher)

---

### SECTION 8: Calculate Final Applied Rate

This section combines all the separate rate columns using **four distinct formulas** based on tariff applicability. All USMCA compliance weighting is applied here for consistency.

**CRITICAL POLICY RULES:**
1. Transport S232 tariffs (auto/MHDV/PV parts) **completely replace** IEEPA and emergency rates
2. Products with NO tariffs (no S232, no IEEPA, no emergency) only face HTS + S301 rates
3. The four formulas provide **complete, mutually exclusive coverage** of all product-country combinations

#### 8.1 USMCA HTS Rate Weighting (Step 1)

**Purpose:** Apply USMCA compliance weighting to HTS rates for Canada and Mexico.

**Implementation:** An intermediate column `hts_rate_weighted` is calculated using product-level USMCA compliance rates from `compliance_shares.csv`.

**Formula:**
```
hts_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × hts_rate
```

Where `usmca_compliance` is the product-level compliance rate (default 90% for unmatched products).

**Example (with 90% product-level compliance and 2.5% MFN rate):**
```
hts_rate_weighted = 0.90 × 0% + 0.10 × 2.5% = 0.25%
```

**For all other countries:**
```
hts_rate_weighted = hts_rate (no weighting applied)
```

**Value affected:** $913.0 billion (Canada + Mexico)

**Code Reference:** us_tariff_calculation_db.R Section 8, Step 1

---

#### 8.2 Complete Step-by-Step Execution Flow

Section 8 executes **8 sequential steps** to calculate the final applied rate. Understanding the execution order is critical:

**Phase 1: Create Weighted Rate Columns (Steps 1-4)**

These steps create intermediate `*_weighted` columns by applying USMCA compliance weighting to base rates:

| Step | Operation | Code Lines | Output Column | Purpose |
|------|-----------|------------|---------------|---------|
| **1** | HTS weighting | 1393-1402 | `hts_rate_weighted` | Apply USMCA preferential rates |
| **2** | Emergency weighting | 1407-1434 | `emergency_rate_weighted` | Apply USMCA emergency exemptions |
| **3** | IEEPA weighting | 1437-1447 | `ieepa_rate_weighted` | Apply USMCA IEEPA exemptions |
| **4** | S232 weighting | 1449-1504 | `s232_rate_weighted` | Apply USMCA S232 auto/MHDV exemptions |

**Phase 2: Prepare Calculation Markers (Steps 5-7)**

These steps create helper columns that the formulas will use:

| Step | Operation | Code Lines | Output Columns | Purpose |
|------|-----------|------------|----------------|---------|
| **5** | Create S232 type markers | 1506-1516 | `transport_s232`, `materials_s232` | Distinguish auto/MHDV from metals/lumber |
| **6** | Determine content share | 1518-1533 | `content_share` | Set share for derivatives (0.5) and parts (1.0) |
| **7** | Determine aircraft share | 1536-1545 | `aircraft_share` | Set country-specific civil aircraft shares |

**Phase 3: Apply Final Rate Formulas (Step 8)**

This step applies one of four mutually exclusive formulas to calculate the final rate:

| Step | Operation | Code Lines | Output Column | Coverage |
|------|-----------|------------|---------------|----------|
| **8** | Apply four formulas | 1547-1579 | `rate` | 100% of flows |

**Why This Order?**

1. **Weighting first (Steps 1-4):** Must create weighted columns before using them in formulas
2. **Markers next (Steps 5-7):** Must identify product types before selecting formula
3. **Formulas last (Step 8):** Uses all weighted columns and markers to calculate final rate

**Error Detection Pattern:**

The code uses a clever error-detection pattern:

```r
# Initialize all rates to -100% (impossible value)
us_imports[, rate := -100]

# Apply the four formulas...
# (Each formula sets rate to a valid value)

# Verify complete coverage
if(any(us_imports$rate == -100)) {
  stop("ERROR: Some products have no formula applied!")
}
```

**Purpose:** Ensures every product matches at least one formula. If any product remains at -100%, it indicates a coverage gap (logic error). This is better than implicit defaults which might hide bugs.

---

#### 8.3 Four-Formula Approach for Complete Coverage

The final rate calculation uses **four distinct formulas** that provide complete, mutually exclusive coverage of all possible tariff scenarios.

**Step 5: Create S232 Type Markers**

Products are classified into mutually exclusive categories:
- `transport_s232 = 1` if product subject to auto S232, MHDV S232, or PV parts S232
- `materials_s232 = 1` if product subject to steel, aluminum, copper, or lumber S232
- Both = 0 if product not subject to any S232 tariff

**Step 6: Determine Effective Incidence Ratio (content_share)**

For products subject to S232 tariffs, determine the effective incidence ratio. These ratios are calibrated from 2025 USITC trade data and represent effective tariff incidence, not physical metal content. See `memo_metal_ratio_assumptions.md` for calibration methodology.

**Steel:**
- **Main products:** `content_share = 0.85` (85% effective incidence; 15% benefits from exclusions)
- **Derivatives:** `content_share = 0.40` (40% effective incidence)

**Aluminum:**
- **Main products:** `content_share = 0.94` (94% effective incidence; limited exclusion use)
- **Derivatives:** `content_share = 0.35` (35% effective incidence)

**Copper:**
- **Main products:** `content_share = 0.70` (70% effective incidence; provisional)
- **Note:** No copper derivatives currently in S232 scope

**Lumber:**
- **Main products:** `content_share = 1.0` (no empirical calibration available)
- **Derivatives (cabinets):** `content_share = 0.50` (50% timber content assumed)

**Transport S232:**
- **PV parts:** `content_share = pv_share_in_parts` (default: 1.0)
- **MHDV parts:** `content_share = mhdv_share_in_parts` (default: 1.0)

**Step 7: Determine Aircraft Share**

For products marked as WTO civil aircraft (`wto_aircraft = 1`), apply country-specific aircraft share:
- **Brazil:** `aircraft_share = bra_civil_aircraft_share` (default: 0.9)
- **Japan:** `aircraft_share = jpn_civil_aircraft_share` (default: 0.9)
- **EU:** `aircraft_share = eu_civil_aircraft_share` (default: 0.9)
- **UK:** `aircraft_share = uk_civil_aircraft_share` (default: 0.9)
- **Korea:** `aircraft_share = kor_civil_aircraft_share` (default: 0.9)
- **Switzerland/Liechtenstein:** `aircraft_share = che_civil_aircraft_share` (default: 0.9)
- **All others:** `aircraft_share = 0` (no aircraft exception)

**NOTE:** Aircraft share applies ONLY to materials S232, NOT to transport S232.

**Step 8: Calculate Final Rate Using Four-Formula Approach**

**FORMULA 0: NO S232, NO IEEPA, NO EMERGENCY (most basic case)**
```
IF transport_s232 == 0 AND materials_s232 == 0 AND ieepa_rate == 0 AND emergency_rate == 0:
  rate = hts_rate_weighted + s301_rate
```

**Key Features:**
- **Simplest case** - Only base HTS rate and China S301 (if applicable)
- **No policy tariffs** - Product not subject to any Trump 2.0 tariff measures
- **Typical products:** Non-strategic goods from countries without reciprocal/emergency tariffs

**Example (Canadian machinery, USMCA-compliant):**
```
rate = 0% (USMCA preferential) + 0% (no S301) = 0%
```

**Example (Australian consumer goods):**
```
rate = 5% (MFN) + 0% (no S301) = 5%
```

**Value affected:** ~$275.8 billion (products with no Trump 2.0 policy tariffs)

---

**FORMULA 1: TRANSPORT S232 (auto/MHDV/PV parts)**
```
IF transport_s232 == 1:
  rate = hts_rate_weighted + s232_rate_weighted + emergency_rate_weighted + s301_rate
```

**Key Features:**
- **NO IEEPA rate** - Transport S232 replaces IEEPA reciprocal tariff
- **YES emergency rate** - Country-specific emergency actions still apply
- **Rationale:** Transport S232 replaces the reciprocal tariff framework (IEEPA), but country-specific emergency actions (China opioid, Canada border) are independent security measures that apply in addition to S232

**Example (Chinese lithium-ion battery, HS 85076000):**
```
rate = 3.4% (MFN) + 25% (auto S232) + 10% (China opioid emergency) + 25% (S301) = 63.4%
(NO IEEPA 10% baseline - replaced by transport S232)
```

**Example (Chinese auto, HS 87032390):**
```
rate = 2.5% (MFN) + 25% (auto S232) + 10% (China opioid emergency) + 25% (S301) = 62.5%
```

**FORMULA 2: MATERIALS S232 (steel/aluminum/copper/lumber)**

**Emergency Stacking Rules:**
Materials S232 uses an `emergency_additive` marker to determine if emergency tariffs stack additively:
- **China (all S232 products):** Emergency stacks additively (`emergency_additive = 1`)
- **Canada/Mexico (copper only):** Emergency stacks additively (`emergency_additive = 1`)
- **Other materials S232:** Emergency inside weighted bracket (`emergency_additive = 0`)

```
IF materials_s232 == 1 AND emergency_additive == 1:
  # Emergency stacks ADDITIVELY (outside weighted bracket)
  rate = hts_rate_weighted +
         (1 - aircraft_share) * [content_share × s232_rate_weighted +
                                 (1 - content_share) × ieepa_rate_weighted] +
         emergency_rate_weighted + s301_rate

IF materials_s232 == 1 AND emergency_additive == 0:
  # Emergency INSIDE weighted bracket (weighted with IEEPA)
  rate = hts_rate_weighted +
         (1 - aircraft_share) * [content_share × s232_rate_weighted +
                                 (1 - content_share) × (ieepa_rate_weighted + emergency_rate_weighted)] +
         s301_rate
```

**Key Features:**
- **Nested formula:** Aircraft share wraps the entire weighted bracket
- **Content share:** Determines portion subject to S232 vs. IEEPA
- **Emergency stacking:** China opioid and Canada/Mexico copper emergencies stack on top of S232
- **Aircraft exception:** Reduces tariffs proportionally for civil aircraft (but NOT emergency when additive)

**Example (Chinese steel derivative HS 73239300, 40% incidence, emergency additive):**
```
rate = 2% (MFN) +
       (1 - 0) * [0.40 × 50% + 0.60 × 10% IEEPA] +
       20% (opioid emergency) + 0% (S301)
     = 2% + [20% + 6%] + 20% = 48%
(Emergency stacks additively outside weighted bracket)
```

**Example (Chinese main steel product, 85% incidence, emergency additive):**
```
rate = 2% (MFN) +
       (1 - 0) * [0.85 × 50% + 0.15 × 10%] +
       20% (opioid emergency) + 0% (S301)
     = 2% + [42.5% + 1.5%] + 20% = 66%
(85% incidence reflects exclusion-adjusted effective rate)
```

**Example (Brazilian steel derivative, 40% incidence, 90% civil aircraft):**
```
rate = 2% (MFN) +
       (1 - 0.9) * [0.40 × 50% + 0.60 × 50% IEEPA] +
       0% (no emergency for Brazil) + 0% (S301)
     = 2% + 0.1 × [20% + 30%] = 2% + 5% = 7%
(90% aircraft exemption: 10% of tariff bracket applies)
```

**Example (Indian steel derivative, 40% incidence, NOT aircraft, emergency_additive = 0):**
```
rate = 2% (MFN) +
       (1 - 0) * [0.40 × 50% + 0.60 × (50% IEEPA + 0% emergency)] +
       0% (S301)
     = 2% + 1.0 * [20% + 30%] = 52%
(40% incidence for S232, remaining 60% subject to IEEPA)
```

**FORMULA 3: NO S232, but IEEPA and/or EMERGENCY present**
```
IF transport_s232 == 0 AND materials_s232 == 0 AND (ieepa_rate > 0 OR emergency_rate > 0):
  rate = hts_rate_weighted + ieepa_rate_weighted + emergency_rate_weighted + s301_rate
```

**Key Features:**
- **Standard additive formula** - All tariffs simply add together
- **No weighted average** - Full rates apply
- **No S232** - Product not subject to national security tariffs

**Example (Chinese textiles):**
```
rate = 10% (MFN) + 30% (IEEPA) + 10% (opioid emergency) + 15% (S301) = 65%
```

**Example (Indian electronics):**
```
rate = 2% (MFN) + 50% (IEEPA: 10% baseline + 40% surcharge) + 0% (no emergency) + 0% (no S301) = 52%
```

---

**Coverage Verification:**

The four formulas provide **complete, mutually exclusive coverage**:

| Formula | Condition | S232 | IEEPA | Emergency | Products |
|---------|-----------|------|-------|-----------|----------|
| 0 | Most basic | NO | NO | NO | ~$275.8B |
| 1 | Transport | YES (transport) | NO (replaced) | YES | ~$XXB |
| 2 | Materials | YES (materials) | YES (weighted) | YES (weighted) | ~$XXB |
| 3 | Standard | NO | YES | YES | ~$XXB |

**Total:** 100% of import flows covered

---

**Rate Formula Column:**

To help users understand which tariffs were considered and how they were aggregated, the data includes a `rate_formula` column that explains the specific formula applied to each import flow.

**Column values by formula:**

| Formula | rate_formula Value | Explanation |
|---------|-------------------|-------------|
| **0** | `"HTS + S301"` | Only base HTS rate plus China S301 (if applicable) |
| **1** | `"Transport S232: HTS + S232(auto) + Emergency + S301"` | Auto S232 with emergency rates |
| **1** | `"Transport S232: HTS + S232(MHDV) + Emergency + S301"` | MHDV S232 with emergency rates |
| **1** | `"Transport S232: HTS + S232(PV parts) + Emergency + S301"` | PV parts S232 with emergency rates |
| **2** | `"Materials S232 (steel): HTS + weighted[S232, IEEPA] + Emergency + S301"` | Steel S232 with emergency stacking (China) |
| **2** | `"Materials S232 (steel): HTS + weighted[S232, (IEEPA+Emergency)] + S301"` | Steel S232 with weighted IEEPA/emergency (other) |
| **2** | `"Materials S232 (aluminum): HTS + weighted[S232, IEEPA] + Emergency + S301"` | Aluminum S232 with emergency stacking (China) |
| **2** | `"Materials S232 (aluminum): HTS + weighted[S232, (IEEPA+Emergency)] + S301"` | Aluminum S232 with weighted IEEPA/emergency (other) |
| **2** | `"Materials S232 (copper): HTS + weighted[S232, IEEPA] + Emergency + S301"` | Copper S232 with emergency stacking (China, Canada, Mexico) |
| **2** | `"Materials S232 (copper): HTS + weighted[S232, (IEEPA+Emergency)] + S301"` | Copper S232 with weighted IEEPA/emergency (other) |
| **2** | `"Materials S232 (lumber): HTS + weighted[S232, IEEPA] + Emergency + S301"` | Lumber S232 with emergency stacking (China) |
| **2** | `"Materials S232 (lumber): HTS + weighted[S232, (IEEPA+Emergency)] + S301"` | Lumber S232 with weighted IEEPA/emergency (other) |
| **3** | `"HTS + IEEPA + Emergency + S301"` | Standard additive formula without S232 |

**Key notation:**
- **"weighted[S232, IEEPA] + Emergency"** indicates emergency stacks additively (China all S232, Canada/Mexico copper)
- **"weighted[S232, (IEEPA+Emergency)]"** indicates emergency is inside weighted bracket (other materials S232)
- The `emergency_additive` marker determines which formula applies
- All rates shown are after USMCA compliance weighting where applicable
- Aircraft exemptions are applied to the weighted bracket in Formula 2 (but NOT to additive emergency)

**Usage:**
This column allows users to quickly identify:
1. Which policy tariffs apply to each product
2. Whether the product uses simple additive formula or complex weighting
3. The specific S232 type (auto, MHDV, steel, aluminum, etc.) for S232-covered products

---

#### 8.4 Emergency Rate Weighting (Step 2)

**Purpose:** Apply USMCA compliance weighting to emergency rates for Canada and Mexico.

**Implementation:** An intermediate column `emergency_rate_weighted` is calculated using product-level USMCA compliance rates. The energy exception list determines the base rate (10% vs 35%), but all products use their empirical compliance rates.

**For Canada energy products (10% base rate):**
```
emergency_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × 10%
```
Example with 90% compliance: `0.90 × 0% + 0.10 × 10% = 1.0%`

**For Canada non-energy products (35% base rate):**
```
emergency_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × 35%
```
Example with 90% compliance: `0.90 × 0% + 0.10 × 35% = 3.5%`

**For Mexico products (25% base rate):**
```
emergency_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × 25%
```
Example with 90% compliance: `0.90 × 0% + 0.10 × 25% = 2.5%`

**For China/Hong Kong (not USMCA):**
```
emergency_rate_weighted = 10% (opioid tariff, no weighting)
```

**For all other countries:**
```
emergency_rate_weighted = emergency_rate (no weighting applied)
```

**Note:** The energy exception list only determines which base rate applies (10% vs 35% for Canada). All products, including energy, use their product-level compliance rates from `compliance_shares.csv` (default 90% for unmatched).

#### 8.5 IEEPA Rate Weighting (Step 3)

**Purpose:** Apply USMCA compliance weighting to IEEPA reciprocal rates for Canada and Mexico.

**Implementation:** An intermediate column `ieepa_rate_weighted` is calculated using product-level USMCA compliance rates.

**Formula:**
```
ieepa_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × ieepa_rate
```

Where `usmca_compliance` is the product-level compliance rate (default 90% for unmatched).

**Example (with 90% compliance and 10% IEEPA rate):**
```
ieepa_rate_weighted = 0.90 × 0% + 0.10 × 10% = 1.0%
```

**For all other countries:**
```
ieepa_rate_weighted = ieepa_rate (no weighting applied)
```

**Value affected:** $913.0 billion (Canada + Mexico)

#### 8.6 S232 Auto and MHDV Rate Weighting (Step 4)

**Purpose:** Apply USMCA compliance weighting to S232 auto and MHDV rates for Canada and Mexico using parameterized rates.

**Implementation:** An intermediate column `s232_rate_weighted` is calculated using product-level USMCA compliance rates for auto and MHDV products.

**Key Parameters (from Section 0.3a):**
- `s232_auto_rate = 25%` (automobiles and light trucks standard rate)
- `s232_mhdv_main_rate = 25%` (MHDV main products standard rate)
- `s232_mhdv_parts_rate = 25%` (MHDV parts standard rate)
- `s232_usmca_rate = 0%` (USMCA preferential rate)

**For Auto S232 (Canada/Mexico):**
```
s232_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × s232_auto_rate
```
Example with 90% product-level compliance: `0.90 × 0% + 0.10 × 25% = 2.5%`

**For MHDV main products (Canada/Mexico):**
```
s232_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × s232_mhdv_main_rate
```
Example with 90% product-level compliance: `0.90 × 0% + 0.10 × 25% = 2.5%`

**For MHDV parts (Canada/Mexico):**
```
s232_rate_weighted = usmca_compliance × 0% + (1 - usmca_compliance) × s232_mhdv_parts_rate
```
Example with 90% product-level compliance: `0.90 × 0% + 0.10 × 25% = 2.5%`

**Note:** Product-level compliance rates from `compliance_shares.csv` are used. Products without observations default to 90%.

**For all other products:**
```
s232_rate_weighted = s232_rate (no weighting applied)
```

**Implementation:**
- **Auto S232:** Applies USMCA compliance weighting
- **MHDV main:** Uses weighted average formula
- **Consistency:** All transport S232 products (auto, MHDV main, MHDV parts) use the same weighted average structure

#### Validation Checks

**Performed:**
1. **Negative rates:** 0 flows (✓ passed)
2. **Rates > 200%:** 3 flows (acceptable - India 50% + China S301 60% + emergency 10% + high MFN)
3. **Missing rates:** 0 flows (✓ passed)

**Final Weighted Average Applied Rate:** 17.75%

---

### SECTION 9: Summary and Export

#### Summary Statistics

**By Rate Type:**
- **MFN only:** $1,029.6 billion (31.8%)
  - Products facing no additional tariffs beyond MFN

- **S232 applied:** $1,096.1 billion (33.8%)
  - Products where S232 > IEEPA (S232 precedence)

- **IEEPA applied:** $959.1 billion (29.6%)
  - Products where IEEPA > S232 or no S232

- **Emergency/S301:** Included in above categories (added to final rate)

#### Output Files

**1. RData File:**
- **Path:** `results/processed_us_imports_with_rates.RData`
- **Size:** 12 MB
- **Contents:** Complete data table with all rate columns and markers
- **Use:** For further analysis in R

**2. Excel File:**
- **Path:** `results/processed_us_imports_with_rates.xlsx`
- **Size:** 39 MB
- **Contents:** All columns exported for review
- **Columns included:**
  - Trade data: exporter, ISO2, UN code, HS code, description, import values
  - Rate columns: hts_rate, s232_rate, ieepa_rate, emergency_rate, s301_rate, **rate**
  - Composite rates: rate_s25, rate_s50, rate_s75, rate_mhdv_*
  - Boolean markers: All 20 marker columns

**3. Missing Codes Log:**
- **Path:** `results/mfn_missing_codes_log.csv`
- **Contents:** 173 HS codes missing from MFN schedule, with import values
- **Use:** Documentation and transparency

---

## V. Key Assumptions Summary

This section consolidates all explicit assumptions made in the methodology:

### Data Assumptions

**ASSUMPTION #1: Missing MFN Rates**
- Products without MFN rates assigned 0%
- Affects 173 codes ($110.9B, 3.4%)
- Documented in mfn_missing_codes_log.csv

**ASSUMPTION #2: Country Rate Interpretation**
- Country-specific rates are **added to** 10% baseline (not replacing)
- Example: 15% country rate → total 25% IEEPA (10% + 15%)

**ASSUMPTION #3: Chapter 98 Treatment**
- Chapter 98 excluded from reciprocal rates
- Four specific codes remain subject: 98020080, 98020040, 98020050, 98020060

**ASSUMPTION #4: Annex 2 Timing**
- August 1, 2025 version used as current policy
- October 14 lumber removal applied
- Earlier versions available for reconstruction

### Effective Incidence Ratio Assumptions

**ASSUMPTION #5: S232 Effective Incidence Ratios (Calibrated from 2025 USITC Data)**
- **Steel derivatives:** 40% effective incidence (trade-weighted average Jan-Sep 2025)
- **Steel main products:** 85% effective incidence (reflects ~15% exclusion utilisation)
- **Aluminum derivatives:** 35% effective incidence
- **Aluminum main products:** 94% effective incidence (limited exclusion use)
- **Copper main products:** 70% effective incidence (provisional; 2 months data)
- **Lumber cabinets:** 50% timber content assumed (no empirical calibration)
- **Note:** These ratios represent effective tariff incidence, not physical metal content. See `memo_metal_ratio_assumptions.md` for full calibration methodology.

**ASSUMPTION #6: End-Use Shares**
- MHDV parts: 100% destined for MHDV assembly (mhdv_share_in_parts = 1.0)
- PV parts: 100% destined for passenger vehicle assembly (pv_share_in_parts = 1.0)
- Note: Share parameters set to 1.0 for full tariff application; adjust if mixed-use evidence available

### Compliance Assumptions

**ASSUMPTION #7: USMCA Compliance (Product-Level)**
- **Data source:** `compliance_shares.csv` from USITC DataWeb (July-September 2025)
- **Coverage:** 11,572 product-country combinations (HS 8-digit × origin)
- **Default for unmatched:** 90% compliance
- **Trade-weighted average:** Canada ~89%, Mexico ~87%
- **Effect:** USMCA partners face reduced tariffs proportional to compliance

**ASSUMPTION #8: Northern Border Energy Products**
- Energy exception list determines base emergency rate (10% vs 35% for Canada)
- Energy products use product-level compliance like all other products
- No special compliance override for energy (removed 0% compliance assumption)

**ASSUMPTION #9: Conditional Exception Shares**
- Brazil, Japan, EU, UK, Korea, Switzerland civil aircraft: 90% assumed civil use
- Brazil passenger vehicles: 90% assumed passenger/light truck use
- UK car parts for UK-built cars: 75% share
- Reflects estimated civil/passenger vehicle use proportions

### Policy Interpretation Assumptions

**ASSUMPTION #10: Rate Additivity (Brazil)**
- Brazil's "10 + 40" structure adds to MFN base rate
- Formula: MFN + 10 + 40
- Rationale: Policy language uses "additional duty"

**ASSUMPTION #11: Tariff Floor Interpretation (EU/Japan)**
- Floor establishes minimum rate without reducing higher existing MFN rates
- Formula: max(floor, MFN)
- Implementation: `ieepa_rate = max(floor, hts) - hts`

**ASSUMPTION #12: Composite Rate MFN Treatment**
- MFN applies to full product value (100%)
- S232 applies only to covered material portion (e.g., 25% steel content)
- Formula: MFN + (share × S232)
- Ensures MFN is not lost on the S232-covered portion

**ASSUMPTION #13: Weighted Sum Approach**
- S232 and IEEPA apply to different portions based on content/end-use shares
- Formula: rate = mfn + (share × s232) + ((1-share) × ieepa) + s301
- Reflects that both tariff types can apply simultaneously as weighted average

**ASSUMPTION #14: Section 301 Additivity**
- S301 rates ADD to final rate (not replace)
- Separate legal instrument addressing different policy concerns
- Chinese products can face multiple tariff layers

**ASSUMPTION #15: Emergency Rate Additivity**
- Emergency rates ADD to final rate
- Apply on top of all other tariffs
- Reflect country-specific security concerns separate from trade policy

---

## VI. Precedence Rules

### Hierarchical Application

The methodology implements a clear hierarchy:

**1. Section 232 Investigations (Internal Precedence):**
```
AUTO S232 (highest)
  ↓ blocks
MHDV S232
  ↓ blocks
PV Parts S232
  ↓ blocks
Steel/Aluminum/Copper/Lumber S232 (lowest)
```

**Effect:** Products subject to auto S232 are exempt from all other S232 tariffs

**2. Section 232 vs. IEEPA (External Precedence):**
```
MAX(s232_rate, ieepa_rate)
```

**Effect:** The higher of the two is used; they do not stack

**3. Emergency and S301 (Always Additive):**
```
Final rate = hts_rate + MAX(s232, ieepa) + emergency_rate + s301_rate
```

**Effect:** Emergency and S301 always add to the final rate

### Decision Tree

```
FOR each product-country flow:

1. Calculate hts_rate
   - Use 0% (preferential rate) for USMCA partners (Layer 1)
   - Use standard MFN rate for others

2. Calculate s232_rate
   - Check auto S232 (highest priority)
   - If not auto, check MHDV S232
   - If not MHDV, check PV Parts S232
   - If not PV, check steel/alu/copper/lumber S232
   - Apply content shares for derivatives
   - Result: s232_rate value

3. Calculate ieepa_rate
   - Start with 10% baseline
   - Add country-specific top-up
   - Subtract if Chapter 98 (except 4 codes)
   - Set to 0 if in Annex 2
   - Result: ieepa_rate value

4. Calculate emergency_rate
   - Check if China/HK and not Annex 2 → add 10%
   - Check if Canada and in border list → add 35%
   - Result: emergency_rate value

5. Calculate s301_rate
   - Check if China and in S301 list → add S301 rate
   - Result: s301_rate value

6. Apply country exceptions (Section 7)
   - MODIFY s232_rate and/or ieepa_rate based on country deals
   - Examples: UK caps, Brazil corrections, EU/Japan floors

7. Calculate final rate
   - rate = hts_rate + MAX(s232_rate, ieepa_rate) + emergency_rate + s301_rate
```

---

## VII. Country-Specific Treatments

### Summary Table

| Country/Group | MFN Treatment | S232 Treatment | IEEPA Treatment | Emergency | S301 | Weighted Avg Rate |
|---------------|---------------|----------------|-----------------|-----------|------|------------------|
| **Canada** | 0% (USMCA Layer 1) | Standard rates | 0% (USMCA Layer 2) | +35% on 413 products | None | 18.76% |
| **Mexico** | 0% (USMCA Layer 1) | Standard rates | 0% (USMCA Layer 2) | None | None | 14.11% |
| **China** | Standard MFN | Standard rates | 10% + 10% = 20% | +10% (opioid) | Variable (up to 100%) | 31.69% |
| **Japan** | Standard MFN | 15% floor (auto/MHDV) | max(floor, MFN) - MFN | None | None | 15.15% |
| **EU** | Standard MFN | 15% floor (auto/MHDV) | max(floor, MFN) - MFN | None | None | 12.93% |
| **UK** | Standard MFN | Caps: Steel/alu 25%, MHDV 10%; Auto 7.5-10% | Standard + exceptions | None | None | Variable by country |
| **Brazil** | Standard MFN | Standard rates | 10% + 40% = 50% + exceptions | None | None | 27.52% |
| **India** | Standard MFN | Standard rates | 10% + 40% = 50% (all products) | None | None | 50.00% |

### Detailed Country Notes

**Canada ($410.0B, 18.76% avg):**
- USMCA Layer 1: 0% MFN on all imports
- USMCA Layer 2: 0% IEEPA on all imports
- Exception: 413 products face +35% emergency rate (northern border security)
- S232 still applies (auto, MHDV with USMCA logic, metals)
- Net effect: Mix of 0% (USMCA-compliant non-S232), S232 rates, and 35% on border exceptions

**Mexico ($503.0B, 14.11% avg):**
- USMCA Layer 1: 0% MFN on all imports
- USMCA Layer 2: 0% IEEPA on all imports
- S232 still applies (auto, MHDV with USMCA logic, metals)
- No emergency rates or other exceptions
- Net effect: Mix of 0% (USMCA-compliant non-S232) and S232 rates

**China ($433.8B, 31.69% avg):**
- Standard MFN rates
- S232 rates apply (steel, aluminum, copper predominant)
- IEEPA: 10% baseline + 10% China rate = 20%
- Emergency: +10% opioid tariff (except Annex 2)
- S301: Variable rates up to 100% on 10,396 products
- All layers can stack (except S232 replaces IEEPA via precedence)
- Result: Highest average rate among major trading partners

**Japan ($146.9B, 15.15% avg):**
- Standard MFN rates
- S232: 15% floor on auto/MHDV (reduced from 25%)
- IEEPA: Tariff floor system (max of floor or MFN)
- Civil aircraft exceptions (WTO agreement): 0%
- No emergency or S301
- Floor system ensures minimum rates while preserving higher MFN where applicable

**EU ($600.8B, 12.93% avg - Germany/France/Italy):**
- Standard MFN rates
- S232: 15% floor on auto/MHDV (reduced from 25%)
- IEEPA: Tariff floor system (max of floor or MFN)
- Unconditional exceptions: $169.2B at 0% IEEPA
- Civil aircraft exceptions (WTO agreement): 0%
- No emergency or S301
- Significant unconditional exceptions reduce average rate

**UK ($67.4B):**
- Standard MFN rates
- S232 special treatment:
  - Auto: 7.5% (category), 10% (parts) - overrides standard 25%
  - Steel/aluminum: Capped at 25% (down from 50%)
  - MHDV/lumber: Capped at 10%
- IEEPA: Standard rates + exceptions
- Civil aircraft: 0% on both S232 and IEEPA
- Complex deal with multiple specific provisions

**Brazil ($42.2B, 27.52% avg):**
- Standard MFN rates
- S232: Standard rates
- IEEPA: 10% + 40% = 50% (additive to MFN)
- Exceptions:
  - Unconditional: Various products at 0% IEEPA
  - Civil aircraft: 90% assumed civil → 10% of tariff bracket applies
  - Passenger vehicles: 90% assumed passenger → 10% of tariff bracket applies
- Net effect: High base rate (50% IEEPA) with significant exceptions (45% of imports)

**India ($86.9B, 50.00% avg):**
- Standard MFN rates
- S232: Standard rates
- IEEPA: 10% + 40% = 50% on **ALL products** (no exceptions)
- Russian Oil Executive Order (blanket application)
- Result: Highest effective IEEPA rate, no product-level relief

#### 7.6 Korea (Type B - Multiple Exception Types)

**Treatment:** Multiple provisions modifying both S232 and IEEPA rates with staggered effective dates

**Value Affected:** [To be calculated after first run]

**Timeline:**
- **November 1, 2025:** Auto tariff floor takes effect
- **November 14, 2025:** Reciprocal rate floor, lumber S232, and civil aircraft exceptions take effect

**1. Auto Tariff Floor via S232 (November 1+):**

**ASSUMPTION #25: Korea Auto Tariff Floor**
- **Product Scope:** Automobiles only (`s232_auto == 1`)
- **Floor Rate:** 15% (total tariff minimum)
- **Implementation:** Floor applied through S232 component
- **Formula:** `s232_rate = max(15%, hts_rate) - hts_rate`
- **Rationale:** Same mechanism as Japan/EU auto/MHDV floor treatment, but Korea excludes MHDV
- **Similar to:** Japan (Section 7.4, lines ~1324-1331) and EU auto/MHDV S232 floors
- **Key Difference:** Korea applies to autos only; Japan/EU include MHDV
- **Code Reference:** us_tariff_calculation.R lines 1425-1437
- **Effective:** November 1, 2025

Formula explanation:
```
s232_rate = pmax(15, hts_rate) - hts_rate

Examples:
- hts_rate = 10% → s232_rate = 5%, total = 15%
- hts_rate = 20% → s232_rate = 0%, total = 20%
- hts_rate = 0% → s232_rate = 15%, total = 15%
```

- **Marker:** `kor_auto = 1`

**2. Lumber S232 Rate (November 14+):**

**ASSUMPTION #26: Korea Lumber Derivative Rate**
- **Product Scope:** Kitchen cabinets/vanities and upholstered furniture
  - `s232_lumber_derivative == 1`
  - `s232_upholstered_furniture == 1`
- **Treatment:** S232 rate set to 15%
- **Formula:** `s232_rate = 15`
- **Rationale:** Reduced from standard lumber derivative rates
- **Code Reference:** us_tariff_calculation.R lines 1442-1452
- **Similar to:** UK lumber treatment (Section 7.3)
- **Marker:** `kor_lumber_derivative = 1`

**3. Civil Aircraft Exception (November 14+):**

**ASSUMPTION #27: Korea Civil Aircraft Share**
- **Product Scope:** WTO civil aircraft list (`civil_aircraft_exceptions_wto.csv`, 554 codes)
- **Share Parameter:** `kor_civil_aircraft_share = 0.9` (90% civil use)
- **Treatment:** 90% exempt from IEEPA and material S232s (steel, alu, copper, lumber); 10% faces standard rates
- **Formula:** Applied in Section 8 via aircraft share weighting
  ```
  rate = hts + (1 - aircraft_share) × [materials_s232 + ieepa + emergency] + transport_s232 + s301

  With share = 0.9:
  rate = hts + 0.1 × [materials_s232 + ieepa + emergency] + transport_s232 + s301
  ```
- **Rationale:** WTO Civil Aircraft Agreement applies to products used in civil (not military) aircraft
- **Code Reference:** us_tariff_calculation.R lines 1454-1463
- **Similar to:** Brazil, UK, Japan, EU aircraft exceptions
- **Markers:** `kor_aircraft = 1`, `wto_aircraft = 1`

**4. Reciprocal Rate Floor (November 14+):**

**ASSUMPTION #28: Korea Reciprocal Rate Floor (After General Exceptions)**
- **Product Scope:** Products in reciprocal rate scope AFTER general exceptions are applied
  - Applies to: `un_code == korea_un_code` AND `kor_auto == 0` AND `rr_exception == 0` AND `ieepa_statute_exception == 0`
  - EXCLUDES: Annex 2 exceptions (`rr_exception == 1`)
  - EXCLUDES: Statutory IEEPA exceptions (`ieepa_statute_exception == 1`)
  - EXCLUDES: Autos with separate floor (`kor_auto == 1`)
- **Floor Rate:** 15% (total tariff minimum)
- **Implementation:** Floor applied through IEEPA component
- **Formula:** `ieepa_rate = max(15%, hts_rate) - hts_rate`
- **Key Difference from Japan/EU:** Korea does NOT have its own exception list; instead, benefits from general reciprocal rate exceptions (Annex 2, statutory IEEPA)
- **Code Reference:** us_tariff_calculation.R lines 1465-1479
- **Similar to:** Japan (7.4) and EU (7.5) tariff floors in mechanism, but Korea applies after general exceptions rather than having country-specific exception lists
- **Marker:** `kor_floor = 1`

Formula explanation:
```
ieepa_rate = pmax(15, hts_rate) - hts_rate

Examples:
- hts_rate = 10% → ieepa_rate = 5%, total = 15%
- hts_rate = 20% → ieepa_rate = 0%, total = 20%
- hts_rate = 5% → ieepa_rate = 10%, total = 15%
```

**General Reciprocal Rate Exceptions (Apply to Korea):**
- **Annex 2 Exceptions:** Products marked `rr_exception == 1` (unconditional exemption from reciprocal rates)
- **Statutory IEEPA Exceptions:** Products marked `ieepa_statute_exception == 1` (statutory exemption)
- These exceptions are applied in Section 4 of the code, before country-specific treatments

**Interaction Between Exceptions:**

For Korean products on/after November 14:
- Exception 1 applies ONLY to products subject to reciprocal rates after general exceptions
- Products with `rr_exception == 1` or `ieepa_statute_exception == 1` are exempt from both reciprocal rates AND the floor
- For autos: Exception 4 (auto floor via S232) applies; Exception 1 explicitly excludes autos (`kor_auto == 0` condition)

**Total Korea Exceptions:** [Value TBD] ([Percentage TBD] of Korea imports)
- Markers: `kor_floor`, `kor_lumber_derivative`, `kor_aircraft`, `kor_auto`

#### 7.7 Switzerland & Liechtenstein (Type B - Exceptions + Tariff Floor + Civil Aircraft + Pharma)

**Treatment:** Products from Switzerland (UN 756) and Liechtenstein (UN 438) receive four types of special treatment: unconditional product exceptions, civil aircraft exemption, non-patented pharmaceuticals exemption, and a tariff floor for other products.

**Countries:** Switzerland, Liechtenstein (treated as a customs union group)

**Effective Date:** November 14, 2025

**1. Unconditional Exceptions:**

**ASSUMPTION #31: Switzerland/Liechtenstein Unconditional Exceptions**
- **File:** `data/chli_exceptions.csv` (321 HS 8-digit codes)
- **Product Scope:** Specific products exempted from IEEPA tariffs
- **Treatment:** `ieepa_rate = 0` (complete IEEPA exemption)
- **Rationale:** Country-specific negotiated product exceptions
- **Code Reference:** us_tariff_calculation.R lines ~1559-1573
- **Similar to:** Brazil (`bra_exceptions.csv`) and EU (`eu_exceptions.csv`) unconditional exceptions
- **Marker:** `che_exception = 1`

**2. Civil Aircraft Exception:**

**ASSUMPTION #30: Switzerland/Liechtenstein Civil Aircraft Share**
- **Product Scope:** WTO civil aircraft list (`civil_aircraft_exceptions_wto.csv`, 554 codes)
- **Share Parameter:** `che_civil_aircraft_share = 0.9` (90% civil use)
- **Treatment:** 90% exempt from IEEPA and material S232s (steel, aluminum, copper, lumber); 10% faces standard rates
- **Formula:** Applied in Section 8 via aircraft share weighting
  ```
  rate = hts + (1 - aircraft_share) × [materials_s232 + ieepa + emergency] + transport_s232 + s301

  With share = 0.9:
  rate = hts + 0.1 × [materials_s232 + ieepa + emergency] + transport_s232 + s301
  ```
- **Rationale:** WTO Civil Aircraft Agreement applies to products used in civil (not military) aircraft
- **Code Reference:** us_tariff_calculation.R lines ~1577-1586
- **Similar to:** Brazil, UK, Japan, EU, Korea aircraft exceptions
- **Markers:** `che_aircraft = 1`, `wto_aircraft = 1`

**3. Non-Patented Pharmaceuticals Exception:**

**ASSUMPTION #32: Switzerland Non-Patented Pharma Share**
- **Product Scope:** Pharmaceutical products list (`pharma_exceptions.csv`, 519 codes)
- **Share Parameter:** `che_nonpatented_pharma_share = 0.2` (20% non-patented in US)
- **Treatment:** Share of pharma imports not patented in US is exempt from IEEPA reciprocal rates
- **Formula:** Applied directly to IEEPA rate in Section 7.7
  ```
  ieepa_rate = (1 - nonpatented_share) × ieepa_rate

  With share = 0.2 (20% non-patented):
  ieepa_rate = 0.8 × ieepa_rate
  ```
- **Rationale:** Products not under US patent protection receive reduced/eliminated reciprocal tariffs
- **Code Reference:** us_tariff_calculation.R lines ~1588-1605
- **Similar to:** EU pharma exception (`eu_nonpatented_pharma_share = 0.4`) conditional exception mechanism
- **Marker:** `che_pharma = 1`

**4. Reciprocal Rate Floor:**

**ASSUMPTION #29: Switzerland/Liechtenstein Tariff Floor**
- **Product Scope:** Products in reciprocal rate scope
  - Applies to: `un_code %in% swiss_liechtenstein` AND `rr_exception == 0` AND `ieepa_statute_exception == 0` AND `che_exception == 0` AND `che_pharma == 0`
  - EXCLUDES: Annex 2 exceptions (`rr_exception == 1`)
  - EXCLUDES: Statutory IEEPA exceptions (`ieepa_statute_exception == 1`)
  - EXCLUDES: Unconditional exceptions (`che_exception == 1`)
  - EXCLUDES: Pharma exceptions (`che_pharma == 1`)
- **Floor Rate:** 15% (total tariff minimum)
- **Implementation:** Floor applied through IEEPA component
- **Formula:** `ieepa_rate = max(15%, hts_rate) - hts_rate`
- **Rationale:** Ensures minimum 15% total tariff on Swiss/Liechtenstein imports subject to reciprocal rates
- **Code Reference:** us_tariff_calculation.R lines ~1607-1618
- **Similar to:** Korea (7.6) and Japan (7.4) tariff floors in mechanism
- **Marker:** `che_floor = 1`

Formula explanation:
```
ieepa_rate = pmax(15, hts_rate) - hts_rate

Examples:
- hts_rate = 10% → ieepa_rate = 5%, total = 15%
- hts_rate = 20% → ieepa_rate = 0%, total = 20%
- hts_rate = 5% → ieepa_rate = 10%, total = 15%
```

**Exceptions (General Reciprocal Rate Exceptions Also Apply):**
- **Annex 2 Exceptions:** Products marked `rr_exception == 1` (unconditional exemption from reciprocal rates)
- **Statutory IEEPA Exceptions:** Products marked `ieepa_statute_exception == 1` (statutory exemption)
- These exceptions are applied in Section 4 of the code, before country-specific treatments

**Summary:**
- Unconditional exceptions: Complete IEEPA exemption for 321 specific products (effective November 14, 2025)
- Civil aircraft: Full exemption from IEEPA and materials S232 (effective November 14, 2025)
- Non-patented pharmaceuticals: Share-weighted IEEPA exemption for 519 products (effective November 14, 2025)
- Floor rate applied to products in reciprocal rate scope (effective November 14, 2025)
- Annex 2, statutory IEEPA, unconditional, and pharma exceptions remain exempt from floor
- Total value affected: [To be calculated after first run]
- Markers: `che_exception`, `che_aircraft`, `che_pharma`, `che_floor`

---

## VIII. Technical Notes

### Calculation Performance

**Execution Time:** ~2-3 minutes on standard hardware
**Memory Usage:** Peak ~500 MB
**Data Volume:** 234,846 rows processed

### Data Quality

**Completeness:**
- 0 missing final rates (100% coverage)
- 173 missing MFN codes (3.4% of imports, documented)
- All 234,846 flows assigned rates

**Validation:**
- 0 negative rates
- 3 rates >200% (explainable: India 50% + China S301 60% + emergency 10% + high MFN)
- Weighted averages reconcile across all categories

### Rate Distribution

**Distribution of Final Rates:**

| Rate Range | Import Value | % of Total | Flows |
|------------|--------------|------------|-------|
| 0% | $1,029.6B | 31.8% | 31,317 |
| 0-10% | $120.2B | 3.7% | 34,618 |
| 10-20% | $779.4B | 24.1% | 110,260 |
| 20-30% | $825.9B | 25.5% | 30,944 |
| 30-50% | $399.8B | 12.3% | 26,638 |
| 50-100% | $84.6B | 2.6% | 1,057 |
| >100% | $0.4B | 0.0% | 12 |

**Interpretation:**
- Nearly one-third of imports face no tariffs beyond MFN (0% rate) - mostly USMCA partners on non-S232 products
- Just over half face moderate tariffs (10-30%) - typical IEEPA reciprocal rates
- Small portion faces very high rates (>50%) - primarily India and China with stacked tariffs

### Audit Trail

**Boolean Markers Provide Complete Audit Trail:**

Each of 20 boolean marker columns tracks which specific policies apply:
- s232_auto, s232_mhdv, s232_pv_parts
- s232_steel, s232_steel_derivative
- s232_alu, s232_alu_derivative
- s232_copper, s232_copper_derivative
- s232_lumber
- rr_exception
- missing_mfn
- bra_exception, bra_aircraft, bra_vehicle
- eu_exception
- uk_aircraft, uk_auto
- wto_aircraft
- chn_opioid
- sec301_tariff
- can_northern_border, mex_emergency

**Use:** Allows verification of why each product-country flow received its specific rate

### Transparency Features

1. **Separate rate columns:** Can see contribution of each policy instrument
2. **Boolean markers:** Can identify which policies apply
3. **Missing codes log:** Documents all MFN assumptions
4. **Versioned Annex 2:** Can reconstruct policy at different time points
5. **Centralized parameters:** All assumptions adjustable in Section 0

---


## IX. Glossary

**Ad Valorem:** Tariff expressed as percentage of product value (e.g., 25%)

**Annex 2:** List of products excepted from reciprocal tariff framework

**Composite Rate:** Tariff calculation for products with variable content shares (e.g., rate_s25, rate_s50 for 25%, 50% content)

**Derivative Product:** Product containing covered material but not purely that material (e.g., steel-containing machinery)

**HS Code:** Harmonized System product classification code (8-digit level used in analysis)

**IEEPA:** International Emergency Economic Powers Act (legal authority for reciprocal tariffs)

**MFN Rate:** Most Favored Nation rate from standard US tariff schedule

**Precedence:** Priority rules determining which tariff applies when multiple could apply

**Reciprocal Rate:** Tariff designed to match trading partner's tariff on US exports

**S232:** Section 232 of Trade Expansion Act (national security tariff authority)

**S301:** Section 301 of Trade Act (intellectual property-based tariffs on China)

**USMCA:** United States-Mexico-Canada Agreement (free trade agreement)

**Weighted Average:** Average rate weighted by import values (larger imports count more)

---

## Document Control

**Author:** Global Trade Alert / Analysis Team
**Script:** `us_tariff_calculation.R`
**Last Updated:** December 9, 2025

**For Questions or Clarifications:**
Contact analysis team with reference to specific section numbers and assumption numbers for efficient resolution.

---

**END OF METHODOLOGY DOCUMENT**
