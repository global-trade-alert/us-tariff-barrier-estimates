# Metal Content Ratios for Section 232 Products

**Source:** SGEPT US Tariff Barrier Estimates - Metal Ratio Assumptions (January 2026)

## Overview

Section 232 tariffs apply to metal-containing products, but the effective incidence depends on metal content. For derivative products (machinery containing steel, etc.), IEEPA tariffs complement rather than add to S232 tariffs. The content share parameter determines the split between S232 and IEEPA rate application.

## Recommended Ratios

| Metal | Derivatives | Main Products | Uncertainty |
|-------|-------------|---------------|-------------|
| **Steel** | 40% | 85% | ±15pp |
| **Aluminium** | 35% | 94% | ±10pp |
| **Copper** | N/A | 70% | Provisional |

## Estimation Methodology

### Data Source

| Parameter | Value |
|-----------|-------|
| Source | USITC DataWeb |
| Coverage | January–September 2025 |
| Trade flow | US Imports (consumption) |
| Statistics | Customs Value, Calculated Duties |
| Products | 810 steel, 151 aluminium, 57 copper codes |

### Estimation Formula

Effective incidence ratios are estimated by comparing observed tariff rates against theoretical rates at 100% metal content:

```
Incidence Ratio = (Observed Rate − Baseline Non-S232 Rate) / Statutory S232 Rate
```

Where the Baseline Non-S232 Rate = HTS + IEEPA + Emergency + S301 (theoretical tariff excluding S232).

### Interpretation

- **Ratio = 100%**: Full statutory S232 rate collected (no exclusions)
- **Ratio = 50%**: Half of statutory rate collected (exclusions or mixed content)
- **Ratio < 100% for main products**: Reflects exclusion utilisation

## Trade-Weighted Results

| Metal | Product Type | Trade Value | Mean Ratio | Median |
|-------|--------------|-------------|------------|--------|
| Steel | Derivatives | $47.7B | 40% | 45% |
| Steel | Main Products | $21.2B | 85% | 92% |
| Aluminium | Derivatives | $46.4B | 35% | 34% |
| Aluminium | Main Products | $9.8B | 94% | 98% |
| Copper | Main Products | $0.7B | 70% | 79% |

## Temporal Stability

Steel derivative ratios exhibited significant instability during 2025:
- **Jan–Feb**: ~2–3% (rate transition period)
- **Apr–May**: ~65–72% (peak compliance)
- **Aug–Sep**: ~25–32% (stabilised)

Aluminium ratios were stable throughout (35% derivatives, 94% main).

Copper ratios are provisional (August–September 2025 data only).

## Calibration Rationale

**Derivatives**: Full-period trade-weighted averages (40% steel, 35% aluminium) capture the structural relationship between derivative composition and effective tariff incidence, smoothing over transitional volatility.

**Main products**: Observed ratios (85% steel, 94% aluminium) rather than theoretical 100%. Main products are physically 100% metal, but observed ratios reflect the current exclusion regime. Since estimates aim to capture actual trade costs under prevailing policy, we incorporate effective incidence of exclusions.

**Copper**: 70% is provisional (two months of data). Applied to main products only as no copper derivatives are currently within S232 scope.

## Configuration

Content share parameters in `data/scenarios/shares.csv`:

```csv
share_type,share,scenario,section,description
steel_content_share,0.85,baseline,3.4,Steel main product incidence
steel_derivative_content_share,0.40,baseline,3.4,Steel derivative incidence
alu_content_share,0.94,baseline,3.5,Aluminium main product incidence
alu_derivative_content_share,0.35,baseline,3.5,Aluminium derivative incidence
copper_content_share,0.70,baseline,3.6,Copper main product incidence
```
