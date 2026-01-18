# MFN Rate Estimation Options

**Source:** SGEPT US Tariff Barrier Estimates - Effective 2024 Rate Estimates (January 2026)

## Overview

Four empirical MFN rate estimates are available for each HS 8-digit product, calculated from 2024 USITC trade data. The choice of rate source affects aggregate tariff calculations.

## Rate Options

| Estimate | Level | Methodology | Trade-Weighted Rate |
|----------|-------|-------------|---------------------|
| **HTS Schedule** | Product | Statutory HTS rate | 2.1% |
| **Mixed Schedule** | Product | Mode-based with fallbacks | 2.1% |
| **Observed Bilateral** | Product × Origin | Duties collected / customs value | 2.3% |
| **Observed Product** | Product | Trade-weighted average across origins | 2.3% |

## Methodology

### Data Source

| Parameter | Value |
|-----------|-------|
| Source | USITC DataWeb monthly imports |
| Period | January–December 2024 |
| Observations | 1,533,107 (HS × Country × Month × Programme) |
| Trade coverage | $3,251.2 billion |
| Minimum value filter | $1,000 customs value |

### Rate Calculation

For each observation:
```
effective_rate = (calculated_duties / customs_value) × 100
```

### Mixed Schedule Selection Logic

The "Mixed" rate uses the following algorithm:
1. **Use Mode** if |gap_mode| < 5 pp AND product is not in S232 scope
2. **Use MFN Schedule** if standard deviation > 5 pp AND product is not in S232 scope
3. **Use Median** otherwise

## Validation

91% of products (by count) and 97% of trade (by value) show empirical rates within ±1 percentage point of the scheduled MFN rate. The remaining discrepancies arise from:

- Preferential programme utilisation (USMCA, GSP, FTAs)
- Non-ad valorem duty conversions
- Trade defence measures (AD/CVD)
- S232 tariffs (steel/aluminium in 2024)

## Recommendations

| Use Case | Recommended Source |
|----------|-------------------|
| **Policy simulation** | `hts_schedule` (statutory baseline) |
| **Aggregate calibration** | `observed_product` (actual 2024 patterns) |
| **Origin-differentiated analysis** | `observed_bilateral` (includes FTA preferences) |
| **Non-ad valorem approximation** | `mixed_schedule` (mode-based conversion) |

## Configuration

Set the MFN rate source via environment variable:
```bash
MFN_RATE_SOURCE=hts_schedule Rscript code/us_tariff_calculation.R
```

Available values: `hts_schedule` (default), `mixed_schedule`, `observed_bilateral`, `observed_product`
