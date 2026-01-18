# USMCA Compliance Assumptions

**Source:** SGEPT US Tariff Barrier Estimates - USMCA Compliance Memo (January 2026)

## Overview

USMCA-compliant imports from Canada and Mexico receive preferential treatment (0% tariff for most products). The compliance share parameter determines what proportion of trade qualifies for this treatment. Analysis of USITC data reveals a structural shift in compliance behaviour in mid-2025, supporting product-level calibration.

## Key Finding

Observed USMCA compliance rates increased sharply in July 2025:

| Period | Canada | Mexico |
|--------|--------|--------|
| 2024 (full year) | 68.5% | 71.2% |
| 2025 H1 (Jan–Jun) | 72.3% | 73.8% |
| **Jul–Sep 2025** | **90.6%** | **89.0%** |

This shift likely reflects a behavioural response to the evolving tariff environment, where the economic incentive to certify USMCA origin has increased.

## Calibration Options

### Option A: Uniform Calibration
- Apply fixed 90% compliance to all products
- Simple but fails to capture sectoral outliers

### Option B: Sector-Level Carve-Outs
- Default 90% with specific lower rates for low-compliance sectors:

| HS Chapter | Description | Canada | Mexico |
|------------|-------------|--------|--------|
| Default | All other | 90% | 90% |
| 84 | Machinery, computers | 10% | 10% |
| 90 | Instruments | 10% | 10% |
| 95 | Toys and games | 90% | 10% |

### Option C: Product-Level Calibration (Adopted)
- Use specific compliance rates for each of 11,572 product-origin combinations
- Most accurate for effective tariff estimation

## Adoption Rationale

We adopt **Option C (product-level)** because:

1. Compliance behaviour observed in July–September 2025 is representative of the forward-looking steady state under current tariff policies
2. Product-level compliance is a more reliable predictor of effective tariff revenue than aggregate country-level averages
3. Significant trade value in machinery (HS 84) and instruments (HS 90) remains at low compliance (<15%), making uniform rates inaccurate

## Compliance Distribution

While aggregate compliance is high (~90%), the distribution is bimodal:

**Canada (Jul–Sep 2025)**
- High compliance (>90%): 4,249 products, 82.1% of trade value
- Low compliance (<10%): 495 products, 2.7% of trade value

**Mexico (Jul–Sep 2025)**
- High compliance (>90%): 3,605 products, 66.5% of trade value
- Low compliance (<10%): 466 products, 0.8% of trade value

## Low-Compliance Sectors

| HS | Description | Canada Compliance | Mexico Compliance |
|----|-------------|-------------------|-------------------|
| 90 | Optical/medical instruments | 8.2% | 9.1% |
| 84 | Machinery, computers | 11.5% | 12.3% |
| 71 | Precious metals/stones | 24.3% | 22.7% |
| 91 | Clocks and watches | 31.2% | 28.4% |
| 95 | Toys, games, sports | N/A | 14.8% |

## Data Source

| Parameter | Value |
|-----------|-------|
| Source | USITC DataWeb |
| Coverage | January 2024–September 2025 |
| Classification | HTS 10-digit |
| Statistics | Customs Value by programme |

Compliance calculated as: share of customs value entered under USMCA programme codes relative to total customs value for each product-origin pair.

## Configuration

Product-level compliance shares are stored in `data/compliance_shares.csv`:

```csv
hs_8digit,iso_code,compliance_share
01012100,CAN,0.95
01012100,MEX,0.92
...
```

The calculation applies compliance weighting in Section 8:
```
rate = compliance_share × usmca_rate + (1 - compliance_share) × non_usmca_rate
```
