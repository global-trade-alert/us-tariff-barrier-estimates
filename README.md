# US Tariff Barrier Estimates

Comprehensive calculation of US tariff rates applied to imports, incorporating MFN rates, Section 232 tariffs, IEEPA reciprocal tariffs, emergency measures, and Section 301 tariffs.

**Author:** Johannes Fritz
**Methodology:** [METHODOLOGY.md](METHODOLOGY.md)

## Overview

This tool calculates the effective tariff rate for every US import flow (product × country) under current US trade policy. It implements a layered tariff calculation that accounts for:

- **MFN Rates:** Base tariff rates from the US Harmonized Tariff Schedule
- **Section 232 Tariffs:** National security tariffs on automobiles, MHDV, steel, aluminium, copper, and lumber
- **IEEPA Reciprocal Tariffs:** Baseline 10% plus country-specific top-ups (e.g., 20% for China, 50% for India)
- **Emergency Tariffs:** Country-specific measures (China opioid, Canada northern border)
- **Section 301 Tariffs:** China-specific tariffs (up to 100% on certain products)
- **Country-Specific Exceptions:** Negotiated treatments for UK, EU, Japan, Korea, Switzerland, Brazil

## Quick Start

### Prerequisites

R (≥4.0) with the following packages:

```r
install.packages(c("data.table", "writexl", "dplyr"))
```

### Basic Usage

```bash
# From the repository root directory
cd us-tariff-barrier-estimates

# Run baseline calculation
Rscript code/us_tariff_calculation.R
```

Output files are saved to `results/`:
- `processed_us_imports_with_rates_*.RData` - Full dataset
- `processed_us_imports_with_rates_*.xlsx` - Excel workbook
- `processed_us_imports_with_rates_*.csv` - CSV for further processing

### Running Scenarios

The tool supports scenario analysis via configuration files in `data/scenarios/`:

```bash
# Run a specific scenario
SCENARIO_NAME=alternative Rscript code/us_tariff_calculation.R
```

See `code/run_scenario.R` for batch scenario execution.

## Repository Structure

```
us-tariff-barrier-estimates/
├── README.md               # This file
├── METHODOLOGY.md          # Full methodology documentation
├── LICENSE                 # CC BY 4.0 license
├── code/
│   ├── us_tariff_calculation.R      # Main calculation script
│   ├── run_scenario.R               # Scenario runner
│   ├── generate_scenario_report.R   # Report generator
│   └── date_config.R                # Date configuration
├── data/
│   ├── usitc_us_imports_2024.csv    # US import data (USITC)
│   ├── us_imports_2025.csv          # 2025 import combinations
│   ├── gta_jurisdiction_list.csv    # Country mappings
│   ├── compliance_shares.csv        # USMCA compliance rates
│   ├── sec301.csv                   # Section 301 tariff list
│   ├── tariff_floor_rates.csv       # Tariff floor parameters
│   └── scenarios/                   # Scenario configuration
│       ├── rates.csv                # Rate parameters
│       ├── shares.csv               # Share parameters
│       ├── countries.csv            # Country definitions
│       ├── country_groups.csv       # Country group definitions
│       ├── mfn_rates.csv            # MFN rate database
│       └── exceptions.csv           # Product scope definitions
└── results/                         # Output files (generated)
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SCENARIO_NAME` | `baseline` | Scenario to run |
| `POLICY_DATE` | `2026-01-17` | Policy effective date |
| `MFN_RATE_SOURCE` | `hts_schedule` | MFN rate source |
| `EXPORT_PROFILE` | `standard` | Output format |

### Scenario System

Scenarios modify parameters via CSV files:

- **`rates.csv`**: Override tariff rate parameters
- **`shares.csv`**: Override content/compliance shares
- **`exceptions.csv`**: Define product scopes and exceptions

Create a new scenario by adding rows to these files with `scenario = "your_scenario_name"`.

## Output Format

The main output includes:

| Column | Description |
|--------|-------------|
| `exporter` | Exporting country name |
| `iso_code` | ISO-3 country code |
| `hs_8digit` | 8-digit HS product code |
| `us_imports` | Import value (USD) |
| `hts_rate` | MFN base rate (%) |
| `s232_rate` | Section 232 rate (%) |
| `ieepa_rate` | IEEPA reciprocal rate (%) |
| `emergency_rate` | Emergency tariff rate (%) |
| `s301_rate` | Section 301 rate (%) |
| `rate` | **Final applied rate (%)** |

Boolean marker columns indicate which policies apply to each flow.

## Data Sources

- **US Import Data:** USITC DataWeb (2024 trade statistics)
- **MFN Rates:** US Harmonized Tariff Schedule (2025)
- **Tariff Policy:** Federal Register notices, Executive Orders
- **USMCA Compliance:** USITC DataWeb (July-September 2025)

## Citation

If you use this tool in your research, please cite:

```
Global Trade Alert (2025). US Tariff Barrier Estimates.
https://github.com/global-trade-alert/us-tariff-barrier-estimates
```

## Methodology

Full methodology documentation is in [METHODOLOGY.md](METHODOLOGY.md), including:

- Section-by-section calculation logic
- 32 numbered assumptions with rationale
- Country-specific treatment details
- Four-formula final rate calculation

## Contact

For questions or issues, please open a GitHub issue or contact the Global Trade Alert team.

## License

This work is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

You are free to share and adapt this material for any purpose, provided you give appropriate credit.
