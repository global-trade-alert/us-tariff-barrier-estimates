# Scorecard Scripts

This directory contains scripts for generating and distributing scorecards for the Trump Tariff Advantage project.

## Scripts

### `generate_scorecards.py`
Generates individual country scorecards (PNG and PDF) from processed data.

**Usage:**
```bash
# Generate all scorecards with default dates
python generate_scorecards.py --pdf

# Generate with specific dates
python generate_scorecards.py --pdf \
  --policy-date "1 August 2025" \
  --update-date "30 September 2025"

# Generate for specific countries only
python generate_scorecards.py --pdf \
  --countries "Brazil" "Mexico" "China"
```

**Arguments:**
- `--pdf` - Also generate PDF versions (default: PNG only)
- `--policy-date` - Policy effective date (default: "1 August 2025")
- `--update-date` - Data update date (default: today)
- `--countries` - List of countries to process (default: all)
- `--limit` - Limit number of countries for testing

### `copy_scorecards.py`
Copies scorecards for specific country groups, creates ZIP files, and uploads to S3.

**Usage:**
```bash
# Copy scorecards with default date (today)
python copy_scorecards.py c4tp
python copy_scorecards.py g20
python copy_scorecards.py world

# Copy with specific date
python copy_scorecards.py c4tp --date 250906
python copy_scorecards.py wb-all --date 250930
```

**Commands:**
- `c4tp` - C4TP member countries
- `g20` - G20 member countries
- `world` - All available scorecards
- `wb-all` - All World Bank regions
- `wb-eap` - East Asia & Pacific
- `wb-eca` - Europe & Central Asia
- `wb-lac` - Latin America & Caribbean
- `wb-mena` - Middle East & North Africa
- `wb-na` - North America
- `wb-sa` - South Asia
- `wb-ssa` - Sub-Saharan Africa
- `zip-all` - Create ZIPs for all existing folders

**Arguments:**
- `--date YYMMDD` - Scorecard date in YYMMDD format (default: today)

## Date Management

**All date parameters are now centralized in `update_processing.R`.**

When running the full pipeline, dates are automatically passed from the main script:
- Policy date (when tariffs took effect)
- Update date (when data was processed)

For standalone use, dates can be specified via command-line arguments or will default to today.

## Workflow

### Full Pipeline (Recommended)
Dates are managed centrally:
```r
# In update_processing.R
POLICY_DATE <- as.Date("2025-08-01")
UPDATE_DATE <- as.Date("2025-09-30")

# Scripts are called automatically with correct dates
```

### Manual Generation
```bash
# 1. Generate scorecards
python generate_scorecards.py --pdf \
  --policy-date "1 August 2025" \
  --update-date "30 September 2025"

# 2. Copy and upload for groups
python copy_scorecards.py c4tp --date 250930
python copy_scorecards.py g20 --date 250930
python copy_scorecards.py world --date 250930
```

## Output Structure

### Generated Scorecards
```
results/scorecards/
└── batch_YYMMDD-HHMM/
    ├── Country_Name_Scorecard.png
    ├── Country_Name_Scorecard.pdf
    └── index.csv
```

### Copied Groups
```
results/scorecards/
├── C4TP Scorecards YYMMDD/
├── C4TP Scorecards YYMMDD.zip
├── G20 Scorecards YYMMDD/
├── G20 Scorecards YYMMDD.zip
├── WORLD Scorecards YYMMDD/
└── WORLD Scorecards YYMMDD.zip
```

## S3 Upload

All ZIP files are automatically uploaded to S3 with stable URLs:
- `s3://bucket/reports/scorecards/C4TP-Scorecards.zip`
- `s3://bucket/reports/scorecards/G20-Scorecards.zip`
- `s3://bucket/reports/scorecards/WORLD-Scorecards.zip`

AWS credentials are loaded from `aws-keys.txt` in this directory.

## Dependencies

Install required packages:
```bash
pip install playwright reportlab pillow boto3
playwright install chromium
```

See `requirements.txt` for complete list.

## Country Groups

### C4TP Members
Albania, Azerbaijan, Bangladesh, Brazil, Cambodia, Chile, Colombia, Costa Rica, Dominican Republic, Egypt, El Salvador, Eswatini, Kazakhstan, Kosovo, Mexico, Mongolia, Morocco, Montenegro, Mozambique, Namibia, Nepal, North Macedonia, Pakistan, Panama, Peru, Philippines, Serbia, Singapore, Solomon Islands, South Africa, Thailand, Tunisia, UAE, Ukraine, Uruguay, Vietnam, Zambia

### G20 Members
Argentina, Australia, Brazil, Canada, China, France, Germany, India, Indonesia, Italy, Japan, Mexico, Saudi Arabia, South Africa, South Korea, Turkey, United Kingdom

### World Bank Regions
- East Asia & Pacific
- Europe & Central Asia
- Latin America & Caribbean
- Middle East & North Africa
- North America
- South Asia
- Sub-Saharan Africa

See `copy_scorecards.py` for complete country lists by region.