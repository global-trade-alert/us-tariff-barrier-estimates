# Professional Chart Book Generation Workflow

## Overview

This system generates professional PDF Chart Books for country group analysis using R Markdown, LaTeX, and automated data processing. The system is designed for the Global Trade Alert's "Relative Trump Tariff Advantage" analysis.

## System Components

### Core Files

1. **`simple_c4tp_template.Rmd`** - Main R Markdown template
2. **`preamble.tex`** - LaTeX styling and configuration  
3. **`generate_chartbook.R`** - Automated generation script
4. **`custom.css`** - CSS styling for HTML output

### Required Data Files

The system expects these Excel files in `results/{group_name}/`:
- `effective_rates_{group_name}.xlsx`
- `relative_advantage_{group_name}.xlsx`
- `hypothetical_tariff_revenue_{group_name}.xlsx`

### Logo Files

Required in `styling/` directory:
- `GTA_logo.png`
- `SECO_logo.png` 
- `C4TP_logo.png`

## Quick Start

### 1. Generate a Chart Book

```r
# Run from project root
source("code/generate_chartbook.R")
generate_chartbook("c4tp", "C4TP_Chart_Book")
```

### 2. Command Line Generation

```bash
cd "C:\Path\To\Project"
Rscript code/generate_chartbook.R
```

## Detailed Workflow

### Step 1: Data Preparation

Ensure your analysis has generated the required Excel files:
- Effective rates analysis
- Relative advantage calculations  
- Hypothetical tariff revenue estimates

### Step 2: Template Customization

Modify `simple_c4tp_template.Rmd` to:
- Update country group references
- Adjust chart titles and content
- Modify the country list for individual pages

### Step 3: PDF Generation

The generation process:
1. **R Markdown Processing** - Loads data, generates charts
2. **LaTeX Compilation** - Applies professional styling
3. **PDF Output** - Creates final professional document

### Step 4: Manual Compilation (if needed)

If automatic generation fails:

```powershell
# Navigate to output directory
cd results\chart_books

# Copy required files
Copy-Item "..\..\pics\*" "." -Force
Copy-Item "..\..\.markdown\preamble.tex" "." -Force

# Compile LaTeX manually
& "C:\Users\[Username]\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe" -interaction=nonstopmode Chart_Book.tex
```

## Output Structure

Generated PDF includes:
1. **Cover Page** - Title, logos, author information
2. **Table of Contents** - Hyperlinked navigation
3. **Aggregate Analysis** (3 pages):
   - Effective tariff rates chart
   - Relative advantage analysis
   - Hypothetical tariff revenue
4. **Individual Country Pages** - Top 10 products analysis per country

## Customization Options

### Adding New Country Groups

1. Create data files in `results/{new_group}/`
2. Call `generate_chartbook("new_group", "Custom_Name")`

### Modifying Styling

**LaTeX Styling:** Edit `preamble.tex`
- Headers and footers
- Font configuration
- Page layout
- Custom colors

**Content Layout:** Edit `simple_c4tp_template.Rmd`
- Chart configurations
- Table formatting
- Text content

### Logo Updates

Replace files in `styling/` directory maintaining:
- PNG format
- Reasonable resolution (recommended: 300 DPI)
- Transparent backgrounds preferred

## Troubleshooting

### Common Issues

**LaTeX Compilation Errors:**
- Ensure MiKTeX is installed and in PATH
- Check all logo files exist
- Verify preamble.tex syntax

**Missing Data Files:**
- Confirm Excel files exist in correct location
- Check file naming conventions
- Verify data structure matches template expectations

**Path Issues:**
- Use absolute paths for MiKTeX if needed
- Ensure pandoc is accessible
- Check working directory settings

### Debug Mode

For troubleshooting, set `keep_tex = TRUE` in the render function to examine intermediate LaTeX files.

## System Requirements

### Software Dependencies

- **R (4.3+)** with packages: rmarkdown, readxl, ggplot2, scales, knitr, dplyr
- **MiKTeX** - LaTeX distribution for Windows
- **Pandoc** - Document converter (included with RStudio)

### Hardware Requirements

- Windows 10/11
- 4GB RAM minimum
- 2GB free disk space for LaTeX installation

## Advanced Features

### Batch Generation

Generate multiple country groups:

```r
groups <- c("c4tp", "ldc", "developed")
for (group in groups) {
  generate_chartbook(group, paste0(toupper(group), "_Chart_Book"))
}
```

### Custom Templates

Create specialized templates by:
1. Copying `simple_c4tp_template.Rmd`
2. Modifying content structure
3. Updating generation function to use new template

## Maintenance

### Regular Updates

- Update country lists as analysis expands
- Refresh logo files as needed
- Update styling to match organizational changes

### Version Control

- Template files should be version controlled
- Generated PDFs can be excluded from git
- Document changes in this workflow file

## Support

For technical issues:
1. Check this workflow documentation
2. Review error messages in R console
3. Examine LaTeX log files for compilation errors
4. Verify all system requirements are met

---

**Last Updated:** August 2025  
**System Version:** 1.0  
**Compatible with:** R 4.3+, MiKTeX 25.4+