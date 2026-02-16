# Professional Chart Book Generation System

This system generates high-quality PDF Chart Books for the "Relative Trump Tariff Advantage" analysis using R Markdown, LaTeX, and automated data processing.

## 🚀 Quick Start

```r
# Generate C4TP Chart Book
source("code/generate_chartbook.R")
generate_chartbook("c4tp", "C4TP_Chart_Book")
```

## 📁 Core Files

- **`simple_c4tp_template.Rmd`** - Main R Markdown template
- **`preamble.tex`** - LaTeX styling and configuration  
- **`generate_chartbook.R`** - Automated generation script
- **`custom.css`** - CSS styling for HTML output
- **`WORKFLOW.md`** - Comprehensive workflow documentation

## 📊 Generated PDF Structure

### Cover Page
- Three professional logos (GTA, SECO, C4TP)
- Dynamic title and author information
- Automated table of contents with hyperlinks

### Analysis Pages
1. **Effective Rates** - Trade-weighted tariff rates (top 20 countries)
2. **Relative Advantage** - Competitive position analysis (top 20 countries)  
3. **Hypothetical Revenue** - Estimated tariff revenue (top 20 countries)
4. **Individual Country Pages** - Top 10 products analysis per country

## 🛠 System Requirements

### Software
- **R (4.3+)** with packages: rmarkdown, readxl, ggplot2, scales, knitr, dplyr
- **MiKTeX** - LaTeX distribution for professional PDF output
- **Pandoc** - Document converter (included with RStudio)

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

## 🔧 Installation & Setup

### 1. Install R Packages
```r
required_packages <- c("rmarkdown", "readxl", "ggplot2", "scales", "knitr", "dplyr")
install.packages(required_packages)
```

### 2. Install MiKTeX (Windows)
```powershell
winget install --id MiKTeX.MiKTeX
```

### 3. Verify Installation
```r
source("code/generate_chartbook.R")
# Should show "Professional Chart Book Generation System"
```

## 📋 Workflow

### Standard Generation
1. **Prepare Data** - Ensure Excel files exist in `results/{group}/`
2. **Run Generation** - Execute `generate_chartbook("c4tp")`
3. **Review Output** - PDF created in `results/chart_books/`

### Manual Compilation (if needed)
```powershell
cd results\chart_books
Copy-Item "..\..\pics\*" "." -Force
& "C:\Users\[Username]\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe" -interaction=nonstopmode Chart_Book.tex
```

## 🎨 Customization

### Adding New Country Groups
```r
generate_chartbook("new_group", "Custom_Chart_Book")
```

### Modifying Styling
- **LaTeX:** Edit `preamble.tex`
- **Content:** Edit `simple_c4tp_template.Rmd`
- **Logos:** Replace files in `styling/` directory

### Template Modifications
- Update country lists in template
- Modify chart configurations
- Adjust text content and formatting

## 📈 Output Details

### Generated Files
- **Location:** `results/chart_books/`
- **Format:** Professional PDF with navigation
- **Size:** ~290KB (6 pages typical)

### Quality Features
- ✅ Professional LaTeX typesetting
- ✅ Custom headers and footers
- ✅ Hyperlinked navigation
- ✅ High-resolution graphics
- ✅ Consistent branding

## 🔍 Troubleshooting

### Common Issues
1. **Missing LaTeX** - Install MiKTeX
2. **Path Errors** - Check logo and data file locations
3. **Compilation Fails** - Verify all dependencies installed

### Debug Mode
Set `keep_tex = TRUE` in generation function to examine intermediate files.

## 📚 Documentation

- **`WORKFLOW.md`** - Detailed workflow documentation
- **Template Comments** - Inline documentation in R Markdown files
- **Error Messages** - Descriptive error reporting in generation script

## 🔄 Integration

This system integrates with the broader analysis pipeline:
1. **Data Processing** → Creates Excel analysis files
2. **Chart Book Generation** → Converts to professional PDFs
3. **Distribution** → Ready for publication/sharing

## 📞 Support

For technical issues:
1. Check `WORKFLOW.md` for detailed troubleshooting
2. Review R console error messages
3. Examine LaTeX log files if compilation fails
4. Verify all system requirements are met

---

**System Version:** 1.0  
**Last Updated:** August 2025  
**Compatible with:** R 4.3+, MiKTeX 25.4+