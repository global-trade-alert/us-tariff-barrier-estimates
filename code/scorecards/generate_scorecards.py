import argparse
import math
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd
from playwright.sync_api import sync_playwright


def format_currency_short(value_usd: float) -> str:
    if pd.isna(value_usd) or value_usd is None:
        return "$0"
    abs_val = abs(value_usd)
    if abs_val >= 1e9:
        return f"${value_usd / 1e9:.1f}B"
    if abs_val >= 1e6:
        return f"${value_usd / 1e6:.0f}M"
    if abs_val >= 1e3:
        return f"${value_usd / 1e3:.0f}K"
    return f"${value_usd:.0f}"


def format_percent(value: float, digits: int = 1, include_sign: bool = False) -> str:
    if pd.isna(value) or value is None:
        return "0%"
    sign = "+" if include_sign and value > 0 else ("" if value >= 0 else "-")
    v = abs(value) if include_sign and value < 0 else value
    return f"{sign}{v:.{digits}f}%"


def compute_effective_rate(row: pd.Series) -> float:
    # rates in the dataset are percentages (e.g., 10 for 10%)
    rate = row.get("rate")
    return float(rate) if not pd.isna(rate) else 0.0


def load_processed_dataset(root: Path) -> pd.DataFrame:
    excel_path = root / "results" / "processed_us_imports_with_rates.xlsx"
    if not excel_path.exists():
        raise FileNotFoundError(
            f"Expected dataset not found: {excel_path}. Please run the R processing or provide the Excel export."
        )
    df = pd.read_excel(excel_path, sheet_name=0)
    # Normalize column names
    df.columns = [c.strip() for c in df.columns]
    required_cols = [
        "exporter",
        "hs_8digit",
        "hs_8digit_name",
        "us_imports",
        "rate",
        
    ]
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        raise ValueError(f"Dataset is missing required columns: {', '.join(missing)}")
    df["effective_rate"] = df.apply(compute_effective_rate, axis=1)
    # Ensure numeric
    df["us_imports"] = pd.to_numeric(df["us_imports"], errors="coerce").fillna(0.0)
    df["effective_rate"] = pd.to_numeric(df["effective_rate"], errors="coerce").fillna(0.0)
    return df


def load_reciprocal_rates(root: Path) -> pd.DataFrame:
    """Load reciprocal country-specific topup rates"""
    csv_path = root / "data" / "reciprocal_country_specific_topup.csv"
    if not csv_path.exists():
        raise FileNotFoundError(f"Reciprocal rates file not found: {csv_path}")
    
    df = pd.read_csv(csv_path, sep=";")
    df.columns = [c.strip() for c in df.columns]
    return df


def compute_country_stats(df_all: pd.DataFrame, country: str, reciprocal_rates: pd.DataFrame) -> Dict:
    df_cty = df_all[df_all["exporter"] == country].copy()
    if df_cty.empty:
        raise ValueError(f"No data found for country: {country}")

    total_imports = float(df_cty["us_imports"].sum())
    # Weighted effective rate (rates are in percent units)
    if total_imports <= 0:
        effective_rate = 0.0
    else:
        effective_rate = float((df_cty["effective_rate"] * df_cty["us_imports"]).sum() / total_imports)

    # Headline rate rules
    # 1) Brazil hard-coded to 50%
    # 2) EU and EU member states display "15% floor"
    # 3) Otherwise: 10% baseline + country-specific topup
    country_topup = reciprocal_rates[reciprocal_rates["exporter"] == country]
    unit_text = (
        str(country_topup["unit"].iloc[0]).lower() if not country_topup.empty and "unit" in country_topup.columns else ""
    )

    eu_members = {
        "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia (Czech Republic)",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
        "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
        "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
        "European Union", "EU"
    }

    # Exception countries that get 0% tariff rates
    # Note: Russia, Belarus, Cuba, North Korea have been removed from the dataset completely
    exception_countries = {
        "Canada", "Mexico"
    }

    is_eu_floor = (country in eu_members) or ("tariff floor of 15%" in unit_text)

    if country in exception_countries:
        headline_rate_value = 0.0
        headline_rate_html = None
    elif country == "Brazil":
        headline_rate_value = 50.0
        headline_rate_html = None
    elif is_eu_floor:
        headline_rate_value = 15.0
        # Display with visible 'floor' label on a second line (Roboto Light)
        headline_rate_html = (
            "15%<br>"
            "<span style=\"display:block; margin-top:2px; font-family:'Roboto', sans-serif; font-weight:300;"
            " line-height:1.2; font-size:14px; color:#0F599D;\">floor</span>"
        )
    else:
        if not country_topup.empty:
            topup_rate = float(country_topup["country_rate_aug1"].iloc[0])
            if pd.isna(topup_rate):
                topup_rate = 0.0
        else:
            topup_rate = 0.0
        headline_rate_value = 10.0 + topup_rate
        headline_rate_html = None

    implied_revenue = float(total_imports * (effective_rate / 100.0))

    # Per-HS competitor-weighted average rates for advantage calculation
    # Compute per-HS world totals excluding focal country
    df_world = df_all.copy()
    # For each HS, competitor weighted avg effective rate
    comp_avgs = (
        df_world[df_world["exporter"] != country]
        .groupby("hs_8digit")
        .apply(lambda g: (g["effective_rate"] * g["us_imports"]).sum() / max(g["us_imports"].sum(), 1e-12))
        .rename("competitor_avg_rate")
        .reset_index()
    )

    df_cty = df_cty.merge(comp_avgs, on="hs_8digit", how="left")
    df_cty["competitor_avg_rate"] = df_cty["competitor_avg_rate"].fillna(df_cty["effective_rate"])  # if no competitors
    df_cty["advantage_rate"] = df_cty["competitor_avg_rate"] - df_cty["effective_rate"]

    # Country-level advantage: trade-weighted avg of per-HS advantage by this country's import weights
    if total_imports <= 0:
        relative_advantage = 0.0
    else:
        relative_advantage = float((df_cty["advantage_rate"] * df_cty["us_imports"]).sum() / total_imports)

    # Top products by import value (display only)
    # Exclude HS codes larger than 98000000 from display, but keep all data for calculations above
    df_display = df_cty.copy()
    df_display["hs_int"] = pd.to_numeric(df_display["hs_8digit"], errors="coerce")
    df_display = df_display[df_display["hs_int"].notna()]
    df_display = df_display[df_display["hs_int"] <= 98_000_000]

    top_source = df_display if not df_display.empty else df_cty
    top = (
        top_source.sort_values("us_imports", ascending=False)
        .head(5)
        .loc[:, ["hs_8digit", "hs_8digit_name", "us_imports", "effective_rate", "advantage_rate"]]
        .copy()
    )

    def format_hs_code(hs_code: str) -> str:
        """Format HS code with dots: 1234.56.78"""
        hs_padded = str(hs_code).zfill(8)
        return f"{hs_padded[:4]}.{hs_padded[4:6]}.{hs_padded[6:8]}"

    top_products: List[Dict] = []
    for _, r in top.iterrows():
        top_products.append(
            {
                "hs_code": format_hs_code(str(r["hs_8digit"])),
                "product": str(r["hs_8digit_name"]).strip(),
                "trade_value": format_currency_short(float(r["us_imports"])),
                "rate": format_percent(float(r["effective_rate"]), digits=0, include_sign=False),
                "advantage": format_percent(float(r["advantage_rate"]), digits=1, include_sign=True),
                "advantage_raw": float(r["advantage_rate"]),
                "revenue": format_currency_short(float(r["us_imports"]) * (float(r["effective_rate"]) / 100.0)),
            }
        )

    stats = {
        "country_name": country.upper(),
        "headline_rate": format_percent(headline_rate_value, digits=0),
        "headline_rate_html": headline_rate_html,
        "effective_rate": format_percent(effective_rate, digits=1),
        "total_trade": format_currency_short(total_imports),
        "implied_revenue": format_currency_short(implied_revenue),
        "relative_advantage": format_percent(relative_advantage, digits=1, include_sign=True),
        "relative_advantage_raw": relative_advantage,
        "advantage_class": "positive" if relative_advantage >= 0 else "negative",
        "products": top_products,
    }
    return stats


def render_scorecard(
    template_path: Path,
    header_icon_path: Path,
    footer_logo_path: Path,
    out_png_path: Path,
    out_pdf_path: Optional[Path],
    stats: Dict,
    policy_date: str = "1 August 2025",
    update_date: Optional[str] = None,
):
    out_png_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Set default update date to today if not provided
    if update_date is None:
        update_date = datetime.now().strftime("%d %B %Y")

    with sync_playwright() as p:
        browser = p.chromium.launch()
        # Use a browser context with an explicit device scale factor for high-DPI rendering
        context = browser.new_context(viewport={"width": 800, "height": 1200}, device_scale_factor=4)
        page = context.new_page()
        page.goto(template_path.as_uri(), wait_until="networkidle")
        # Ensure print uses screen styles and backgrounds
        page.emulate_media(media="screen")

        # Inject data
        page.evaluate(
            """
(statsData) => {
  const setText = (selector, text) => {
    const el = document.querySelector(selector);
    if (el) el.textContent = String(text);
  };

  const setByLabel = (labelText, valueText) => {
    const boxes = document.querySelectorAll('.metric-box');
    for (const box of boxes) {
      const lab = box.querySelector('.metric-label');
      const val = box.querySelector('.metric-value');
      if (lab && val && lab.textContent.trim().toLowerCase() === labelText.toLowerCase()) {
        val.textContent = String(valueText);
        return true;
      }
    }
    return false;
  };

  // Country name
  setText('.country-name', statsData.country_name);

  // Title: US -> U.S.
  const mainTitle = document.querySelector('.main-title');
  if (mainTitle) {
    mainTitle.textContent = 'U.S. Market Access Scorecard';
  }

  // Metrics
  if (statsData.headline_rate_html) {
    const boxes = document.querySelectorAll('.metric-box');
    for (const box of boxes) {
      const lab = box.querySelector('.metric-label');
      const val = box.querySelector('.metric-value');
      if (lab && val && lab.textContent.trim().toLowerCase() === 'amended reciprocal rate'.toLowerCase()) {
        val.innerHTML = statsData.headline_rate_html;
        break;
      }
    }
  } else {
    setByLabel('Amended Reciprocal Rate', statsData.headline_rate);
  }
  setByLabel('Effective Rate incl. MFN + Sectorals', statsData.effective_rate);
  setByLabel('Total Trade 2024', statsData.total_trade);
  // Replace revenue label and value
  (function setRevenueMetric(){
    const boxes = document.querySelectorAll('.metric-box');
    for (const box of boxes) {
      const lab = box.querySelector('.metric-label');
      const val = box.querySelector('.metric-value');
      if (!lab || !val) continue;
      const txt = lab.textContent.trim().toLowerCase();
      if (txt === 'implied revenue' || txt === 'hypothetical revenue w/ 2024 trade') {
        lab.textContent = 'Hypothetical Revenue w/ 2024 Trade';
        val.textContent = String(statsData.implied_revenue);
        break;
      }
    }
  })();

  // Advantage metric: set value and class
  const advantageBox = document.querySelector('.metric-box.advantage');
  if (advantageBox) {
    const v = advantageBox.querySelector('.metric-value');
    if (v) v.textContent = String(statsData.relative_advantage);
    advantageBox.classList.remove('positive', 'negative');
    advantageBox.classList.add(statsData.advantage_class);
  }

  // Update subtitle with policy date
  const subtitle = document.querySelector('.subtitle');
  if (subtitle) {
    subtitle.textContent = `As of ${statsData.__policy_date}`;
  }

  // Update date stamp with update date
  const dateStamp = document.querySelector('.date-stamp');
  if (dateStamp) {
    dateStamp.textContent = `Generated on ${statsData.__update_date}`;
  }

  // Replace header icon and footer logo with provided images
  const headerIcon = document.querySelector('.header-logo');
  if (headerIcon) {
    headerIcon.innerHTML = '';
    headerIcon.style.position = 'absolute';
    headerIcon.style.top = '50%';
    headerIcon.style.right = '20px';  // Moved slightly inwards from 28px
    headerIcon.style.transform = 'translateY(-50%)';  // Center vertically
    headerIcon.style.width = '65px';   // Increased size
    headerIcon.style.height = '65px';  // Increased size
    
    const link = document.createElement('a');
    link.href = 'https://globaltradealert.org';
    link.target = '_blank';
    link.style.display = 'block';
    link.style.width = '100%';
    link.style.height = '100%';
    
    const img = document.createElement('img');
    img.src = statsData.__header_icon_url;
    img.style.width = '100%';
    img.style.height = '100%';
    img.style.objectFit = 'contain';
    img.style.opacity = '0.95';
    
    link.appendChild(img);
    headerIcon.appendChild(link);
  }

  const footerLogo = document.querySelector('.logo-icon');
  if (footerLogo) {
    footerLogo.innerHTML = '';
    footerLogo.style.display = 'flex';
    footerLogo.style.alignItems = 'center';
    footerLogo.style.justifyContent = 'center';
    footerLogo.style.height = '100%';
    footerLogo.style.width = '160px';
    footerLogo.style.minHeight = '72px';
    
    const link = document.createElement('a');
    link.href = 'https://globaltradealert.org';
    link.target = '_blank';
    link.style.display = 'block';
    
    const img = document.createElement('img');
    img.src = statsData.__footer_logo_url;
    img.style.maxWidth = '100%';
    img.style.maxHeight = '48px';   // Visible height within footer on PNG
    img.style.objectFit = 'contain';
    img.style.display = 'block';
    
    link.appendChild(img);
    footerLogo.appendChild(link);
  }

  // Explicitly hide the textual URL under the icon (should not be shown)
  const logoText = document.querySelector('.logo-text');
  if (logoText) {
    logoText.style.display = 'none';
  }

  // Remove rounded corners from scorecard for PNG (only useful with transparent backgrounds)
  const scorecard = document.querySelector('.scorecard');
  if (scorecard) {
    scorecard.style.borderRadius = '0';
  }

  // Compact layout slightly to reduce overall height (for mobile)
  (function compactLayout(){
    const header = document.querySelector('.header');
    if (header) header.style.padding = '18px 24px';
    const metrics = document.querySelectorAll('.metric-box');
    metrics.forEach(b => { b.style.padding = '18px 14px'; });
    const section = document.querySelector('.products-section');
    if (section) section.style.padding = '22px 24px 18px';
    const tableCells = document.querySelectorAll('.products-table td');
    tableCells.forEach(td => { td.style.padding = '10px 8px'; td.style.fontSize = '12px'; });
    const footer = document.querySelector('.footer');
    if (footer) footer.style.padding = '12px 24px 14px';
  })();

  // Build products table
  const tbody = document.querySelector('.products-table tbody');
  if (tbody) {
    tbody.innerHTML = '';
    for (const pr of statsData.products) {
      const tr = document.createElement('tr');

      const tdHs = document.createElement('td');
      const spanHs = document.createElement('span');
      spanHs.className = 'hs-code';
      spanHs.textContent = pr.hs_code;
      tdHs.appendChild(spanHs);
      tr.appendChild(tdHs);

      const tdName = document.createElement('td');
      tdName.className = 'product-name';
      tdName.textContent = pr.product;
      tr.appendChild(tdName);

      const tdTrade = document.createElement('td');
      tdTrade.className = 'trade-value';
      tdTrade.textContent = pr.trade_value;
      tr.appendChild(tdTrade);

      const tdRate = document.createElement('td');
      tdRate.className = 'rate-value';
      tdRate.textContent = pr.rate;
      tr.appendChild(tdRate);

      const tdAdv = document.createElement('td');
      const spanAdv = document.createElement('span');
      spanAdv.className = 'advantage-value ' + (pr.advantage_raw >= 0 ? 'positive' : 'negative');
      spanAdv.textContent = pr.advantage;
      tdAdv.appendChild(spanAdv);
      tr.appendChild(tdAdv);

      const tdRev = document.createElement('td');
      tdRev.className = 'revenue-value';
      tdRev.textContent = pr.revenue;
      tr.appendChild(tdRev);

      tbody.appendChild(tr);
    }
  }
}
            """,
            {
                **stats,
                "__header_icon_url": header_icon_path.as_uri(),
                "__footer_logo_url": footer_logo_path.as_uri(),
                "__policy_date": policy_date,
                "__update_date": update_date,
            },
        )

        # Ensure fonts and gradients finish rendering
        page.wait_for_timeout(800)

        # Screenshot only the card
        card = page.query_selector(".scorecard")
        if card is None:
            # fallback: full page
            page.screenshot(path=str(out_png_path), full_page=True)
        else:
            card.screenshot(path=str(out_png_path))

        # Optional PDF export using screenshot-to-PDF approach for guaranteed single page
        if out_pdf_path is not None:
            try:
                # Get card dimensions and footer logo position for clickable area
                metrics = page.evaluate("""
                () => {
                  const card = document.querySelector('.scorecard');
                  if (!card) return null;
                  
                   // Ensure footer logo is wrapped in a link
                  const footerLogo = document.querySelector('.logo-icon');
                  let logoRect = null;
                  if (footerLogo) {
                    let link = footerLogo.querySelector('a');
                    if (!link) {
                      link = document.createElement('a');
                      while (footerLogo.firstChild) link.appendChild(footerLogo.firstChild);
                      footerLogo.appendChild(link);
                    }
                    link.href = 'https://globaltradealert.org';
                    link.target = '_blank';
                    link.rel = 'noopener noreferrer';
                    
                    // Get logo position relative to card
                    const cardRect = card.getBoundingClientRect();
                     const logoElementRect = (link || footerLogo).getBoundingClientRect();
                    logoRect = {
                      x: Math.round(logoElementRect.left - cardRect.left),
                      y: Math.round(logoElementRect.top - cardRect.top),
                      width: Math.round(logoElementRect.width),
                      height: Math.round(logoElementRect.height)
                    };
                  }

                  // Footer CTA text link area
                  let ctaRect = null;
                  const ctaLink = document.querySelector('.footer-cta .cta-link');
                  if (ctaLink) {
                    const cardRect = card.getBoundingClientRect();
                    const cRect = ctaLink.getBoundingClientRect();
                    ctaRect = {
                      x: Math.round(cRect.left - cardRect.left),
                      y: Math.round(cRect.top - cardRect.top),
                      width: Math.round(cRect.width),
                      height: Math.round(cRect.height)
                    };
                  }

                  const r = card.getBoundingClientRect();
                  return { 
                    width: Math.ceil(r.width), 
                    height: Math.ceil(r.height),
                    logoRect,
                    ctaRect,
                    dpr: (window.devicePixelRatio || 1)
                  };
                }
                """)

                if metrics is None:
                    print("Warning: Could not find scorecard element for PDF")
                    return

                # Take high-DPI screenshot of just the card element (Strategy 3)
                import tempfile
                card = page.query_selector(".scorecard")
                temp_png = tempfile.NamedTemporaryFile(suffix='.png', delete=False)
                temp_png.close()

                # High-resolution PNG snapshot of the card
                card.screenshot(path=temp_png.name)

                # Convert PNG to a high-resolution PDF and add link overlays
                try:
                    from reportlab.pdfgen import canvas
                    
                    # Compute page size in points based on desired 300 DPI target.
                    # Derive pixel dimensions from CSS size and output device pixel ratio.
                    target_dpi = 300.0
                    dpr = float(metrics.get('dpr', 1))
                    width_px = float(metrics['width']) * dpr
                    height_px = float(metrics['height']) * dpr
                    width_pts = width_px * 72.0 / target_dpi
                    height_pts = height_px * 72.0 / target_dpi
                    
                    c = canvas.Canvas(str(out_pdf_path), pagesize=(width_pts, height_pts))
                    
                    # Draw the screenshot image to fill the page
                    c.drawImage(temp_png.name, 0, 0, width=width_pts, height=height_pts)
                    
                    # Helper to map CSS-pixel rects to PDF points using ratios
                    def rect_css_to_pts(r: dict) -> Tuple[float, float, float, float]:
                        css_w = max(1, float(metrics['width']))
                        css_h = max(1, float(metrics['height']))
                        x1 = (float(r['x']) / css_w) * width_pts
                        y1 = height_pts - ((float(r['y']) + float(r['height'])) / css_h) * height_pts
                        x2 = ((float(r['x']) + float(r['width'])) / css_w) * width_pts
                        y2 = height_pts - (float(r['y']) / css_h) * height_pts
                        return x1, y1, x2, y2

                    # Add clickable link over footer logo area
                    if metrics.get('logoRect'):
                        x1_pts, y1_pts, x2_pts, y2_pts = rect_css_to_pts(metrics['logoRect'])
                        
                        c.linkURL('https://globaltradealert.org', 
                                (x1_pts, y1_pts, x2_pts, y2_pts), 
                                relative=0)

                    # Add clickable link over footer CTA text area
                    if metrics.get('ctaRect'):
                        x1_pts, y1_pts, x2_pts, y2_pts = rect_css_to_pts(metrics['ctaRect'])
                        c.linkURL('https://globaltradealert.org/reports/Relative-Trump-Tariff-Advantage-Chart-Book',
                                  (x1_pts, y1_pts, x2_pts, y2_pts),
                                  relative=0)

                    # Removed logo-text overlay: textual link under icon is not displayed
                    
                    c.showPage()
                    c.save()
                    
                finally:
                    # Clean up temporary file
                    import os
                    try:
                        os.unlink(temp_png.name)
                    except:
                        pass
                        
            except ImportError as imp_err:
                # Clarify which dependency is missing (reportlab or Pillow)
                print(f"Warning: PDF generation skipped due to missing dependency: {imp_err}."
                      f" Please install 'reportlab' and 'Pillow'.")
            except Exception as pdf_err:
                print(f"PDF generation error: {pdf_err}")

        # Cleanly close context and browser
        try:
            context.close()
        finally:
            browser.close()


def main():
    parser = argparse.ArgumentParser(description="Generate scorecard PNGs (and optional PDFs) using Playwright")
    parser.add_argument(
        "--countries",
        nargs="*",
        help="Optional list of country names to process (as in 'exporter' column). If omitted, all countries are processed.",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Optional limit for number of countries to process (useful for quick tests)",
    )
    parser.add_argument(
        "--pdf",
        action="store_true",
        help="Also generate A4 PDFs alongside PNGs",
    )
    parser.add_argument(
        "--policy-date",
        type=str,
        default="1 August 2025",
        help="Policy effective date (e.g., '1 August 2025')",
    )
    parser.add_argument(
        "--update-date",
        type=str,
        default=None,
        help="Data update/processing date (e.g., '6 September 2025'). Defaults to today.",
    )
    args = parser.parse_args()
    
    # Set default update date to today if not provided
    if args.update_date is None:
        args.update_date = datetime.now().strftime("%d %B %Y")

    root = Path(__file__).resolve().parents[2]
    template_path = root / "styling" / "trump-tariff-scorecard.html"
    header_icon_path = root / "styling" / "GTA Icon Color dark.png"
    footer_logo_path = root / "styling" / "GTA_logo.png"

    if not template_path.exists():
        raise FileNotFoundError(f"Template HTML not found at {template_path}")
    if not header_icon_path.exists():
        raise FileNotFoundError(f"Header icon not found at {header_icon_path}")
    if not footer_logo_path.exists():
        raise FileNotFoundError(f"Footer logo not found at {footer_logo_path}")

    df = load_processed_dataset(root)
    reciprocal_rates = load_reciprocal_rates(root)

    all_countries = sorted(df["exporter"].dropna().unique().tolist())
    target_countries = args.countries if args.countries else all_countries
    if args.limit is not None:
        target_countries = target_countries[: max(0, int(args.limit))]

    # Use simple date-based folder name (YYMMDD format)
    # Extract date from update_date string (e.g., "30 September 2025" -> "250930")
    date_suffix = datetime.strptime(args.update_date, "%d %B %Y").strftime("%y%m%d")
    out_dir = root / "results" / "scorecards" / date_suffix
    out_dir.mkdir(parents=True, exist_ok=True)

    index_rows = []
    for i, country in enumerate(target_countries, start=1):
        try:
            stats = compute_country_stats(df, country, reciprocal_rates)
            out_png = out_dir / f"{country.replace(' ', '_')}_Scorecard.png"
            render_scorecard(
                template_path=template_path,
                header_icon_path=header_icon_path,
                footer_logo_path=footer_logo_path,
                out_png_path=out_png,
                out_pdf_path=(out_dir / f"{country.replace(' ', '_')}_Scorecard.pdf") if args.pdf else None,
                stats=stats,
                policy_date=args.policy_date,
                update_date=args.update_date,
            )
            index_rows.append({"country": country, "status": "ok", "file": str(out_png.relative_to(out_dir))})
            print(f"[{i}/{len(target_countries)}] OK - {country}")
        except Exception as e:
            index_rows.append({"country": country, "status": f"error: {e}", "file": ""})
            print(f"[{i}/{len(target_countries)}] ERROR - {country}: {e}")

    # Save index CSV
    pd.DataFrame(index_rows).to_csv(out_dir / "index.csv", index=False)
    print(f"\nDone. Output: {out_dir}")


if __name__ == "__main__":
    main()


