"""
Reusable script to parse bilateral deal Annex 2A and 2B CSV files and append
qualifying products to data/scenarios/exceptions.csv.

Includes only rows where scope is empty or "Ex" (not "Pharma" or "Aircraft").
Zero-pads HTSUS codes to 8 digits.

Usage:
    python3 code/prepare_deal_exceptions.py \
        --annex-2a temp/twn_2a.csv \
        --annex-2b temp/twn_2b.csv \
        --rate-type twn_ieepa_deal \
        --origin TWN \
        --effective-date 2099-01-01 \
        --source-prefix "Chinese Taipei"
"""

import argparse
import csv
import os
import sys


def parse_annex(filepath, annex_label):
    """Parse an annex CSV, handling multiline descriptions and BOM."""
    rows = []
    with open(filepath, "r", encoding="utf-8-sig") as f:
        reader = csv.reader(f, delimiter=";")
        header = next(reader)

        # Find column indices (handle both "Scope_limitations" and "Scope Limitations")
        htsus_idx = header.index("HTSUS")
        scope_idx = None
        for i, col in enumerate(header):
            if col.strip().lower().replace(" ", "_") == "scope_limitations":
                scope_idx = i
                break
        if scope_idx is None:
            raise ValueError(f"Could not find scope column in {filepath}. "
                             f"Headers: {header}")

        current_code = None
        current_scope = None

        for row in reader:
            if not row:
                continue

            # A new product row starts with a non-empty HTSUS code
            if row[htsus_idx].strip():
                # Save previous record if it exists
                if current_code is not None:
                    rows.append((current_code, current_scope, annex_label))

                current_code = row[htsus_idx].strip()
                current_scope = row[scope_idx].strip() if scope_idx < len(row) else ""
            # Otherwise it's a continuation line (multiline description) - skip

        # Don't forget the last record
        if current_code is not None:
            rows.append((current_code, current_scope, annex_label))

    return rows


def main():
    parser = argparse.ArgumentParser(
        description="Parse bilateral deal annexes and append to exceptions.csv"
    )
    parser.add_argument("--annex-2a", required=True, help="Path to Annex 2A CSV")
    parser.add_argument("--annex-2b", default=None, help="Path to Annex 2B CSV (optional)")
    parser.add_argument("--rate-type", required=True, help="Rate type (e.g. twn_ieepa_deal)")
    parser.add_argument("--origin", required=True, help="Import origin code (e.g. TWN)")
    parser.add_argument("--effective-date", required=True, help="Effective date (e.g. 2099-01-01)")
    parser.add_argument("--source-prefix", required=True, help="Source label prefix (e.g. 'Chinese Taipei')")
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    args = parser.parse_args()

    # Resolve paths relative to script location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_dir = os.path.dirname(script_dir)
    exceptions_file = os.path.join(project_dir, "data", "scenarios", "exceptions.csv")

    annex_2a_path = os.path.join(project_dir, args.annex_2a)
    annex_2b_path = os.path.join(project_dir, args.annex_2b) if args.annex_2b else None

    # Parse annexes
    annex_2a = parse_annex(annex_2a_path, f"{args.source_prefix} Annex 2A")
    all_products = list(annex_2a)

    print(f"Annex 2A: {len(annex_2a)} total products")

    if annex_2b_path:
        annex_2b = parse_annex(annex_2b_path, f"{args.source_prefix} Annex 2B")
        all_products.extend(annex_2b)
        print(f"Annex 2B: {len(annex_2b)} total products")

    # Count by scope limitation
    for label, annex in [("2A", annex_2a)] + ([("2B", annex_2b)] if annex_2b_path else []):
        scopes = {}
        for _, scope, _ in annex:
            key = scope if scope else "(none)"
            scopes[key] = scopes.get(key, 0) + 1
        print(f"  Annex {label} scope breakdown: {scopes}")

    # Filter: keep only empty scope or "Ex" (exclude "Pharma" and "Aircraft")
    filtered = [(code, source) for code, scope, source in all_products
                if scope in ("", "Ex")]

    print(f"\nFiltered (empty or Ex scope): {len(filtered)} products")

    # Zero-pad HTSUS codes to 8 digits
    exception_rows = []
    for code, source in filtered:
        padded = code.zfill(8)
        assert len(padded) == 8, f"Code {code} padded to {padded} (length {len(padded)})"
        exception_rows.append((padded, source))

    # Check for duplicates
    codes_only = [r[0] for r in exception_rows]
    if len(codes_only) != len(set(codes_only)):
        dupes = set(c for c in codes_only if codes_only.count(c) > 1)
        print(f"WARNING: Duplicate codes found: {dupes}")
    else:
        print(f"No duplicates in {len(codes_only)} exception codes")

    # Write or preview
    if args.dry_run:
        print(f"\n[DRY RUN] Would append {len(exception_rows)} rows to {exceptions_file}")
        print("Sample rows:")
        for padded_code, source in exception_rows[:5]:
            print(f"  {padded_code};{args.rate_type};{args.origin};country_exception;"
                  f"{args.effective_date};;{source}")
        if len(exception_rows) > 5:
            print(f"  ... and {len(exception_rows) - 5} more")
    else:
        with open(exceptions_file, "a", encoding="utf-8") as f:
            for padded_code, source in exception_rows:
                line = (f"{padded_code};{args.rate_type};{args.origin};country_exception;"
                        f"{args.effective_date};;{source}\n")
                f.write(line)
        print(f"\nAppended {len(exception_rows)} rows to {exceptions_file}")


if __name__ == "__main__":
    main()
