"""
Map WTO ATC Annex HS-92 codes to current US HTS8 codes.

Two-stage mapping pipeline:

  Stage 1 — Concordance-based (primary):
    Uses the UN HS2022-HS1992 correlation table to map HS-92 codes to
    HS-2022 6-digit codes, then expands each to HTS8 via the existing
    universe. Only 1:1, n:1, and 1:n relationships are used; n:n is
    excluded to avoid false positives.

  Stage 2 — Direct prefix matching (fallback):
    For any ATC codes not resolved by the concordance (n:n-only codes),
    falls back to matching HTS8 codes whose first 6 digits equal the
    HS-92 code.

Results from both stages are merged and deduplicated. Each HTS8 row
records its mapping_method: concordance, prefix_match, or both.

IMPORTANT: The 2024 USITC import data stores some HS codes as 7-digit
integers (leading zero stripped). This script pads all codes to 8 digits
before matching.

Inputs:
    data/atc_annex_hs92_raw.csv                                — ATC annex codes
    data/HS2022toHS1992ConversionAndCorrelationTables.xlsx      — UN concordance
    data/usitc_us_imports_2024.csv                              — 2024 US imports
    data/us_imports_2025.csv                                    — 2025 US imports
    data/scenarios/mfn_rates.csv                                — Full MFN schedule

Output:
    data/atc_annex_hts8_mapped.csv                              — Mapped HTS8 codes

Usage:
    python code/map_atc_to_hts8.py
"""

import csv
from pathlib import Path

import openpyxl

BASE = Path(__file__).resolve().parent.parent
ATC_CSV = BASE / "data" / "atc_annex_hs92_raw.csv"
CONCORDANCE_XLSX = BASE / "data" / "HS2022toHS1992ConversionAndCorrelationTables.xlsx"
IMPORTS_2024 = BASE / "data" / "usitc_us_imports_2024.csv"
IMPORTS_2025 = BASE / "data" / "us_imports_2025.csv"
MFN_RATES = BASE / "data" / "scenarios" / "mfn_rates.csv"
OUTPUT_CSV = BASE / "data" / "atc_annex_hts8_mapped.csv"

VALID_RELATIONSHIPS = {"1:1", "n:1", "1:n"}


def load_atc_codes():
    """Load ATC annex HS-92 codes. Returns dict: hs6 -> (description, excluded)."""
    codes = {}
    with open(ATC_CSV) as f:
        reader = csv.reader(f, delimiter=";")
        next(reader)
        for row in reader:
            hs6 = row[0].replace(".", "")
            codes[hs6] = (row[1], row[3])
    return codes


def load_concordance():
    """Load UN HS2022-HS1992 concordance (Correlations sheet).

    Returns dict: hs92_6digit -> set of hs2022_6digit codes.
    Only includes 1:1, n:1, and 1:n relationships (n:n excluded).
    """
    wb = openpyxl.load_workbook(CONCORDANCE_XLSX, read_only=True)
    ws = wb["HS2022-HS1992 Correlations"]

    hs92_to_hs2022 = {}
    total_rows = 0
    included_rows = 0

    for i, row in enumerate(ws.iter_rows(values_only=True)):
        if i < 2:
            continue
        total_rows += 1
        hs2022 = str(row[0]).strip()
        hs1992 = str(row[1]).strip()
        rel = str(row[2]).strip()

        if rel not in VALID_RELATIONSHIPS:
            continue

        included_rows += 1
        if hs1992 not in hs92_to_hs2022:
            hs92_to_hs2022[hs1992] = set()
        hs92_to_hs2022[hs1992].add(hs2022)

    wb.close()

    print(f"  Concordance: {total_rows:,} rows total, "
          f"{included_rows:,} rows after excluding n:n")
    print(f"  Unique HS-92 codes in concordance: {len(hs92_to_hs2022):,}")

    return hs92_to_hs2022


def build_hts8_universe():
    """Build complete universe of unique HTS8 codes from all sources.

    Also builds a prefix index (hs6 -> set of hts8) for fast lookup.
    """
    hts8_codes = set()

    with open(IMPORTS_2024) as f:
        reader = csv.reader(f, delimiter=";")
        next(reader)
        for row in reader:
            hts8_codes.add(row[4].zfill(8))

    n_2024 = len(hts8_codes)

    with open(IMPORTS_2025) as f:
        reader = csv.reader(f, delimiter=";")
        next(reader)
        for row in reader:
            hts8_codes.add(row[4].zfill(8))

    n_trade = len(hts8_codes)

    with open(MFN_RATES) as f:
        reader = csv.reader(f)
        next(reader)
        for row in reader:
            if row[0] == "hts_schedule":
                hts8_codes.add(row[1].zfill(8))

    n_full = len(hts8_codes)

    print(f"  HTS8 universe: {n_2024:,} (2024) + {n_trade - n_2024:,} (2025 new)"
          f" + {n_full - n_trade:,} (MFN schedule only) = {n_full:,} total")

    # Build prefix index for O(1) lookups
    prefix_index = {}
    for hts8 in hts8_codes:
        hs6 = hts8[:6]
        if hs6 not in prefix_index:
            prefix_index[hs6] = set()
        prefix_index[hs6].add(hts8)

    return hts8_codes, prefix_index


def map_via_concordance(atc_codes, concordance, prefix_index):
    """Stage 1: Map ATC codes through the concordance.

    For each ATC HS-92 code:
      1. Look up all HS-2022 6-digit codes via concordance
      2. Expand each HS-2022 code to HTS8 via the prefix index

    Returns:
        results: dict of hts8 -> (hs92_fmt, description, excluded)
        resolved_hs92: set of hs92 codes that produced at least one HTS8
        unresolved_hs92: set of hs92 codes with concordance entries but
                         no HTS8 expansion (HS-2022 codes not in universe)
        not_in_concordance: set of hs92 codes absent from the concordance
    """
    results = {}
    resolved_hs92 = set()
    unresolved_hs92 = set()
    not_in_concordance = set()

    for hs6, (desc, excl) in sorted(atc_codes.items()):
        hs92_fmt = f"{hs6[:4]}.{hs6[4:]}"

        if hs6 not in concordance:
            not_in_concordance.add(hs6)
            continue

        hs2022_codes = concordance[hs6]
        found_any = False

        for hs2022 in hs2022_codes:
            matching_hts8 = prefix_index.get(hs2022, set())
            for hts8 in matching_hts8:
                results[hts8] = (hs92_fmt, desc, excl)
                found_any = True

        if found_any:
            resolved_hs92.add(hs6)
        else:
            unresolved_hs92.add(hs6)

    return results, resolved_hs92, unresolved_hs92, not_in_concordance


def map_via_prefix(atc_codes, prefix_index, skip_hs6=None):
    """Stage 2: Direct prefix matching fallback.

    For each ATC HS-92 code (optionally skipping already-resolved ones),
    finds all HTS8 codes whose first 6 digits match.

    Returns:
        results: dict of hts8 -> (hs92_fmt, description, excluded)
        resolved_hs92: set of hs92 codes matched
        unmatched_hs92: list of (hs92_fmt, description, excluded) for unmatched
    """
    if skip_hs6 is None:
        skip_hs6 = set()

    results = {}
    resolved_hs92 = set()
    unmatched_hs92 = []

    for hs6, (desc, excl) in sorted(atc_codes.items()):
        if hs6 in skip_hs6:
            continue

        hs92_fmt = f"{hs6[:4]}.{hs6[4:]}"
        matching_hts8 = prefix_index.get(hs6, set())

        if matching_hts8:
            for hts8 in matching_hts8:
                results[hts8] = (hs92_fmt, desc, excl)
            resolved_hs92.add(hs6)
        else:
            unmatched_hs92.append((hs92_fmt, desc, excl))

    return results, resolved_hs92, unmatched_hs92


def merge_results(concordance_results, prefix_results):
    """Merge results from both stages, tracking mapping method.

    Returns list of (hts8, hs92_fmt, description, excluded, mapping_method).
    """
    all_hts8 = set(concordance_results.keys()) | set(prefix_results.keys())
    merged = []

    for hts8 in sorted(all_hts8):
        in_conc = hts8 in concordance_results
        in_prefix = hts8 in prefix_results

        if in_conc and in_prefix:
            method = "both"
            hs92_fmt, desc, excl = concordance_results[hts8]
        elif in_conc:
            method = "concordance"
            hs92_fmt, desc, excl = concordance_results[hts8]
        else:
            method = "prefix_match"
            hs92_fmt, desc, excl = prefix_results[hts8]

        merged.append((hts8, hs92_fmt, desc, excl, method))

    return merged


def write_output(merged, output_path):
    """Write mapped HTS8 codes to CSV."""
    with open(output_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, delimiter=";", quoting=csv.QUOTE_MINIMAL)
        writer.writerow(["hs_8digit", "hs_92_origin", "description",
                         "excluded", "source", "mapping_method"])
        for hts8, hs92, desc, excl, method in merged:
            writer.writerow([hts8, hs92, desc, excl, "WTO ATC Annex", method])

    print(f"\nSaved {len(merged)} HTS8 codes to {output_path}")


def write_unmatched_report(unmatched):
    """Print unmatched codes for manual concordance."""
    if not unmatched:
        print("\nAll ATC codes matched successfully.")
        return

    chapters = {}
    for hs92, desc, excl in unmatched:
        ch = hs92[:2]
        if ch not in chapters:
            chapters[ch] = []
        chapters[ch].append((hs92, desc, excl))

    print(f"\n{'='*70}")
    print(f"UNMATCHED CODES: {len(unmatched)} HS-92 codes need manual review")
    print(f"{'='*70}")
    print("These HS-92 codes only have n:n concordance relationships and no")
    print("direct prefix match in the current HTS8 universe.\n")

    for ch in sorted(chapters.keys(), key=int):
        codes = chapters[ch]
        ex_count = sum(1 for _, _, e in codes if e == "1")
        label = f" ({ex_count} excluded)" if ex_count else ""
        print(f"  Chapter {ch}: {len(codes)} codes{label}")
        for hs92, desc, excl in codes:
            flag = " [EXCLUDED]" if excl == "1" else ""
            print(f"    {hs92}: {desc[:60]}{flag}")


def main():
    print("Mapping ATC Annex HS-92 codes to current HTS8 codes")
    print("=" * 55)

    # Step 1: Load inputs
    print("\n1. Loading ATC annex codes...")
    atc_codes = load_atc_codes()
    active = sum(1 for _, (_, e) in atc_codes.items() if e == "0")
    excluded = sum(1 for _, (_, e) in atc_codes.items() if e == "1")
    print(f"  {len(atc_codes)} HS-92 codes ({active} active, {excluded} excluded)")

    print("\n2. Loading UN HS2022-HS1992 concordance...")
    concordance = load_concordance()

    print("\n3. Building HTS8 universe...")
    hts8_universe, prefix_index = build_hts8_universe()

    # Step 2: Stage 1 — Concordance mapping (all codes)
    print("\n4. Stage 1: Concordance-based mapping (all codes)...")
    conc_results, conc_resolved, conc_unresolved, not_in_conc = \
        map_via_concordance(atc_codes, concordance, prefix_index)

    print(f"  Concordance resolved: {len(conc_resolved)} HS-92 codes "
          f"-> {len(conc_results):,} HTS8 codes")
    print(f"  Not in concordance: {len(not_in_conc)} HS-92 codes")
    print(f"  In concordance but no HTS8 expansion: {len(conc_unresolved)} codes")

    # Step 3: Stage 2 — Prefix matching (all codes, for completeness)
    print("\n5. Stage 2: Direct prefix matching (all codes)...")
    prefix_results, prefix_resolved, fully_unmatched = \
        map_via_prefix(atc_codes, prefix_index)

    print(f"  Prefix resolved: {len(prefix_resolved)} HS-92 codes "
          f"-> {len(prefix_results):,} HTS8 codes")
    print(f"  Prefix unmatched: {len(fully_unmatched)} HS-92 codes")

    # Step 4: Merge and deduplicate
    print("\n6. Merging results...")
    merged = merge_results(conc_results, prefix_results)

    # Identify truly unmatched (neither concordance nor prefix)
    all_resolved_hs92 = conc_resolved | prefix_resolved
    truly_unmatched = []
    for hs6, (desc, excl) in sorted(atc_codes.items()):
        if hs6 not in all_resolved_hs92:
            hs92_fmt = f"{hs6[:4]}.{hs6[4:]}"
            truly_unmatched.append((hs92_fmt, desc, excl))

    # Statistics
    unique_hs92_in_output = len(set(m[1] for m in merged))
    method_counts = {}
    for _, _, _, _, method in merged:
        method_counts[method] = method_counts.get(method, 0) + 1

    conc_only = len(set(conc_results.keys()) - set(prefix_results.keys()))
    prefix_only = len(set(prefix_results.keys()) - set(conc_results.keys()))

    print(f"  Total unique HTS8 codes: {len(merged):,}")
    print(f"  From {unique_hs92_in_output} HS-92 codes "
          f"({len(atc_codes) - len(truly_unmatched)} resolved, "
          f"{len(truly_unmatched)} unmatched)")
    print(f"  By mapping method:")
    for method in ["concordance", "prefix_match", "both"]:
        count = method_counts.get(method, 0)
        print(f"    {method}: {count:,}")
    print(f"  Net new from concordance (not found by prefix): {conc_only:,}")
    print(f"  Prefix-only (not in concordance): {prefix_only:,}")

    active_hts8 = sum(1 for m in merged if m[3] == "0")
    excluded_hts8 = sum(1 for m in merged if m[3] == "1")
    print(f"  Active HTS8: {active_hts8:,} | Excluded HTS8: {excluded_hts8:,}")

    # Step 5: Write output
    write_output(merged, OUTPUT_CSV)
    write_unmatched_report(truly_unmatched)

    print(f"\nNext steps:")
    if truly_unmatched:
        active_unmatched = sum(1 for _, _, e in truly_unmatched if e == "0")
        print(f"  1. Review {len(truly_unmatched)} unmatched codes "
              f"({active_unmatched} active) — n:n concordance only")
    print(f"  2. Review mapped codes in {OUTPUT_CSV}")
    print(f"  3. Add active (excluded=0) codes to exceptions.csv for S122")


if __name__ == "__main__":
    main()
