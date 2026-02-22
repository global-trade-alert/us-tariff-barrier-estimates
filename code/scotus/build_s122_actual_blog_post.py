#!/usr/bin/env python3
"""
Build blog post for Section 122 Actual Tariff Regime in the GTA API database.

Creates a new report entry in core_report_list, upserts text sections
(body, abstract, lead, subtitle), links author and tag, and reads the
generated blog HTML from results/scotus/s122_actual/blog/.

Publication status is set to 0 (unpublished) until manually activated.

Usage:
    python3 build_s122_actual_blog_post.py --dry-run    # Preview changes
    python3 build_s122_actual_blog_post.py               # Apply to database

Requirements:
    pip install pymysql
"""

import argparse
import os
import sys
from datetime import datetime
from pathlib import Path

import pymysql

# =============================================================================
# Configuration
# =============================================================================

AUTHOR_ID = 1  # Johannes Fritz
TAG_ID = 2     # GTA

DB_CONFIG = {
    "host": "gtaapi.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
    "port": 3306,
    "user": "gtaapi",
    "password": "W3T4WoE5KcnTV7YcrHrKFHdM5qjWQy2C",
    "database": "gtaapi",
    "cursorclass": pymysql.cursors.DictCursor,
}

# Report metadata
REPORT_TITLE = "Section 122 in effect: what the US tariff regime looks like now"
REPORT_SLUG = "S122-Actual-Tariff-Regime"
REPORT_SUBTITLE = (
    "The Section 122 proclamation imposes a surcharge on most US imports (raised from "
    "10% to 15% on 22 February), effective 24 February 2026 for 150 days. The trade-weighted "
    "average US tariff rate is 13.2% under S122 at 15%, compared with 11.6% at 10%, "
    "15.3% before SCOTUS, and 8.3% after the IEEPA strike-down."
)
REPORT_TYPE_ID = 1  # Report
PUBLICATION_DATE = datetime(2026, 2, 21, 14, 0, 0)
ORIGIN_URL = f"https://globaltradealert.org/reports/{REPORT_SLUG}"
IMAGE_URL = "https://ricardo-dashboard.s3.eu-west-1.amazonaws.com/reports/1756101473065_Untitled - 2025-08-25T085744.330.png"

ABSTRACT = (
    "The Supreme Court struck down IEEPA tariffs on 20 February 2026. The White House "
    "responded with a Section 122 proclamation imposing a surcharge on most imports "
    "(initially 10%, raised to 15% on 22 February), effective 24 February 2026 for 150 days. "
    "The trade-weighted average US tariff rate is 13.2% under S122 at 15%. This report "
    "quantifies the tariff regime under four scenarios: before SCOTUS, IEEPA strike-down, "
    "S122 at 10%, and S122 at 15%. The analysis covers 274,000+ trade flows at the "
    "exporter x HS 8-digit level."
)

LEAD = ABSTRACT

# Text type IDs (from core_report_text_type_list)
TEXT_TYPE_BODY = 1
TEXT_TYPE_ABSTRACT = 2
TEXT_TYPE_LEAD = 3
TEXT_TYPE_SUBTITLE = 4


# =============================================================================
# Project paths
# =============================================================================

PROJECT_ROOT = Path(__file__).resolve().parents[2]  # us-tariff-barrier-estimates/
BLOG_HTML_PATH = PROJECT_ROOT / "results" / "scotus" / "s122_actual" / "blog" / "s122_actual_blog_body.html"


# =============================================================================
# Database functions
# =============================================================================

def get_connection():
    return pymysql.connect(**DB_CONFIG)


def find_or_create_report(cursor, dry_run=False):
    """Find existing report by slug or create a new one. Returns report_id."""

    cursor.execute(
        "SELECT report_id FROM core_report_list WHERE slug = %s",
        (REPORT_SLUG,),
    )
    existing = cursor.fetchone()

    if existing:
        report_id = existing["report_id"]
        print(f"Found existing report: report_id={report_id}")
        return report_id

    print("Creating new report entry...")

    if not dry_run:
        cursor.execute(
            """INSERT INTO core_report_list
               (title, slug, date_published, date_created, published, type_id, origin_url,
                show_toc, pinned, pinned_landing, pinned_overview, pinned_header)
               VALUES (%s, %s, %s, NOW(), 0, %s, %s, 0, 0, 0, 0, 0)""",
            (REPORT_TITLE, REPORT_SLUG, PUBLICATION_DATE, REPORT_TYPE_ID, ORIGIN_URL),
        )
        report_id = cursor.lastrowid
        print(f"  -> Created report_id={report_id}")
        return report_id
    else:
        print("  -> DRY RUN: would create new report")
        cursor.execute("SELECT MAX(report_id) + 1 AS next_id FROM core_report_list")
        result = cursor.fetchone()
        return result["next_id"] if result["next_id"] else 999


def update_report_metadata(cursor, report_id, dry_run=False):
    """Update core_report_list with report metadata."""
    sql = """
        UPDATE core_report_list
        SET title = %s,
            slug = %s,
            date_published = %s,
            published = 0,
            type_id = %s,
            origin_url = %s,
            image_url = %s,
            show_toc = 0,
            pinned = 0,
            pinned_landing = 0,
            pinned_overview = 0,
            pinned_header = 0
        WHERE report_id = %s
    """
    params = (REPORT_TITLE, REPORT_SLUG, PUBLICATION_DATE, REPORT_TYPE_ID,
              ORIGIN_URL, IMAGE_URL, report_id)

    print(f"Updating report metadata (report_id={report_id})")
    print(f"  title: {REPORT_TITLE}")
    print(f"  slug: {REPORT_SLUG}")
    print(f"  date_published: {PUBLICATION_DATE}")
    print(f"  published: 0")

    if not dry_run:
        cursor.execute(sql, params)
        print(f"  -> {cursor.rowcount} row(s) updated")
    else:
        print("  -> DRY RUN: no changes made")


def upsert_text_section(cursor, report_id, type_id, type_name, text, dry_run=False):
    """Insert or update a text section in core_report_text_log."""
    cursor.execute(
        "SELECT id FROM core_report_text_log WHERE report_id = %s AND type_id = %s",
        (report_id, type_id),
    )
    existing = cursor.fetchone()

    print(f"Text section: {type_name} (type_id={type_id})")
    print(f"  length: {len(text)} chars")

    if existing:
        sql = "UPDATE core_report_text_log SET text = %s WHERE report_id = %s AND type_id = %s"
        params = (text, report_id, type_id)
        action = "UPDATE"
    else:
        sql = "INSERT INTO core_report_text_log (text, report_id, type_id) VALUES (%s, %s, %s)"
        params = (text, report_id, type_id)
        action = "INSERT"

    if not dry_run:
        cursor.execute(sql, params)
        print(f"  -> {action} completed ({cursor.rowcount} row)")
    else:
        print(f"  -> DRY RUN: would {action}")


def ensure_author_linkage(cursor, report_id, dry_run=False):
    """Ensure author is linked in core_report_author_log."""
    cursor.execute(
        "SELECT id FROM core_report_author_log WHERE report_id = %s AND author_id = %s",
        (report_id, AUTHOR_ID),
    )
    existing = cursor.fetchone()

    print(f"Author linkage: author_id={AUTHOR_ID} (Johannes Fritz)")

    if existing:
        print(f"  -> Already linked (id={existing['id']})")
    elif not dry_run:
        cursor.execute(
            "INSERT INTO core_report_author_log (author_id, report_id) VALUES (%s, %s)",
            (AUTHOR_ID, report_id),
        )
        print("  -> Inserted new linkage")
    else:
        print("  -> DRY RUN: would insert linkage")


def ensure_tag_linkage(cursor, report_id, dry_run=False):
    """Ensure GTA tag is linked in core_report_tag_log."""
    cursor.execute(
        "SELECT id FROM core_report_tag_log WHERE report_id = %s AND tag_id = %s",
        (report_id, TAG_ID),
    )
    existing = cursor.fetchone()

    print(f"Tag linkage: tag_id={TAG_ID} (GTA)")

    if existing:
        print(f"  -> Already linked (id={existing['id']})")
    elif not dry_run:
        cursor.execute(
            "INSERT INTO core_report_tag_log (tag_id, report_id) VALUES (%s, %s)",
            (TAG_ID, report_id),
        )
        print("  -> Inserted new linkage")
    else:
        print("  -> DRY RUN: would insert linkage")


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Build Section 122 actual tariff regime blog post in GTA database"
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Show what would be done without making changes"
    )
    parser.add_argument(
        "--report-id", type=int, default=None,
        help="Use a specific report_id instead of auto-detecting"
    )
    args = parser.parse_args()

    dry_run = args.dry_run
    if dry_run:
        print("=" * 60)
        print("DRY RUN MODE - No changes will be made to the database")
        print("=" * 60)

    # Load blog body HTML
    if BLOG_HTML_PATH.exists():
        body_html = BLOG_HTML_PATH.read_text(encoding="utf-8").strip()
        print(f"Loaded blog HTML from {BLOG_HTML_PATH} ({len(body_html)} chars)")
    else:
        print(f"WARNING: Blog HTML not found at {BLOG_HTML_PATH}")
        print("Run generate_s122_actual_output.R first to generate the blog HTML.")
        sys.exit(1)

    print(f"\nReport: {REPORT_TITLE}")
    print(f"Publication date: {PUBLICATION_DATE}")
    print()

    conn = get_connection()
    try:
        with conn.cursor() as cursor:
            # 1. Find or create report
            if args.report_id:
                report_id = args.report_id
                print(f"Using specified report_id={report_id}")
            else:
                report_id = find_or_create_report(cursor, dry_run)
            print()

            # 2. Update report metadata
            update_report_metadata(cursor, report_id, dry_run)
            print()

            # 3. Upsert text sections
            upsert_text_section(cursor, report_id, TEXT_TYPE_BODY, "Body",
                                body_html, dry_run)
            upsert_text_section(cursor, report_id, TEXT_TYPE_ABSTRACT, "Abstract",
                                ABSTRACT, dry_run)
            upsert_text_section(cursor, report_id, TEXT_TYPE_LEAD, "Lead",
                                LEAD, dry_run)
            upsert_text_section(cursor, report_id, TEXT_TYPE_SUBTITLE, "Subtitle",
                                REPORT_SUBTITLE, dry_run)
            print()

            # 4. Ensure author linkage
            ensure_author_linkage(cursor, report_id, dry_run)

            # 5. Ensure tag linkage
            ensure_tag_linkage(cursor, report_id, dry_run)
            print()

        if not dry_run:
            conn.commit()
            print("=" * 60)
            print(f"SUCCESS: Blog post created/updated (report_id={report_id})")
            print(f"URL: {ORIGIN_URL}")
            print(f"Status: UNPUBLISHED (set published=1 when ready)")
            print("=" * 60)
        else:
            print("=" * 60)
            print("DRY RUN COMPLETE")
            print(f"Would create/update report_id={report_id}")
            print("Run without --dry-run to apply changes")
            print("=" * 60)

    finally:
        conn.close()


if __name__ == "__main__":
    main()
