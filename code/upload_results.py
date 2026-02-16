#!/usr/bin/env python3
"""
Consolidated AWS S3 Upload Script for US Tariff Barrier Estimates
Uploads all project results (replication files, charts, documents) to S3
"""

import os
import sys
from pathlib import Path
from typing import Optional, List, Dict

# Try to import boto3 with helpful error message
try:
    import boto3
    from botocore.exceptions import NoCredentialsError, ClientError
    BOTO3_AVAILABLE = True
except ImportError:
    print("Error: boto3 is not installed.")
    print("Please install it using: pip install boto3")
    BOTO3_AVAILABLE = False


# =============================================================================
# AWS CONFIGURATION
# =============================================================================

def load_aws_config():
    """Load AWS configuration from aws-keys.txt file."""
    config_file = Path(__file__).parent / "scorecards" / "aws-keys.txt"
    config = {}

    if not config_file.exists():
        print(f"Warning: AWS config file not found: {config_file}")
        return None

    try:
        with open(config_file, 'r') as f:
            for line in f:
                line = line.strip()
                if line and '=' in line and not line.startswith('#'):
                    key, value = line.split('=', 1)
                    config[key.strip()] = value.strip()
        return config
    except Exception as e:
        print(f"Error reading AWS config: {e}")
        return None


# Load AWS configuration
aws_config = load_aws_config()
AWS_ACCESS_KEY_ID = aws_config.get('AWS_ACCESS_KEY_ID') if aws_config else None
AWS_SECRET_ACCESS_KEY = aws_config.get('AWS_SECRET_ACCESS_KEY') if aws_config else None
AWS_REGION = aws_config.get('AWS_REGION', 'eu-west-1') if aws_config else 'eu-west-1'
S3_BUCKET_NAME = aws_config.get('S3_BUCKET_NAME', 'ricardo-dashboard') if aws_config else 'ricardo-dashboard'
S3_FOLDER_PREFIX = aws_config.get('S3_FOLDER_PREFIX', 'reports/') if aws_config else 'reports/'


# =============================================================================
# S3 CLIENT MANAGEMENT
# =============================================================================

def get_s3_client():
    """Initialize and return an S3 client with the configured credentials."""
    if not BOTO3_AVAILABLE:
        print("Cannot create S3 client: boto3 is not installed.")
        return None

    if not AWS_ACCESS_KEY_ID or not AWS_SECRET_ACCESS_KEY:
        print("AWS credentials not available. Check aws-keys.txt file.")
        return None

    try:
        return boto3.client(
            's3',
            aws_access_key_id=AWS_ACCESS_KEY_ID,
            aws_secret_access_key=AWS_SECRET_ACCESS_KEY,
            region_name=AWS_REGION
        )
    except Exception as e:
        print(f"Error creating S3 client: {e}")
        return None


# =============================================================================
# CORE UPLOAD FUNCTION
# =============================================================================

def upload_file_to_s3(file_path: Path, s3_key: str = None, folder: str = "replication") -> Optional[str]:
    """
    Upload a file to S3 with public read access.

    Args:
        file_path: Path to the file to upload
        s3_key: Custom S3 key (path). If None, uses filename with folder prefix
        folder: Subfolder within S3_FOLDER_PREFIX (default: "replication")

    Returns:
        Public URL of the uploaded file, or None if upload failed
    """
    if not file_path.exists():
        print(f"File not found: {file_path}")
        return None

    s3_client = get_s3_client()
    if not s3_client:
        return None

    # Generate S3 key if not provided
    if s3_key is None:
        s3_key = f"{S3_FOLDER_PREFIX}{folder}/{file_path.name}"

    try:
        # Upload file with public-read ACL
        print(f"Uploading {file_path.name} to S3...")
        s3_client.upload_file(
            str(file_path),
            S3_BUCKET_NAME,
            s3_key,
            ExtraArgs={'ACL': 'public-read'}
        )

        # Generate public URL
        public_url = f"https://{S3_BUCKET_NAME}.s3.{AWS_REGION}.amazonaws.com/{s3_key}"
        print(f"  Uploaded: {public_url}")
        return public_url

    except FileNotFoundError:
        print(f"  File not found: {file_path}")
        return None
    except NoCredentialsError:
        print("  AWS credentials not available")
        return None
    except ClientError as e:
        print(f"  AWS error uploading {file_path.name}: {e}")
        return None
    except Exception as e:
        print(f"  Unexpected error uploading {file_path.name}: {e}")
        return None


# =============================================================================
# RUN DIRECTORY RESOLUTION
# =============================================================================

def get_project_root() -> Path:
    """Get the project root directory."""
    return Path(__file__).resolve().parents[1]


def resolve_run_dir(run_dir_arg: Optional[str] = None) -> Path:
    """
    Resolve the run output directory.

    If --run-dir is given, use it (relative to project root).
    Otherwise, find the most recent results/{YYMMDD}/ directory.
    """
    root = get_project_root()

    if run_dir_arg:
        run_dir = root / run_dir_arg
        if run_dir.exists():
            return run_dir
        print(f"Warning: Specified run-dir not found: {run_dir}")

    # Fall back: find the most recent results/{YYMMDD}/ directory
    results_dir = root / "results"
    if results_dir.exists():
        subdirs = sorted(
            [d for d in results_dir.iterdir() if d.is_dir() and d.name.isdigit()],
            key=lambda d: d.name,
            reverse=True
        )
        if subdirs:
            return subdirs[0]

    # Final fallback: project root results/
    return results_dir


# =============================================================================
# FILE FINDER FUNCTIONS
# =============================================================================

def find_replication_zip(run_dir: Path) -> Optional[Path]:
    """Find the most recent replication ZIP file."""
    search_dir = run_dir / "public"
    pattern = "GTA - US Tariff Barrier Estimates - Replication Files - *.zip"
    zip_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if not zip_files:
        print(f"No replication ZIP files found in {search_dir}")
        return None

    latest_zip = max(zip_files, key=lambda p: p.stat().st_mtime)
    print(f"Found replication ZIP: {latest_zip.name}")
    return latest_zip


def find_public_excel(run_dir: Path) -> Optional[Path]:
    """Find the public Excel file in the run directory."""
    search_dir = run_dir / "public"

    # New naming: *_public.xlsx
    pattern = "*_public.xlsx"
    public_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if public_files:
        # Prefer undated version (no " - YYMMDD" suffix)
        undated = [f for f in public_files if " - " not in f.stem]
        if undated:
            latest_file = max(undated, key=lambda p: p.stat().st_mtime)
        else:
            latest_file = max(public_files, key=lambda p: p.stat().st_mtime)
        print(f"Found public Excel file: {latest_file.name}")
        return latest_file

    print(f"No public Excel file found in {search_dir}")
    return None


def find_charts_data_zip(run_dir: Path) -> Optional[Path]:
    """Find the most recent Charts & Data ZIP file."""
    search_dir = run_dir / "distribution"
    pattern = "GTA - US Tariff Barrier Estimates - Charts & Data *.zip"
    zip_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if not zip_files:
        print(f"No Charts & Data ZIP files found in {search_dir}")
        return None

    latest_zip = max(zip_files, key=lambda p: p.stat().st_mtime)
    print(f"Found Charts & Data ZIP: {latest_zip.name}")
    return latest_zip


def find_relative_advantage_chart(run_dir: Path) -> Optional[Path]:
    """Find the relative advantage top 20 chart."""
    chart_file = run_dir / "analysis" / "charts" / "relative_advantage_top20.png"

    if not chart_file.exists():
        print(f"Relative advantage chart not found at {chart_file}")
        return None

    print(f"Found relative advantage chart: {chart_file.name}")
    return chart_file


def find_c4tp_chart(run_dir: Path) -> Optional[Path]:
    """Find the C4TP relative advantage chart."""
    chart_file = run_dir / "countries" / "c4tp" / "relative_advantage_c4tp.png"

    if not chart_file.exists():
        print(f"C4TP chart not found at {chart_file}")
        return None

    print(f"Found C4TP chart: {chart_file.name}")
    return chart_file


def find_c4tp_chartbook(run_dir: Path) -> Optional[Path]:
    """Find the latest C4TP chart book PDF."""
    search_dir = run_dir / "chartbooks" / "c4tp"
    pattern = "RTTA - C4TP - *.pdf"
    pdf_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if not pdf_files:
        print(f"No C4TP chart book PDFs found in {search_dir}")
        return None

    latest_pdf = max(pdf_files, key=lambda p: p.stat().st_mtime)
    print(f"Found C4TP chart book: {latest_pdf.name}")
    return latest_pdf


def find_c4tp_data_charts_zip(run_dir: Path) -> Optional[Path]:
    """Find the latest C4TP data & charts ZIP file."""
    search_dir = run_dir / "chartbooks" / "c4tp"
    pattern = "RTTA - C4TP - data & charts - *.zip"
    zip_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if not zip_files:
        print(f"No C4TP data & charts ZIP files found in {search_dir}")
        return None

    latest_zip = max(zip_files, key=lambda p: p.stat().st_mtime)
    print(f"Found C4TP data & charts ZIP: {latest_zip.name}")
    return latest_zip


def find_general_chartbook(run_dir: Path) -> Optional[Path]:
    """Find the most recent general illustration chartbook PDF."""
    search_dir = run_dir / "chartbooks" / "general"
    pattern = "RTTA - Illustration ChartBook - *.pdf"
    pdf_files = list(search_dir.glob(pattern)) if search_dir.exists() else []

    if not pdf_files:
        print(f"No general illustration chartbook PDF files found in {search_dir}")
        return None

    latest_pdf = max(pdf_files, key=lambda p: p.stat().st_mtime)
    print(f"Found general chartbook: {latest_pdf.name}")
    return latest_pdf


# =============================================================================
# UPLOAD FUNCTIONS BY CATEGORY
# =============================================================================

def upload_replication_files(run_dir: Path) -> Dict[str, Optional[str]]:
    """Upload replication ZIP and public Excel file to S3."""
    print("=" * 80)
    print("UPLOADING REPLICATION FILES")
    print("=" * 80)

    results = {'zip_url': None, 'excel_url': None}

    # Upload public Excel file
    print("\n1. PUBLIC EXCEL FILE")
    print("-" * 40)
    excel_file = find_public_excel(run_dir)
    if excel_file:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}replication/{excel_file.name}"
        results['excel_url'] = upload_file_to_s3(excel_file, s3_key=fixed_s3_key)

    return results


def upload_charts(run_dir: Path) -> Dict[str, Optional[str]]:
    """Upload chart files to S3."""
    print("=" * 80)
    print("UPLOADING CHARTS")
    print("=" * 80)

    results = {}

    # General relative advantage chart
    print("\n1. GENERAL RELATIVE ADVANTAGE CHART")
    print("-" * 40)
    chart_file = find_relative_advantage_chart(run_dir)
    if chart_file:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}charts/relative-advantage-top20.png"
        results['general_chart'] = upload_file_to_s3(chart_file, s3_key=fixed_s3_key)

    # C4TP chart
    print("\n2. C4TP RELATIVE ADVANTAGE CHART")
    print("-" * 40)
    c4tp_chart_file = find_c4tp_chart(run_dir)
    if c4tp_chart_file:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}charts/relative-advantage-c4tp.png"
        results['c4tp_chart'] = upload_file_to_s3(c4tp_chart_file, s3_key=fixed_s3_key)

    return results


def upload_documents(run_dir: Path) -> Dict[str, Optional[str]]:
    """Upload document files (PDFs, ZIPs) to S3."""
    print("=" * 80)
    print("UPLOADING DOCUMENTS")
    print("=" * 80)

    results = {}

    # Charts & Data ZIP
    print("\n1. CHARTS & DATA PACKAGE")
    print("-" * 40)
    charts_zip = find_charts_data_zip(run_dir)
    if charts_zip:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}charts-data/GTA-US-Tariff-Barrier-Estimates-Charts-Data.zip"
        results['charts_data_zip'] = upload_file_to_s3(charts_zip, s3_key=fixed_s3_key)

    # C4TP Chart Book
    print("\n2. C4TP CHART BOOK")
    print("-" * 40)
    c4tp_book = find_c4tp_chartbook(run_dir)
    if c4tp_book:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}chartbooks/RTTA-C4TP-ChartBook.pdf"
        results['c4tp_chartbook'] = upload_file_to_s3(c4tp_book, s3_key=fixed_s3_key)

    # C4TP Data & Charts ZIP
    print("\n3. C4TP DATA & CHARTS PACKAGE")
    print("-" * 40)
    c4tp_data_zip = find_c4tp_data_charts_zip(run_dir)
    if c4tp_data_zip:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}chartbooks/RTTA-C4TP-Data-Charts.zip"
        results['c4tp_data_charts'] = upload_file_to_s3(c4tp_data_zip, s3_key=fixed_s3_key)

    # General Illustration Chartbook
    print("\n4. GENERAL ILLUSTRATION CHARTBOOK")
    print("-" * 40)
    general_book = find_general_chartbook(run_dir)
    if general_book:
        fixed_s3_key = f"{S3_FOLDER_PREFIX}chartbooks/RTTA-Illustration-ChartBook.pdf"
        results['general_chartbook'] = upload_file_to_s3(general_book, s3_key=fixed_s3_key)

    return results


# =============================================================================
# MAIN UPLOAD ORCHESTRATION
# =============================================================================

def upload_all(run_dir: Path) -> Dict[str, Dict[str, Optional[str]]]:
    """Upload all project results to S3."""
    print(f"\nAWS Configuration:")
    print(f"  Bucket: {S3_BUCKET_NAME}")
    print(f"  Region: {AWS_REGION}")
    print(f"  Folder Prefix: {S3_FOLDER_PREFIX}")
    print(f"  Run Directory: {run_dir}\n")

    all_results = {
        'replication': {},
        'charts': {},
        'documents': {}
    }

    all_results['replication'] = upload_replication_files(run_dir)
    all_results['charts'] = upload_charts(run_dir)
    all_results['documents'] = upload_documents(run_dir)

    # Print comprehensive summary
    print("\n" + "=" * 80)
    print("COMPREHENSIVE UPLOAD SUMMARY")
    print("=" * 80)

    all_urls = []
    for category, results in all_results.items():
        successful = [url for url in results.values() if url]
        all_urls.extend(successful)
        print(f"\n{category.upper()}: {len(successful)}/{len(results)} files uploaded")
        for key, url in results.items():
            status = "+" if url else "-"
            print(f"  {status} {key}: {url if url else 'Failed'}")

    print(f"\n{'=' * 80}")
    print(f"TOTAL: {len(all_urls)} files successfully uploaded")
    print(f"{'=' * 80}\n")

    return all_results


# =============================================================================
# COMMAND LINE INTERFACE
# =============================================================================

def main():
    """Main entry point with command-line interface."""

    # Parse --run-dir from arguments
    run_dir_arg = None
    command = 'all'
    positional_args = []

    i = 1
    while i < len(sys.argv):
        if sys.argv[i] == '--run-dir' and i + 1 < len(sys.argv):
            run_dir_arg = sys.argv[i + 1]
            i += 2
        else:
            positional_args.append(sys.argv[i])
            i += 1

    if positional_args:
        command = positional_args[0].lower()

    # Resolve run directory
    run_dir = resolve_run_dir(run_dir_arg)
    print(f"Using run directory: {run_dir}")

    if not run_dir.exists():
        print(f"Error: Run directory does not exist: {run_dir}")
        sys.exit(1)

    if command == 'replication':
        upload_replication_files(run_dir)
    elif command == 'charts':
        upload_charts(run_dir)
    elif command == 'documents':
        upload_documents(run_dir)
    elif command == 'all':
        upload_all(run_dir)
    else:
        print(f"Unknown command: {command}")
        print("\nAvailable commands:")
        print("  replication  - Upload replication files (ZIP + Excel)")
        print("  charts       - Upload chart files (PNGs)")
        print("  documents    - Upload documents (Chart books, data ZIPs)")
        print("  all          - Upload everything (default)")
        print("\nOptions:")
        print("  --run-dir DIR  - Specify results subdirectory (e.g., results/260216)")


if __name__ == "__main__":
    main()
