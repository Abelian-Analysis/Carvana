"""
Fetch historical Federal Funds Rate data from FRED and save to CSV.

This script fetches the effective federal funds rate (FEDFUNDS) from
the Federal Reserve Economic Data (FRED) API and saves it to a CSV file
for use in spread calculations.
"""

import pandas as pd
import requests
from datetime import datetime

# FRED API endpoint for the effective federal funds rate (monthly)
# Series ID: FEDFUNDS (monthly average)
# Alternative: DFF for daily data
FRED_SERIES_ID = "FEDFUNDS"
FRED_API_URL = "https://api.stlouisfed.org/fred/series/observations"

def fetch_fed_funds_rate(api_key: str, start_date: str = "2015-01-01") -> pd.DataFrame:
    """
    Fetch federal funds rate data from FRED API.

    Args:
        api_key: FRED API key (get one free at https://fred.stlouisfed.org/docs/api/api_key.html)
        start_date: Start date for data retrieval (YYYY-MM-DD format)

    Returns:
        DataFrame with date and federal funds rate columns
    """
    params = {
        "series_id": FRED_SERIES_ID,
        "api_key": api_key,
        "file_type": "json",
        "observation_start": start_date,
        "observation_end": datetime.now().strftime("%Y-%m-%d"),
    }

    response = requests.get(FRED_API_URL, params=params)
    response.raise_for_status()

    data = response.json()
    observations = data.get("observations", [])

    df = pd.DataFrame(observations)
    df = df[["date", "value"]]
    df.columns = ["date", "fed_funds_rate"]

    # Convert to proper types
    df["date"] = pd.to_datetime(df["date"])
    df["fed_funds_rate"] = pd.to_numeric(df["fed_funds_rate"], errors="coerce")

    # Drop any missing values
    df = df.dropna()

    return df


def fetch_fed_funds_rate_no_key(start_date: str = "2015-01-01") -> pd.DataFrame:
    """
    Fetch federal funds rate data using pandas_datareader (no API key required).
    Falls back to a direct CSV download from FRED if pandas_datareader unavailable.

    Args:
        start_date: Start date for data retrieval (YYYY-MM-DD format)

    Returns:
        DataFrame with date and federal funds rate columns
    """
    try:
        import pandas_datareader.data as web

        start = pd.to_datetime(start_date)
        end = datetime.now()

        df = web.DataReader(FRED_SERIES_ID, "fred", start, end)
        df = df.reset_index()
        df.columns = ["date", "fed_funds_rate"]
        return df

    except ImportError:
        # Fallback: Download CSV directly from FRED
        csv_url = f"https://fred.stlouisfed.org/graph/fredgraph.csv?id={FRED_SERIES_ID}"
        df = pd.read_csv(csv_url)
        df.columns = ["date", "fed_funds_rate"]
        df["date"] = pd.to_datetime(df["date"])
        df["fed_funds_rate"] = pd.to_numeric(df["fed_funds_rate"], errors="coerce")
        df = df[df["date"] >= start_date]
        df = df.dropna()
        return df


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Fetch Federal Funds Rate data from FRED")
    parser.add_argument("--api-key", type=str, help="FRED API key (optional)")
    parser.add_argument("--start-date", type=str, default="2015-01-01",
                        help="Start date (YYYY-MM-DD)")
    parser.add_argument("--output", type=str, default="fed_funds_rate.csv",
                        help="Output CSV file path")

    args = parser.parse_args()

    print(f"Fetching Federal Funds Rate data from {args.start_date}...")

    if args.api_key:
        df = fetch_fed_funds_rate(args.api_key, args.start_date)
    else:
        print("No API key provided, using direct download method...")
        df = fetch_fed_funds_rate_no_key(args.start_date)

    print(f"Retrieved {len(df)} observations")
    print(f"Date range: {df['date'].min()} to {df['date'].max()}")
    print(f"Rate range: {df['fed_funds_rate'].min():.2f}% to {df['fed_funds_rate'].max():.2f}%")

    # Save to CSV
    df.to_csv(args.output, index=False)
    print(f"Saved to {args.output}")

    # Show sample
    print("\nSample data:")
    print(df.tail(10).to_string(index=False))


if __name__ == "__main__":
    main()
