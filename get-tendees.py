import requests
import pandas as pd
import os
import time
from datetime import datetime, timedelta

# Configuration - THE CORE CHANGE
# Use the Trust CIKs we identified to ensure you only get the 'Prime' data
TRUST_CIKS = [
    '0002037952', # 2025-P4
    '0001999854', # 2024-P4
    '0001976115', # 2023-P4
    '0001903754', # 2023-P1 (Substitute for 2022-P4)
    '0001903753', # 2022-P2 (Crisis Peak)
    '0001845213'  # 2021-P4 (Baseline)
]

USER_AGENT = 'Tyler Lukasiewicz (tyler@example.com)' # SEC requires Name/Email
OUTPUT_DIR = '10-D_filings'
LOOKBACK_YEARS = 5

def get_sec_submissions(cik):
    # Ensure CIK is 10 digits with leading zeros for the API
    cik_padded = str(cik).zfill(10)
    url = f"https://data.sec.gov/submissions/CIK{cik_padded}.json"
    headers = {'User-Agent': USER_AGENT}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"Error fetching CIK {cik}: {e}")
        return None

def main():
    if not os.path.exists(OUTPUT_DIR): os.makedirs(OUTPUT_DIR)
    cutoff_date = datetime.now() - timedelta(days=LOOKBACK_YEARS * 365)

    for cik in TRUST_CIKS:
        print(f"\n--- Processing Trust: {cik} ---")
        data = get_sec_submissions(cik)
        if not data: continue

        filings = data.get('filings', {}).get('recent', {})
        df = pd.DataFrame(filings)
        
        # Filter for 10-D forms and date
        df_10d = df[(df['form'] == '10-D')].copy()
        df_10d['filingDate'] = pd.to_datetime(df_10d['filingDate'])
        df_10d = df_10d[df_10d['filingDate'] >= cutoff_date]

        for _, row in df_10d.iterrows():
            acc_no = row['accessionNumber']
            acc_no_clean = acc_no.replace('-', '')
            
            # THE CRITICAL FIX: The Archives URL needs the raw CIK without leading zeros
            cik_path = str(int(cik)) 
            
            # Construct the URL to the primary document (usually a .htm or .txt)
            # This is safer than the .txt full submission for parsing tables later
            primary_doc = row['primaryDocument']
            file_url = f"https://www.sec.gov/Archives/edgar/data/{cik_path}/{acc_no_clean}/{primary_doc}"
            
            filename = f"{OUTPUT_DIR}/{cik}_{row['filingDate'].date()}_10D.html"

            if not os.path.exists(filename):
                print(f"Downloading {cik} for {row['filingDate'].date()}...")
                success = download_file(file_url, filename)
                time.sleep(0.15) # Respect SEC 10 requests/sec limit

# [Include your existing download_file function here]
def download_file(url, filepath):
    """Downloads a file from a URL to the specified path."""
    headers = {'User-Agent': USER_AGENT}
    try:
        response = requests.get(url, headers=headers, stream=True)
        response.raise_for_status()
        with open(filepath, 'wb') as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f"Downloaded: {filepath}")
        return True
    except requests.exceptions.RequestException as e:
        print(f"Failed to download {url}: {e}")
        return False

def main():
    # Create output directory if it doesn't exist
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    # Calculate the cutoff date
    cutoff_date = datetime.now() - timedelta(days=LOOKBACK_YEARS * 365)
    print(f"Fetching 10-D forms filed after {cutoff_date.strftime('%Y-%m-%d')}...")

    for cik in TRUST_CIKS:
        print(f"Processing CIK: {cik}")
        data = get_sec_submissions(cik)
        
        if not data:
            continue

        # 'filings' -> 'recent' contains the lists of filing data
        filings = data.get('filings', {}).get('recent', {})
        if not filings:
            print(f"No recent filings found for CIK {cik}")
            continue

        # Convert to DataFrame for easier manipulation
        df = pd.DataFrame(filings)
        
        # Filter for 10-D forms
        if 'form' not in df.columns:
            print(f"No form data found for CIK {cik}")
            continue
            
        df_10d = df[df['form'] == '10-D'].copy()
        
        # Convert filingDate to datetime and filter
        df_10d['filingDate'] = pd.to_datetime(df_10d['filingDate'])
        df_10d = df_10d[df_10d['filingDate'] >= cutoff_date]

        print(f"Found {len(df_10d)} 10-D filings for CIK {cik} in the last {LOOKBACK_YEARS} years.")

        for _, row in df_10d.iterrows():
            accession_number = row['accessionNumber']
            accession_no_dash = accession_number.replace('-', '')
            filing_date = row['filingDate'].strftime('%Y-%m-%d')
            
            # Construct the URL for the full text submission file
            # This matches the format used in get_data.py which allows for XML extraction
            # Format: https://www.sec.gov/Archives/edgar/data/{cik}/{accession_no_dash}/{accession_number}.txt
            cik_int = int(cik) # Remove leading zeros for the URL path
            file_url = f"https://www.sec.gov/Archives/edgar/data/{cik_int}/{accession_no_dash}/{accession_number}.txt"
            
            filename = f"{OUTPUT_DIR}/{cik}_{filing_date}_10-D_{accession_number}.txt"
            
            # Check if file already exists to avoid re-downloading
            if not os.path.exists(filename):
                success = download_file(file_url, filename)
                if success:
                    # Be polite to the SEC server (limit is 10 requests/sec, but good to pace)
                    time.sleep(0.15)
            else:
                print(f"Skipping existing file: {filename}")

if __name__ == "__main__":
    main()