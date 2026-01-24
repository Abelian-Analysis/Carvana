import xml.etree.ElementTree as ET
import pandas as pd
import re
import requests
import time
from sqlalchemy import create_engine

# Configuration
TRUST_CIKS = [
    '1841341', '1843657', '1843627', '1845213', '1903763', '1903753', '1903756',
    '1999671', '1999856', '1999854', '2037956', '2037955', '2037953', '2037952'
]
USER_AGENT = 'Tyler Lukasiewicz (kablaa@abelian-labs.com)'
DB_FILE = 'cvna_loan_tape.db'

def get_latest_absee_url(cik):
    """Fetches the URL for the most recent ABS-EE filing for a given CIK."""
    cik_padded = str(cik).zfill(10)
    url = f"https://data.sec.gov/submissions/CIK{cik_padded}.json"
    headers = {'User-Agent': USER_AGENT}
    
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        data = response.json()
        
        trust_name = data.get('name', f"Trust {cik}")
        
        filings = data.get('filings', {}).get('recent', {})
        if not filings:
            return None, None, None
            
        df = pd.DataFrame(filings)
        # Filter for ABS-EE forms
        df_abs = df[df['form'] == 'ABS-EE'].copy()
        
        if df_abs.empty:
            return None, None, None
            
        # Sort by filing date descending and take the first one
        df_abs['filingDate'] = pd.to_datetime(df_abs['filingDate'])
        latest = df_abs.sort_values('filingDate', ascending=False).iloc[0]
        
        # Construct URL
        acc_no = latest['accessionNumber']
        acc_no_clean = acc_no.replace('-', '')
        file_url = f"https://www.sec.gov/Archives/edgar/data/{int(cik)}/{acc_no_clean}/{acc_no}.txt"
        
        return file_url, latest['filingDate'].strftime('%Y-%m-%d'), trust_name
    except Exception as e:
        print(f"Error fetching submissions for {cik}: {e}")
        return None, None, None

def process_trusts():
    engine = create_engine(f'sqlite:///{DB_FILE}')
    headers = {'User-Agent': USER_AGENT}
    
    # Flag to handle table creation (replace first, then append)
    first_run = True

    for cik in TRUST_CIKS:
        print(f"\nProcessing Trust CIK: {cik}")
        url, date, trust_name = get_latest_absee_url(cik)
        
        if not url:
            print(f"No ABS-EE filing found for CIK {cik}")
            continue
            
        print(f"Found filing from {date} for {trust_name}. Downloading from {url}...")
        
        try:
            response = requests.get(url, headers=headers)
            response.raise_for_status()
            raw_data = response.text
            
            # Extract XML content if this is a full SEC submission text file
            xml_match = re.search(r'<XML>(.*?)</XML>', raw_data, re.DOTALL | re.IGNORECASE)
            if xml_match:
                print("Found XML block in submission, extracting...")
                raw_data = xml_match.group(1)

            # STRIP THE NAMESPACE
            clean_data = re.sub(r'\sxmlns="[^"]+"', '', raw_data, count=1)

            # Parse the cleaned XML
            root = ET.fromstring(clean_data.strip())

            # Extract data
            assets_list = []
            for asset in root.findall('.//assets'):
                asset_dict = {child.tag: child.text for child in asset}
                # Add metadata
                asset_dict['trust_cik'] = cik
                asset_dict['filing_date'] = date
                asset_dict['trust_name'] = trust_name
                assets_list.append(asset_dict)

            if not assets_list:
                print("No assets found in XML.")
                continue

            df = pd.DataFrame(assets_list)
            df.columns = [re.sub(r'\{.*\}', '', col) for col in df.columns]

            if first_run:
                print("\n--- DEBUG: Raw Data Sample (First 5 rows) ---")
                cols_to_check = ['reportingPeriodBeginningLoanBalanceAmount', 'paymentToIncomePercentage', 'vehicleValueAmount']
                print(df[[c for c in cols_to_check if c in df.columns]].head())
                print("---------------------------------------------\n")

            # Define Column Groups
            float_cols = [
                'originalLoanAmount', 'originalInterestRatePercentage', 'vehicleValueAmount',
                'paymentToIncomePercentage', 'reportingPeriodBeginningLoanBalanceAmount',
                'nextReportingPeriodPaymentAmountDue', 'reportingPeriodInterestRatePercentage',
                'nextInterestRatePercentage', 'otherAssessedUncollectedServicerFeeAmount',
                'scheduledInterestAmount', 'scheduledPrincipalAmount', 'otherPrincipalAdjustmentAmount',
                'reportingPeriodActualEndBalanceAmount', 'reportingPeriodScheduledPaymentAmount',
                'totalActualAmountPaid', 'actualInterestCollectedAmount', 'actualPrincipalCollectedAmount',
                'actualOtherCollectedAmount', 'servicerAdvancedAmount'
            ]

            int_cols = [
                'assetNumber', 'originalLoanTerm', 'gracePeriodNumber', 'vehicleModelYear',
                'obligorCreditScore', 'remainingTermToMaturityNumber', 'currentDelinquencyStatus',
                'obligorIncomeVerificationLevelCode', 'obligorEmploymentVerificationCode'
            ]

            date_cols = [
                'reportingPeriodBeginningDate', 'reportingPeriodEndingDate', 
                'originationDate', 'loanMaturityDate', 'originalFirstPaymentDate', 
                'interestPaidThroughDate', 'zeroBalanceEffectiveDate'
            ]

            # Apply Conversions
            for col in float_cols:
                if col in df.columns:
                    df[col] = pd.to_numeric(df[col], errors='coerce')

            for col in int_cols:
                if col in df.columns:
                    df[col] = pd.to_numeric(df[col], errors='coerce')

            for col in date_cols:
                if col in df.columns:
                    df[col] = pd.to_datetime(df[col], errors='coerce')

            # Dump to SQL
            # Replace table on first run, then append
            if_exists_action = 'replace' if first_run else 'append'
            df.to_sql('carvana_assets', engine, if_exists=if_exists_action, index=False)
            print(f"Saved {len(df)} rows to SQL ({if_exists_action}).")
            
            first_run = False
            
            # Respect SEC rate limits
            time.sleep(0.2)

        except Exception as e:
            print(f"Failed to process {cik}: {e}")

    print(f"\nAll done. Data saved to {DB_FILE}")

if __name__ == "__main__":
    process_trusts()