import xml.etree.ElementTree as ET
import pandas as pd
import re
import requests
from sqlalchemy import create_engine

# 2024
# url = "https://www.sec.gov/Archives/edgar/data/1770373/000177037324000019/0001770373-24-000019.txt"

# 2025
url = "https://www.sec.gov/Archives/edgar/data/1770373/000177037325000051/0001770373-25-000051.txt"
# SEC requires a User-Agent header to avoid 403 Forbidden errors



headers = {'User-Agent': 'Mozilla/5.0 (Data Analysis Project)'}

print(f"Downloading data from {url}...")
response = requests.get(url, headers=headers)
raw_data = response.text

# Extract XML content if this is a full SEC submission text file
xml_match = re.search(r'<XML>(.*?)</XML>', raw_data, re.DOTALL | re.IGNORECASE)
if xml_match:
    print("Found XML block in submission, extracting...")
    raw_data = xml_match.group(1)

# 2. STRIP THE NAMESPACE
# This regex removes the xmlns="..." part from the root tag
clean_data = re.sub(r'\sxmlns="[^"]+"', '', raw_data, count=1)

# 3. Parse the cleaned XML
root = ET.fromstring(clean_data.strip())

# 4. Extract data
assets_list = []
for asset in root.findall('.//assets'):
    # This will now just be {'vehicleManufacturerName': 'Subaru', ...}
    asset_dict = {child.tag: child.text for child in asset}
    assets_list.append(asset_dict)

# 5. Convert to DataFrame and SQL
df = pd.DataFrame(assets_list)

# Clean up any potential leftover bracketed namespaces in column names 
# (Just in case some sub-elements had their own namespaces)
df.columns = [re.sub(r'\{.*\}', '', col) for col in df.columns]

# 1. Define Column Groups
# All columns are strings by default; we only need to specify the ones to change.

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
    'obligorCreditScore', 'remainingTermToMaturityNumber', 'currentDelinquencyStatus'
]

date_cols = [
    'reportingPeriodBeginningDate', 'reportingPeriodEndingDate', 
    'originationDate', 'loanMaturityDate', 'originalFirstPaymentDate', 
    'interestPaidThroughDate', 'zeroBalanceEffectiveDate'
]

# 2. Apply Conversions
# Use errors='coerce' to turn bad data/empty strings into NaN (NULL in SQL)

for col in float_cols:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')

for col in int_cols:
    if col in df.columns:
        # We use float first because Ints can't handle NaNs in older pandas
        df[col] = pd.to_numeric(df[col], errors='coerce')

for col in date_cols:
    if col in df.columns:
        # SEC dates are often MM-DD-YYYY or MM/YYYY
        df[col] = pd.to_datetime(df[col], errors='coerce')

# 3. Dump to SQL
engine = create_engine('sqlite:///cvna-2025.db')
df.to_sql('carvana_assets', engine, if_exists='replace', index=False)

print("Data types successfully mapped to SQL.")