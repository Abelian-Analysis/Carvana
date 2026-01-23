import os
import sqlite3
import re
from bs4 import BeautifulSoup
from datetime import datetime
import pandas as pd

# Configuration
DB_NAME = 'carvana_historical_trends.db'
INPUT_DIR = '10-D_filings'
DEBUG_MODE = False  # Set to False to suppress debug messages

def init_db():
    """Initialize the SQLite database and table."""
    conn = sqlite3.connect(DB_NAME)
    c = conn.cursor()
    c.execute('''
        CREATE TABLE IF NOT EXISTS servicer_metrics (
            trust_name TEXT,
            filing_date DATE,
            ext_count INTEGER,
            ext_balance REAL,
            pool_balance REAL,
            delinq_60_plus REAL,
            PRIMARY KEY (trust_name, filing_date)
        )
    ''')
    conn.commit()
    return conn

def clean_number(text):
    """Cleans numeric strings, handling currency, percentages, and negative parentheses."""
    if not text or text.strip() in ['-', 'N/A', 'None']:
        return 0.0
    
    clean = text.strip()
    
    # Handle negative numbers in parentheses e.g., (1,234.56)
    is_negative = False
    if clean.startswith('(') and clean.endswith(')'):
        is_negative = True
        clean = clean[1:-1]
    elif clean.startswith('-'):
        is_negative = True
        clean = clean[1:]
    
    # Remove currency, commas, percent
    clean = re.sub(r'[$,%]', '', clean)
    
    try:
        val = float(clean)
        return -val if is_negative else val
    except ValueError:
        return 0.0

def parse_10d_file(filepath):
    """Parses a single 10-D file for metrics."""
    if DEBUG_MODE:
        print(f"DEBUG: Parsing file: {filepath}")
        
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()

    # 1. Extract Metadata (Trust Name, Date) from SGML Header
    trust_name = "Unknown Trust"
    trust_match = re.search(r'COMPANY CONFORMED NAME:\s+(.*)', content)
    if trust_match:
        trust_name = trust_match.group(1).strip()
    
    filing_date = None
    date_match = re.search(r'FILED AS OF DATE:\s+(\d{8})', content)
    if date_match:
        try:
            filing_date = datetime.strptime(date_match.group(1), '%Y%m%d').date()
        except ValueError:
            pass
            
    # Fallback: Try to parse date from filename if SGML header fails
    if not filing_date:
        filename = os.path.basename(filepath)
        # Expected format: CIK_YYYY-MM-DD_...
        match = re.search(r'\d+_(\d{4}-\d{2}-\d{2})_', filename)
        if match:
            try:
                filing_date = datetime.strptime(match.group(1), '%Y-%m-%d').date()
            except ValueError:
                pass

    if not filing_date:
        print(f"Skipping {os.path.basename(filepath)}: No filing date found.")
        return None

    if DEBUG_MODE:
        print(f"DEBUG: Extracted Trust: '{trust_name}', Date: {filing_date}")

    # 2. Parse HTML Content
    soup = BeautifulSoup(content, 'html.parser')
    # Get full text with space separator to handle table cells and line breaks
    full_text = soup.get_text(separator=' ', strip=True)
    # Normalize whitespace (collapse multiple spaces to one)
    full_text = " ".join(full_text.split())

    def extract_value(pattern):
        """Extracts a value using a regex pattern from the full text."""
        match = re.search(pattern, full_text, re.IGNORECASE)
        if match:
            val_str = match.group(1)
            if DEBUG_MODE:
                print(f"DEBUG: Matched '{pattern}': {val_str}")
            return clean_number(val_str)
        
        if DEBUG_MODE:
            print(f"DEBUG: No match for '{pattern}'")
        return 0.0

    # 3. Extract Specific Metrics using tailored regexes
    
    # Number of receivables extended
    # Pattern: ... (A) (112) 3 ...
    ext_count = int(extract_value(r"Number of receivables extended.*?\(A\).*?\(\d+\)\s*([\d,]+)"))

    # Principal Balance of receivables extended
    # Pattern: ... (B) (113) 84,738.89 ...
    ext_balance = extract_value(r"Principal Balance of receivables extended.*?\(B\).*?\(\d+\)\s*([\d,.]+)")

    # Pool Balance as of the beginning of the Collection Period
    # Pattern: ... (D) (115) 910,804,097.98 ...
    pool_balance = extract_value(r"Pool Balance as of the beginning of the Collection Period.*?\(D\).*?\(\d+\)\s*([\d,.]+)")

    # Receivables greater than 60 days delinquent
    # Pattern: ... (102) 0.01% ...
    delinq_60_plus = extract_value(r"Receivables greater than 60 days delinquent.*?\(\d+\)\s*([\d,.]+)%?")

    return {
        'trust_name': trust_name,
        'filing_date': filing_date,
        'ext_count': ext_count,
        'ext_balance': ext_balance,
        'pool_balance': pool_balance,
        'delinq_60_plus': delinq_60_plus
    }

def main():
    if not os.path.exists(INPUT_DIR):
        print(f"Directory '{INPUT_DIR}' not found. Please run get-tendees.py first.")
        return

    conn = init_db()
    cursor = conn.cursor()
    
    files = [f for f in os.listdir(INPUT_DIR) if f.endswith('.txt') or f.endswith('.html') or f.endswith('.htm')]
    print(f"Found {len(files)} files in {INPUT_DIR}...")

    count = 0
    for filename in files:
        filepath = os.path.join(INPUT_DIR, filename)
        try:
            data = parse_10d_file(filepath)
            if data:
                cursor.execute('''
                    INSERT INTO servicer_metrics 
                    (trust_name, filing_date, ext_count, ext_balance, pool_balance, delinq_60_plus)
                    VALUES (:trust_name, :filing_date, :ext_count, :ext_balance, :pool_balance, :delinq_60_plus)
                    ON CONFLICT(trust_name, filing_date) DO UPDATE SET
                        ext_count=excluded.ext_count,
                        ext_balance=excluded.ext_balance,
                        pool_balance=excluded.pool_balance,
                        delinq_60_plus=excluded.delinq_60_plus
                ''', data)
                count += 1
                if count % 10 == 0:
                    print(f"Processed {count} files...")
        except Exception as e:
            print(f"Error processing {filename}: {e}")

    conn.commit()
    conn.close()
    print(f"Successfully processed {count} files. Data saved to {DB_NAME}.")

    # Verification Query
    print("\n--- Sample Data Verification ---")
    conn = sqlite3.connect(DB_NAME)
    try:
        df = pd.read_sql_query("SELECT * FROM servicer_metrics ORDER BY filing_date DESC", conn)
        print(df)
    except Exception as e:
        print(f"Error reading database: {e}")
    finally:
        conn.close()

if __name__ == "__main__":
    main()
