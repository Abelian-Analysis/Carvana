import os
import time
import csv
import json
import google.generativeai as genai

# Configuration
INPUT_DIR = '10-D_filings'
OUTPUT_FILE = 'carvana_metrics_gemini.csv'
# Assumes GOOGLE_API_KEY is set in environment variables
API_KEY = os.environ.get("GOOGLE_API_KEY")

def extract_data_with_gemini(text, filename):
    """
    Sends the filing text to the Gemini API to extract structured data.
    """
    if not API_KEY:
        raise ValueError("GOOGLE_API_KEY environment variable is not set. Please export GOOGLE_API_KEY='your_key'.")

    genai.configure(api_key=API_KEY)
    
    # gemini-1.5-flash is efficient for large context tasks like document extraction
    model = genai.GenerativeModel('gemini-2.5-flash')

    prompt = f"""
    You are a financial data extraction assistant. 
    I am providing the text content of an SEC Form 10-D filing (Filename: {filename}).

    Please extract the following specific data points:
    1. **Trust Name**: The name of the issuing entity (e.g., Carvana Auto Receivables Trust 20XX-XX).
    2. **Ending Pool Balance**: The pool balance at the end of the collection period.
    3. **Receivables > 60 Days Delinquent**: The amount or percentage of receivables greater than 60 days delinquent.
    4. **Delinquency Trigger Level**: The delinquency trigger percentage.
    5. **Number of Receivables Extended**: The number of receivables extended during the collection period.
    6. **Principal Balance of Receivables Extended**: The principal balance of receivables extended during the collection period.
    7. **Number of Receivables 61-90 Days Delinquent**: The number of receivables that are 61-90 days delinquent.
    8. **Principal Balance of Receivables 61-90 Days Delinquent**: The principal balance of receivables that are 61-90 days delinquent.
    9. **Number of Receivables 91-120 Days Delinquent**: The number of receivables that are 91-120 days delinquent.
    10. **Principal Balance of Receivables 91-120 Days Delinquent**: The principal balance of receivables that are 91-120 days delinquent.
    11. **Collection Period End Date**: The date of the last day of the collection period (e.g., if the period is "January 1, 2023 through January 31, 2023", extract "January 31, 2023").

    Return the result strictly as a JSON object with the following keys:
    {{
        "trust_name": "string",
        "ending_pool_balance": "number",
        "delinq_60_plus": "number",
        "delinquency_trigger": "number",
        "extensions_count": "number",
        "extensions_balance": "number",
        "delinq_61_90_count": "number",
        "delinq_61_90_balance": "number",
        "delinq_91_120_count": "number",
        "delinq_91_120_balance": "number",
        "collection_period_end_date": "date-string (MM-DD-YYYY)"
    }}

    If a value is not found, set it to null. Do not include any markdown formatting (like ```json) in your response.
    """

    # Retry logic for quota handling
    max_retries = 5
    current_delay = .05

    for attempt in range(max_retries):
        try:
            # Generate content
            response = model.generate_content([prompt, text])
            return response.text
        except Exception as e:
            error_msg = str(e).lower()
            if "429" in error_msg or "quota" in error_msg or "resource exhausted" in error_msg:
                print(f"Quota exceeded processing {filename}. Waiting {current_delay}s before retry (Attempt {attempt+1}/{max_retries})...")
                time.sleep(current_delay)
                current_delay *= 1.5  # Exponential backoff
            else:
                print(f"Error calling Gemini API for {filename}: {e}")
                if "404" in str(e):
                    print("Listing available models to help debug:")
                    for m in genai.list_models():
                        if 'generateContent' in m.supported_generation_methods:
                            print(f" - {m.name}")
                return None
    return None

def clean_json_response(response_text):
    """
    Cleans the LLM response to ensure valid JSON.
    """
    if not response_text:
        return None
    
    clean = response_text.strip()
    # Strip markdown code blocks if the model includes them
    if clean.startswith("```json"):
        clean = clean[7:]
    if clean.startswith("```"):
        clean = clean[3:]
    if clean.endswith("```"):
        clean = clean[:-3]
    
    return clean.strip()

def main():
    if not os.path.exists(INPUT_DIR):
        print(f"Input directory '{INPUT_DIR}' not found.")
        return

    # Filter for text and html files
    files = [f for f in os.listdir(INPUT_DIR) if f.endswith(('.txt', '.html', '.htm'))]
    print(f"Found {len(files)} files in {INPUT_DIR}. Starting extraction...")

    # Define CSV headers
    headers = ['filename', 'trust_name', 'collection_period_end_date', 'ending_pool_balance', 'delinq_60_plus', 'delinquency_trigger', 'extensions_count', 'extensions_balance', 'delinq_61_90_count', 'delinq_61_90_balance', 'delinq_91_120_count', 'delinq_91_120_balance']

    with open(OUTPUT_FILE, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=headers)
        writer.writeheader()

        for i, filename in enumerate(files):
            filepath = os.path.join(INPUT_DIR, filename)
            print(f"Processing [{i+1}/{len(files)}]: {filename}")

            try:
                with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()

                # Call the API
                raw_response = extract_data_with_gemini(content, filename)
                clean_response = clean_json_response(raw_response)

                if clean_response:
                    try:
                        data = json.loads(clean_response)
                        row = {
                            'filename': filename,
                            'trust_name': data.get('trust_name'),
                            'collection_period_end_date': data.get('collection_period_end_date'),
                            'ending_pool_balance': data.get('ending_pool_balance'),
                            'delinq_60_plus': data.get('delinq_60_plus'),
                            'delinquency_trigger': data.get('delinquency_trigger'),
                            'extensions_count': data.get('extensions_count'),
                            'extensions_balance': data.get('extensions_balance'),
                            'delinq_61_90_count': data.get('delinq_61_90_count'),
                            'delinq_61_90_balance': data.get('delinq_61_90_balance'),
                            'delinq_91_120_count': data.get('delinq_91_120_count'),
                            'delinq_91_120_balance': data.get('delinq_91_120_balance')
                        }
                        writer.writerow(row)
                        # Flush to save progress
                        csvfile.flush()
                    except json.JSONDecodeError:
                        print(f"Failed to parse JSON for {filename}. Raw response: {raw_response[:50]}...")
                else:
                    print(f"No response received for {filename}")

            except Exception as e:
                print(f"Error processing file {filename}: {e}")

            # Rate limiting: Sleep to respect API limits (adjust based on your quota)
            # Increased wait time to avoid hitting quota limits
            time.sleep(45)

    print(f"Extraction complete. Data saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    main()