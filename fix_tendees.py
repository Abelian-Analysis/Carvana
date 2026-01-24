import csv
import os

INPUT_FILE = "/home/tluka/cvna/tendees.csv"
OUTPUT_FILE = "/home/tluka/cvna/tendees_fixed.csv"

def fix_csv():
    if not os.path.exists(INPUT_FILE):
        print(f"Error: Input file {INPUT_FILE} not found.")
        return

    print(f"Reading from {INPUT_FILE}...")
    with open(INPUT_FILE, 'r', newline='') as infile, open(OUTPUT_FILE, 'w', newline='') as outfile:
        reader = csv.DictReader(infile)
        fieldnames = reader.fieldnames
        writer = csv.DictWriter(outfile, fieldnames=fieldnames)
        writer.writeheader()

        fixed_count = 0
        for row in reader:
            try:
                trigger = float(row['delinquency_trigger'])
                # If trigger is less than 2 (e.g. 0.02 instead of 2.0), it's likely a scaling error
                if trigger < 2.0:
                    row['delinquency_trigger'] = trigger * 100
                    row['delinq_60_plus'] = float(row['delinq_60_plus']) * 100
                    fixed_count += 1
            except ValueError:
                pass # Skip rows with invalid data if any
            
            writer.writerow(row)

    print(f"Done. Fixed {fixed_count} rows. Output written to {OUTPUT_FILE}")

if __name__ == "__main__":
    fix_csv()