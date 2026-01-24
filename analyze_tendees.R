# analyze_tendees.R

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

# Configuration
INPUT_FILE <- "/home/tluka/cvna/tendees_fixed.csv"
OUTPUT_DIR <- "/home/tluka/cvna/plots"

# Ensure output directory exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(sprintf("Created output directory: %s", OUTPUT_DIR))
}

# Read the CSV file
if (!file.exists(INPUT_FILE)) {
  stop(sprintf("Input file not found: %s", INPUT_FILE))
}

message(sprintf("Reading data from %s...", INPUT_FILE))
df <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

# Check for rows with extensions_count > 300 as requested
high_extensions <- subset(df, as.numeric(extensions_count) > 300)
if (nrow(high_extensions) > 0) {
  message("Found rows with extensions_count > 300:")
  print(high_extensions)
} else {
  message("No rows with extensions_count > 300 found in the dataset.")
}

# Preprocess the data
# 1. Parse dates (MM-DD-YYYY format)
# 2. Ensure metrics are numeric
# 3. Trim whitespace from trust names
df_clean <- df %>%
  mutate(
    date = mdy(collection_period_end_date),
    delinq_60_plus = as.numeric(delinq_60_plus),
    delinquency_trigger = as.numeric(delinquency_trigger),
    extensions_count = as.numeric(extensions_count),
    delinq_plus_extensions = (as.numeric(delinq_61_90_balance) + 
                              as.numeric(delinq_91_120_balance) + 
                              as.numeric(extensions_balance)) / 
                              as.numeric(ending_pool_balance) * 100,
    trust_name = str_trim(trust_name)
  ) %>%
  filter(!is.na(date)) %>%
  # Deduplicate based on trust and date to prevent stacking bars
  distinct(trust_name, date, .keep_all = TRUE) %>%
  arrange(date)

# Identify unique trusts
trusts <- unique(df_clean$trust_name)
message(sprintf("Found %d unique trusts to process.", length(trusts)))

# Generate plots for each trust
for (trust in trusts) {
  # Filter data for the specific trust
  trust_data <- df_clean %>%
    filter(trust_name == trust)
  
  # Skip if no data available
  if (nrow(trust_data) == 0) next
  
  message(sprintf("Generating plot for: %s", trust))
  
  # Calculate scaling factor for secondary axis
  max_pct <- max(c(trust_data$delinq_60_plus, trust_data$delinquency_trigger, trust_data$delinq_plus_extensions), na.rm = TRUE)
  max_count <- max(trust_data$extensions_count, na.rm = TRUE)
  
  if (is.na(max_pct) || max_pct == 0) max_pct <- 1
  if (is.na(max_count) || max_count == 0) max_count <- 1
  coeff <- max_count / max_pct

  # Create the plot
  p <- ggplot(trust_data, aes(x = date)) +
    # Plot Extensions Count (Bar) on secondary axis scale
    geom_bar(aes(y = extensions_count / coeff, fill = "Extensions Count"), stat = "identity", alpha = 0.3, width = 20) +
    # Plot 60+ Days Delinquency
    geom_line(aes(y = delinq_60_plus, color = "60+ Days Delinquency"), size = 1) +
    geom_point(aes(y = delinq_60_plus, color = "60+ Days Delinquency"), size = 2) +
    # Plot Delinquencies + Extensions
    geom_line(aes(y = delinq_plus_extensions, color = "Delinquencies + Extensions"), size = 1) +
    geom_point(aes(y = delinq_plus_extensions, color = "Delinquencies + Extensions"), size = 2) +
    # Plot Delinquency Trigger
    geom_line(aes(y = delinquency_trigger, color = "Delinquency Trigger"), linetype = "dashed", size = 1) +
    # Custom colors
    scale_color_manual(values = c(
      "60+ Days Delinquency" = "#1f77b4", # Blue
      "Delinquencies + Extensions" = "#2ca02c", # Green
      "Delinquency Trigger" = "#d62728"   # Red
    )) +
    scale_fill_manual(values = c(
      "Extensions Count" = "black"
    )) +
    scale_y_continuous(
      name = "Percentage (%)",
      sec.axis = sec_axis(~ . * coeff, name = "Number of Extensions")
    ) +
    # Labels and formatting
    labs(
      title = paste("Delinquency Metrics:", trust),
      subtitle = "60+ Days Delinquency vs. Trigger Level",
      x = "Collection Period End Date",
      color = "Metric",
      fill = "Count",
      caption = paste("Source:", basename(INPUT_FILE))
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")
  
  # Construct a safe filename
  safe_name <- str_replace_all(trust, "[^a-zA-Z0-9]", "_")
  filename <- file.path(OUTPUT_DIR, paste0(safe_name, "_delinquency.png"))
  
  # Save the plot
  ggsave(filename, plot = p, width = 10, height = 6, bg = "white")
}

message("Processing complete. Check the output directory for images.")