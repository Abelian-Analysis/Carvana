# Load libraries
library(RSQLite)
library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Connect and Fetch Data
con <- dbConnect(RSQLite::SQLite(), "tendees.db")
trends_df <- dbReadTable(con, "servicer_metrics")
dbDisconnect(con)

# 2. Process and Calculate Ratios
# We calculate the ratio and filter out bad/zero balances to keep the graph clean
trends_df <- trends_df %>%
  mutate(
    filing_date = as.Date(filing_date),
    ext_ratio_pct = (ext_balance / pool_balance) * 100
  ) %>%
  filter(pool_balance > 0 & !is.na(ext_ratio_pct)) %>%
  arrange(trust_name, filing_date)

# 3. Create the Visualization
p <- ggplot(trends_df, aes(x = filing_date, y = ext_ratio_pct)) +
  # Main trend line
  geom_line(color = "#00d1b2", size = 1) + 
  # Shaded area beneath the line to emphasize volume
  geom_area(fill = "#00d1b2", alpha = 0.2) +
  # Points to show individual reporting periods
  geom_point(color = "#00d1b2", size = 1.5) +
  # Facet by Trust - allows side-by-side comparison of different years
  facet_wrap(~trust_name, scales = "free_x") +
  # Aesthetics and Labels
  labs(
    title = "Historical Pattern of Loan Extensions: Carvana Prime ABS",
    subtitle = "Spikes in Extension Ratios often correlate with proximity to Delinquency Triggers",
    x = "Reporting Period",
    y = "Extension Ratio (% of Pool Balance)",
    caption = "Source: SEC EDGAR 10-D Reports (Parsed via Python/SQLite)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# Display the plot
print(p)

# Save for your whistleblower filing
ggsave("extension_spikes_historical.png", p, width = 12, height = 8, dpi = 300)

# Sort by the highest extension ratios to find the most aggressive months
high_stress_months <- trends_df %>%
  arrange(desc(ext_ratio_pct)) %>%
  select(trust_name, filing_date, ext_count, ext_balance, pool_balance, ext_ratio_pct) %>%
  head(20)

print(high_stress_months)

# Calculate the 'Honest' Delinquency Rate
trends_df <- trends_df %>%
  mutate(
    # Reported Rate: What they tell the SEC
    reported_delinq_pct = (delinq_60_plus / pool_balance) * 100,
    
    # Adjusted Rate: What it would be without the 'Extension Shield'
    # Logic: Delinquent Balance + Extended Balance
    adjusted_delinq_pct = ((delinq_60_plus + ext_balance) / pool_balance) * 100,
    
    # Trigger Level: 2.20% is the standard Year 1 ARR trigger for Prime CART deals
    trigger_level = 2.20
  )

# Create the Comparison Graph
ggplot(trends_df, aes(x = filing_date)) +
  # Reported Delinquency (The 'Safe' Number)
  geom_line(aes(y = reported_delinq_pct, color = "Reported Delinquency"), size = 1) +
  # Adjusted Delinquency (The 'Real' Number)
  geom_line(aes(y = adjusted_delinq_pct, color = "Adjusted (Real) Rate"), linetype = "dashed", size = 1) +
  # The ARR Trigger Line
  geom_hline(yintercept = 2.20, color = "red", linetype = "dotted", size = 1) +
  facet_wrap(~trust_name, scales = "free") +
  scale_color_manual(values = c("Reported Delinquency" = "#00d1b2", "Adjusted (Real) Rate" = "#ff3860")) +
  labs(title = "Carvana ABS: Reported vs. Adjusted Delinquency",
       subtitle = "Dashed red line indicates the 2.20% ARR Audit Trigger",
       y = "Percentage of Pool Balance", x = "Filing Date") +
  theme_minimal()