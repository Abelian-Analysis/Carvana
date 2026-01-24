library(RSQLite)
library(ggplot2)
library(dplyr)
library(stringr)

# Set theme to minimal (white background)
theme_set(theme_minimal())

# Create output directories
ltv_dir <- "plots/ltv_distros"
pti_dir <- "plots/pti_distros"
rate_dir <- "plots/rate_distros"
trend_dir <- "plots/trends"

if (!dir.exists(ltv_dir)) dir.create(ltv_dir, recursive = TRUE)
if (!dir.exists(pti_dir)) dir.create(pti_dir, recursive = TRUE)
if (!dir.exists(rate_dir)) dir.create(rate_dir, recursive = TRUE)
if (!dir.exists(trend_dir)) dir.create(trend_dir, recursive = TRUE)

# Connect to the database
db_path <- "cvna_loan_tape.db"
if (!file.exists(db_path)) {
  stop(paste("Database file not found:", db_path))
}
conn <- dbConnect(RSQLite::SQLite(), db_path)

# Check if table exists
if (!dbExistsTable(conn, "carvana_assets")) {
  dbDisconnect(conn)
  stop("Table 'carvana_assets' not found in database.")
}

# Get list of unique trusts
trusts_df <- dbGetQuery(conn, "SELECT DISTINCT trust_name FROM carvana_assets")
trusts <- trusts_df$trust_name

cat(sprintf("Found %d trusts to process.\n", length(trusts)))

# Initialize storage for summary stats
trust_stats <- data.frame(
  trust_name = character(),
  avg_ltv = numeric(),
  pct_underwater = numeric(),
  avg_pti = numeric(),
  pct_high_pti_10 = numeric(),
  pct_high_pti_15 = numeric(),
  pct_high_pti_20 = numeric(),
  avg_rate = numeric(),
  # Regression Stats: Credit vs PTI
  reg_pti_slope = numeric(),
  reg_pti_intercept = numeric(),
  reg_pti_r2 = numeric(),
  reg_pti_corr = numeric(),
  # Regression Stats: Credit vs LTV
  reg_ltv_slope = numeric(),
  reg_ltv_intercept = numeric(),
  reg_ltv_r2 = numeric(),
  reg_ltv_corr = numeric(),
  # Regression Stats: Credit vs Rate
  reg_rate_slope = numeric(),
  reg_rate_intercept = numeric(),
  reg_rate_r2 = numeric(),
  reg_rate_corr = numeric(),
  # Danger Zones
  pct_prime_high_pti = numeric(),
  pct_prime_high_rate = numeric(),
  stringsAsFactors = FALSE
)

# Process each trust
for (trust in trusts) {
  cat(sprintf("\nProcessing Trust: %s\n", trust))
  
  # Variables to hold current trust averages
  curr_ltv <- NA
  curr_underwater_pct <- NA
  curr_pti <- NA
  curr_high_pti_10 <- NA
  curr_high_pti_15 <- NA
  curr_high_pti_20 <- NA
  curr_rate <- NA
  
  # Regression & Danger Zone placeholders
  r_pti_slope <- NA; r_pti_int <- NA; r_pti_r2 <- NA; r_pti_corr <- NA
  r_ltv_slope <- NA; r_ltv_int <- NA; r_ltv_r2 <- NA; r_ltv_corr <- NA
  r_rate_slope <- NA; r_rate_int <- NA; r_rate_r2 <- NA; r_rate_corr <- NA
  
  curr_prime_high_pti <- NA
  curr_prime_high_rate <- NA
  
  # Fetch data for the specific trust
  query <- "SELECT * FROM carvana_assets WHERE trust_name = ?"
  data <- dbGetQuery(conn, query, params = list(trust))
  
  if (nrow(data) == 0) {
    cat("  No data found.\n")
    next
  }
  
  # Filter out paid-off loans (Zero Balance)
  if ("reportingPeriodBeginningLoanBalanceAmount" %in% names(data)) {
    n_orig <- nrow(data)
    data <- data %>% 
      filter(reportingPeriodBeginningLoanBalanceAmount > 0.01)
    
    n_dropped <- n_orig - nrow(data)
    if (n_dropped > 0) {
      cat(sprintf("  Dropping %d paid-off loans (zero balance).\n", n_dropped))
    }
  }

  # Convert date columns
  if ("reportingPeriodBeginningDate" %in% names(data)) {
    data$reportingPeriodBeginningDate <- as.Date(data$reportingPeriodBeginningDate)
  }
  if ("originationDate" %in% names(data)) {
    data$originationDate <- as.Date(data$originationDate)
  }
  
  # Create safe filename prefix
  safe_name <- str_replace_all(trust, "[^a-zA-Z0-9]", "_")
  
  # --- LTV Analysis ---
  if ("vehicleValueAmount" %in% names(data) && "reportingPeriodBeginningLoanBalanceAmount" %in% names(data)) {
    
    # Calculate LTV (Current Balance / Original Value)
    data$ltvRatio <- (data$reportingPeriodBeginningLoanBalanceAmount / data$vehicleValueAmount) * 100
    
    # Calculate percentages of underwater loans (> 100% LTV)
    if (sum(!is.na(data$ltvRatio)) > 0) {
      curr_underwater_pct <- sum(data$ltvRatio > 100, na.rm = TRUE) / sum(!is.na(data$ltvRatio)) * 100
    }
    
    # Filter for plot
    ltv_plot_data <- data %>%
      filter(!is.na(ltvRatio) & ltvRatio <= 200)

    if (nrow(ltv_plot_data) > 0) {
      curr_ltv <- mean(ltv_plot_data$ltvRatio, na.rm = TRUE)
      p_ltv <- ggplot(ltv_plot_data, aes(x = ltvRatio)) +
        geom_histogram(bins = 50, fill = "purple", color = "black", alpha = 0.7) +
        geom_vline(xintercept = 100, color = "red", linetype = "dashed", size = 1) +
        labs(
          title = paste("LTV Distribution:", trust),
          subtitle = "Red line indicates 100% LTV (Underwater)",
          x = "LTV Ratio (%)",
          y = "Frequency"
        ) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        
      filename <- file.path(ltv_dir, paste0(safe_name, "_ltv.png"))
      ggsave(filename, p_ltv, width = 10, height = 6, bg = "white")
      cat("  Saved LTV plot:", filename, "\n")
    }
  }
  
  # --- PTI Analysis ---
  if ("paymentToIncomePercentage" %in% names(data)) {
    # Filter out 0 PTI (likely missing/invalid data)
    pti_plot_data <- data %>%
      filter(!is.na(paymentToIncomePercentage) & paymentToIncomePercentage > 0)
      
    zeros_pti <- sum(data$paymentToIncomePercentage == 0, na.rm = TRUE)
    if (zeros_pti > 0) {
      cat(sprintf("  Excluded %d loans with 0 PTI.\n", zeros_pti))
    }
      
    if (nrow(pti_plot_data) > 0) {
      curr_pti <- mean(pti_plot_data$paymentToIncomePercentage, na.rm = TRUE)
      curr_high_pti_10 <- sum(pti_plot_data$paymentToIncomePercentage > 0.10) / nrow(pti_plot_data) * 100
      curr_high_pti_15 <- sum(pti_plot_data$paymentToIncomePercentage > 0.15) / nrow(pti_plot_data) * 100
      curr_high_pti_20 <- sum(pti_plot_data$paymentToIncomePercentage > 0.20) / nrow(pti_plot_data) * 100
      p_pti <- ggplot(pti_plot_data, aes(x = paymentToIncomePercentage)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
        labs(
          title = paste("PTI Distribution:", trust),
          x = "Payment to Income Ratio",
          y = "Frequency"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12)
        )
        
      filename <- file.path(pti_dir, paste0(safe_name, "_pti.png"))
      ggsave(filename, p_pti, width = 10, height = 6, bg = "white")
      cat("  Saved PTI plot:", filename, "\n")
    }
  }
  
  # --- Interest Rate Analysis ---
  if ("reportingPeriodInterestRatePercentage" %in% names(data)) {
    rate_plot_data <- data %>%
      filter(!is.na(reportingPeriodInterestRatePercentage) & reportingPeriodInterestRatePercentage > 0)
      
    if (nrow(rate_plot_data) > 0) {
      curr_rate <- mean(rate_plot_data$reportingPeriodInterestRatePercentage, na.rm = TRUE)
      
      p_rate <- ggplot(rate_plot_data, aes(x = reportingPeriodInterestRatePercentage)) +
        geom_histogram(bins = 50, fill = "darkgreen", color = "black", alpha = 0.7) +
        labs(
          title = paste("Interest Rate Distribution:", trust),
          x = "Interest Rate (%)",
          y = "Frequency"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12)
        )
        
      filename <- file.path(rate_dir, paste0(safe_name, "_rate.png"))
      ggsave(filename, p_rate, width = 10, height = 6, bg = "white")
      cat("  Saved Rate plot:", filename, "\n")
    }
  }
  
  # --- Regression & Danger Zone Analysis ---
  if ("obligorCreditScore" %in% names(data)) {
    
    # 1. Credit vs PTI
    if ("paymentToIncomePercentage" %in% names(data)) {
      tmp <- data %>% filter(!is.na(obligorCreditScore) & !is.na(paymentToIncomePercentage))
      if (nrow(tmp) > 10) {
        m <- lm(paymentToIncomePercentage ~ obligorCreditScore, data = tmp)
        r_pti_slope <- coef(m)[2]
        r_pti_int <- coef(m)[1]
        r_pti_r2 <- summary(m)$r.squared
        r_pti_corr <- cor(tmp$paymentToIncomePercentage, tmp$obligorCreditScore, method = "pearson")
        
        # Danger Zone: FICO > 740 & PTI > 12% (0.12)
        curr_prime_high_pti <- (sum(data$obligorCreditScore > 740 & data$paymentToIncomePercentage > 0.12, na.rm=TRUE) / nrow(data)) * 100
      }
    }
    
    # 2. Credit vs LTV
    if ("ltvRatio" %in% names(data)) {
      tmp <- data %>% filter(!is.na(obligorCreditScore) & !is.na(ltvRatio) & ltvRatio < 250)
      if (nrow(tmp) > 10) {
        m <- lm(ltvRatio ~ obligorCreditScore, data = tmp)
        r_ltv_slope <- coef(m)[2]
        r_ltv_int <- coef(m)[1]
        r_ltv_r2 <- summary(m)$r.squared
        r_ltv_corr <- cor(tmp$ltvRatio, tmp$obligorCreditScore, method = "pearson")
      }
    }
    
    # 3. Credit vs Rate
    if ("reportingPeriodInterestRatePercentage" %in% names(data)) {
      tmp <- data %>% filter(!is.na(obligorCreditScore) & !is.na(reportingPeriodInterestRatePercentage))
      if (nrow(tmp) > 10) {
        m <- lm(reportingPeriodInterestRatePercentage ~ obligorCreditScore, data = tmp)
        r_rate_slope <- coef(m)[2]
        r_rate_int <- coef(m)[1]
        r_rate_r2 <- summary(m)$r.squared
        r_rate_corr <- cor(tmp$reportingPeriodInterestRatePercentage, tmp$obligorCreditScore, method = "pearson")
        
        # Danger Zone: FICO > 740 & Rate > 12%
        # Check scale: if mean > 1, assume percentage (12), else decimal (0.12)
        rate_threshold <- 0.12
        if (mean(tmp$reportingPeriodInterestRatePercentage, na.rm=TRUE) > 1) {
          rate_threshold <- 12
        }
        curr_prime_high_rate <- (sum(data$obligorCreditScore > 740 & data$reportingPeriodInterestRatePercentage > rate_threshold, na.rm=TRUE) / nrow(data)) * 100
      }
    }
  }
  
  # Add to summary stats
  trust_stats <- rbind(trust_stats, data.frame(
    trust_name = trust,
    avg_ltv = curr_ltv,
    pct_underwater = curr_underwater_pct,
    avg_pti = curr_pti,
    pct_high_pti_10 = curr_high_pti_10,
    pct_high_pti_15 = curr_high_pti_15,
    pct_high_pti_20 = curr_high_pti_20,
    avg_rate = curr_rate,
    reg_pti_slope = r_pti_slope, reg_pti_intercept = r_pti_int, reg_pti_r2 = r_pti_r2, reg_pti_corr = r_pti_corr,
    reg_ltv_slope = r_ltv_slope, reg_ltv_intercept = r_ltv_int, reg_ltv_r2 = r_ltv_r2, reg_ltv_corr = r_ltv_corr,
    reg_rate_slope = r_rate_slope, reg_rate_intercept = r_rate_int, reg_rate_r2 = r_rate_r2, reg_rate_corr = r_rate_corr,
    pct_prime_high_pti = curr_prime_high_pti,
    pct_prime_high_rate = curr_prime_high_rate
  ))
}

# --- Trend Analysis ---
cat("\nGenerating trend plots...\n")

# Extract Year and Series for sorting
# Assuming format "Carvana Auto Receivables Trust YYYY-PX" or similar
trust_stats <- trust_stats %>%
  mutate(
    year = as.numeric(str_extract(trust_name, "20\\d{2}")),
    series_str = str_extract(trust_name, "(P|N)\\d+"),
    series_num = as.numeric(str_extract(series_str, "\\d+"))
  ) %>%
  arrange(year, series_str, series_num) %>%
  mutate(trust_label = paste(year, series_str, sep="-")) %>%
  filter(!is.na(year)) # Filter out any that didn't match pattern

# Ensure factor order for plotting
trust_stats$trust_label <- factor(trust_stats$trust_label, levels = trust_stats$trust_label)

# Save Summary Table (CSV)
write.csv(trust_stats, file.path(trend_dir, "trust_analysis_summary.csv"), row.names = FALSE)
cat(sprintf("Saved summary table to %s\n", file.path(trend_dir, "trust_analysis_summary.csv")))

# --- Generate LaTeX Tables ---
cat("\nGenerating LaTeX tables...\n")
latex_file <- file.path(trend_dir, "regression_tables.tex")
sink(latex_file)

# Table 1: Credit vs PTI
cat("% Table 1: Credit Score vs PTI\n")
cat("\\begin{table}[ht]\n\\centering\n")
cat("\\caption{Regression Analysis: Credit Score vs. Payment-to-Income (PTI)}\n")
cat("\\begin{tabular}{lcccc}\n\\hline\n")
cat("Trust & Slope & Intercept & $R^2$ & Correlation \\\\\n\\hline\n")
for(i in 1:nrow(trust_stats)) {
  cat(sprintf("%s & %.6f & %.4f & %.4f & %.4f \\\\\n", 
              trust_stats$trust_label[i], 
              trust_stats$reg_pti_slope[i], 
              trust_stats$reg_pti_intercept[i], 
              trust_stats$reg_pti_r2[i], 
              trust_stats$reg_pti_corr[i]))
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

# Table 2: Credit vs LTV
cat("% Table 2: Credit Score vs LTV\n")
cat("\\begin{table}[ht]\n\\centering\n")
cat("\\caption{Regression Analysis: Credit Score vs. Loan-to-Value (LTV)}\n")
cat("\\begin{tabular}{lcccc}\n\\hline\n")
cat("Trust & Slope & Intercept & $R^2$ & Correlation \\\\\n\\hline\n")
for(i in 1:nrow(trust_stats)) {
  cat(sprintf("%s & %.6f & %.4f & %.4f & %.4f \\\\\n", 
              trust_stats$trust_label[i], 
              trust_stats$reg_ltv_slope[i], 
              trust_stats$reg_ltv_intercept[i], 
              trust_stats$reg_ltv_r2[i], 
              trust_stats$reg_ltv_corr[i]))
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

# Table 3: Credit vs Rate
cat("% Table 3: Credit Score vs Interest Rate\n")
cat("\\begin{table}[ht]\n\\centering\n")
cat("\\caption{Regression Analysis: Credit Score vs. Interest Rate}\n")
cat("\\begin{tabular}{lcccc}\n\\hline\n")
cat("Trust & Slope & Intercept & $R^2$ & Correlation \\\\\n\\hline\n")
for(i in 1:nrow(trust_stats)) {
  cat(sprintf("%s & %.6f & %.4f & %.4f & %.4f \\\\\n", 
              trust_stats$trust_label[i], 
              trust_stats$reg_rate_slope[i], 
              trust_stats$reg_rate_intercept[i], 
              trust_stats$reg_rate_r2[i], 
              trust_stats$reg_rate_corr[i]))
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

sink()
cat(sprintf("  Saved LaTeX tables to %s\n", latex_file))

if (nrow(trust_stats) > 0) {
  # 1. LTV Trend
  p_trend_ltv <- ggplot(trust_stats, aes(x = trust_label, y = avg_ltv, group = 1)) +
    geom_line(color = "purple", size = 1) +
    geom_point(size = 3, color = "purple") +
    geom_label(aes(label = paste0(round(avg_ltv, 1), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Average LTV Trend by Trust Vintage", x = "Trust", y = "Average LTV (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_ltv.png"), p_trend_ltv, width = 12, height = 6, bg = "white")
  cat("  Saved LTV trend plot.\n")
  
  # 1b. Underwater LTV Trend (Combined)
  p_trend_underwater <- ggplot(trust_stats, aes(x = trust_label, group = 1)) +
    geom_line(aes(y = pct_underwater), color = "red", size = 1) +
    geom_point(aes(y = pct_underwater), color = "red", size = 3) +
    geom_label(aes(y = pct_underwater, label = paste0(round(pct_underwater, 1), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Trend: Percentage of Underwater Loans (LTV > 100%)", x = "Trust", y = "Percentage of Loans (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_ltv_underwater.png"), p_trend_underwater, width = 12, height = 6, bg = "white")
  cat("  Saved Underwater LTV trend plot.\n")

  # 2. PTI Trend
  p_trend_pti <- ggplot(trust_stats, aes(x = trust_label, group = 1)) +
    geom_line(aes(y = pct_high_pti_10, color = "> 10%"), size = 1) +
    geom_point(aes(y = pct_high_pti_10, color = "> 10%"), size = 3) +
    geom_label(aes(y = pct_high_pti_10, label = paste0(round(pct_high_pti_10, 1), "%"), color = "> 10%"), fill = "white", label.size = NA, vjust = -0.5, size = 3, show.legend = FALSE) +
    geom_line(aes(y = pct_high_pti_15, color = "> 15%"), size = 1) +
    geom_point(aes(y = pct_high_pti_15, color = "> 15%"), size = 3) +
    geom_label(aes(y = pct_high_pti_15, label = paste0(round(pct_high_pti_15, 1), "%"), color = "> 15%"), fill = "white", label.size = NA, vjust = -0.5, size = 3, show.legend = FALSE) +
    geom_line(aes(y = pct_high_pti_20, color = "> 20%"), size = 1) +
    geom_point(aes(y = pct_high_pti_20, color = "> 20%"), size = 3) +
    geom_label(aes(y = pct_high_pti_20, label = paste0(round(pct_high_pti_20, 1), "%"), color = "> 20%"), fill = "white", label.size = NA, vjust = -0.5, size = 3, show.legend = FALSE) +
    scale_color_manual(name = "PTI Threshold", values = c("> 10%" = "forestgreen", "> 15%" = "orange", "> 20%" = "firebrick")) +
    labs(title = "Trend: Percentage of Pool with High PTI", x = "Trust", y = "Percentage of Loans (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  ggsave(file.path(trend_dir, "trend_pti_high.png"), p_trend_pti, width = 12, height = 6, bg = "white")
  cat("  Saved High PTI trend plot.\n")

  # 2b. Mean PTI Trend
  p_trend_avg_pti <- ggplot(trust_stats, aes(x = trust_label, y = avg_pti * 100, group = 1)) +
    geom_line(color = "blue", size = 1) +
    geom_point(size = 3, color = "blue") +
    geom_label(aes(label = paste0(round(avg_pti * 100, 1), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Trend: Average Payment-to-Income (PTI) Ratio", x = "Trust", y = "Average PTI (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_pti_avg.png"), p_trend_avg_pti, width = 12, height = 6, bg = "white")
  cat("  Saved Average PTI trend plot.\n")

  # 3. Rate Trend
  p_trend_rate <- ggplot(trust_stats, aes(x = trust_label, y = avg_rate, group = 1)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_point(size = 3, color = "darkgreen") +
    geom_label(aes(label = paste0(round(avg_rate, 2), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Average Interest Rate Trend by Trust Vintage", x = "Trust", y = "Average Interest Rate (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_rate.png"), p_trend_rate, width = 12, height = 6, bg = "white")
  cat("  Saved Rate trend plot.\n")
  
  # 4. Danger Zone: Prime & High PTI
  p_trend_prime_pti <- ggplot(trust_stats, aes(x = trust_label, y = pct_prime_high_pti, group = 1)) +
    geom_line(color = "orange", size = 1) +
    geom_point(size = 3, color = "orange") +
    geom_label(aes(label = paste0(round(pct_prime_high_pti, 2), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Trend: Prime Borrowers (FICO > 740) with High PTI (> 12%)", x = "Trust", y = "Percentage of Pool (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_danger_prime_pti.png"), p_trend_prime_pti, width = 12, height = 6, bg = "white")
  cat("  Saved Prime High PTI trend plot.\n")

  # 5. Danger Zone: Prime & High Rate
  p_trend_prime_rate <- ggplot(trust_stats, aes(x = trust_label, y = pct_prime_high_rate, group = 1)) +
    geom_line(color = "firebrick", size = 1) +
    geom_point(size = 3, color = "firebrick") +
    geom_label(aes(label = paste0(round(pct_prime_high_rate, 2), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Trend: Prime Borrowers (FICO > 740) with High Rate (> 12%)", x = "Trust", y = "Percentage of Pool (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_danger_prime_rate.png"), p_trend_prime_rate, width = 12, height = 6, bg = "white")
  cat("  Saved Prime High Rate trend plot.\n")
}

dbDisconnect(conn)
cat("\nAnalysis complete.\n")