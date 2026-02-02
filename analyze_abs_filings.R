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
term_dir <- "plots/term_distros"
trend_dir <- "plots/trends"
spread_dir <- "plots/spread_distros"

if (!dir.exists(ltv_dir)) dir.create(ltv_dir, recursive = TRUE)
if (!dir.exists(pti_dir)) dir.create(pti_dir, recursive = TRUE)
if (!dir.exists(rate_dir)) dir.create(rate_dir, recursive = TRUE)
if (!dir.exists(term_dir)) dir.create(term_dir, recursive = TRUE)
if (!dir.exists(trend_dir)) dir.create(trend_dir, recursive = TRUE)
if (!dir.exists(spread_dir)) dir.create(spread_dir, recursive = TRUE)

# Load federal funds rate data
ffr_file <- "fed_funds_rate.csv"
if (!file.exists(ffr_file)) {
  stop(paste("Federal funds rate file not found:", ffr_file,
             "\nRun fetch_fed_funds_rate.py first to generate this file."))
}
ffr_data <- read.csv(ffr_file)
ffr_data$date <- as.Date(ffr_data$date)
ffr_data$year_month <- format(ffr_data$date, "%Y-%m")
cat(sprintf("Loaded federal funds rate data: %d observations\n", nrow(ffr_data)))

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
  avg_spread = numeric(),
  avg_term = numeric(),
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

# Initialize storage for verification distributions
income_dist_all <- data.frame()
employ_dist_all <- data.frame()

# Initialize storage for spread data (for combined plot)
spread_all <- data.frame()

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
  curr_spread <- NA
  curr_term <- NA
  
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

  # --- Spread Analysis ---
  if ("reportingPeriodInterestRatePercentage" %in% names(data) &&
      "reportingPeriodBeginningDate" %in% names(data)) {

    # Create year-month key for joining with FFR data
    spread_data <- data %>%
      filter(!is.na(reportingPeriodInterestRatePercentage) &
             reportingPeriodInterestRatePercentage > 0 &
             !is.na(reportingPeriodBeginningDate)) %>%
      mutate(year_month = format(reportingPeriodBeginningDate, "%Y-%m"))

    # Join with federal funds rate
    spread_data <- spread_data %>%
      left_join(ffr_data %>% select(year_month, fed_funds_rate), by = "year_month")

    # Calculate spread (APR - Federal Funds Rate)
    # Note: Loan rates are stored as decimals (0.08 = 8%), FFR is in percentage (4.33 = 4.33%)
    # Convert loan rate to percentage for consistent comparison
    spread_data <- spread_data %>%
      filter(!is.na(fed_funds_rate)) %>%
      mutate(
        apr_pct = reportingPeriodInterestRatePercentage * 100,
        spread = apr_pct - fed_funds_rate
      )

    if (nrow(spread_data) > 0) {
      curr_spread <- mean(spread_data$spread, na.rm = TRUE)

      # Create scatter plot of spread vs FICO score
      if ("obligorCreditScore" %in% names(spread_data)) {
        spread_fico_data <- spread_data %>%
          filter(!is.na(obligorCreditScore) & obligorCreditScore > 300)

        if (nrow(spread_fico_data) > 0) {
          # Calculate correlation
          fico_spread_corr <- cor(spread_fico_data$obligorCreditScore,
                                   spread_fico_data$spread,
                                   use = "complete.obs")

          p_spread_fico <- ggplot(spread_fico_data, aes(x = obligorCreditScore, y = spread)) +
            geom_point(alpha = 0.15, color = "steelblue", size = 1) +
            geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1) +
            labs(
              title = paste("Spread vs FICO Score:", trust),
              subtitle = paste0("Correlation: ", round(fico_spread_corr, 3),
                               " | Mean spread: ", round(curr_spread, 2), "%",
                               " | n=", nrow(spread_fico_data)),
              x = "FICO Score",
              y = "Spread (APR - Fed Funds Rate, %)"
            ) +
            theme(
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 12)
            )

          filename <- file.path(spread_dir, paste0(safe_name, "_spread_fico.png"))
          ggsave(filename, p_spread_fico, width = 10, height = 6, bg = "white")
          cat("  Saved Spread vs FICO plot:", filename, "\n")
        }
      }

      # Also create a histogram of spreads
      p_spread_hist <- ggplot(spread_data, aes(x = spread)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
        geom_vline(xintercept = mean(spread_data$spread, na.rm = TRUE),
                   color = "red", linetype = "dashed", linewidth = 1) +
        labs(
          title = paste("Spread Distribution:", trust),
          subtitle = paste0("Mean spread: ", round(curr_spread, 2),
                           "% over Fed Funds Rate"),
          x = "Spread (APR - Fed Funds Rate, %)",
          y = "Frequency"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12)
        )

      filename <- file.path(spread_dir, paste0(safe_name, "_spread_hist.png"))
      ggsave(filename, p_spread_hist, width = 10, height = 6, bg = "white")
      cat("  Saved Spread histogram:", filename, "\n")

      # Accumulate spread data for combined plot (include FICO if available)
      if ("obligorCreditScore" %in% names(spread_data)) {
        spread_all <- rbind(spread_all, spread_data %>%
          select(trust_name, spread, apr_pct, fed_funds_rate, obligorCreditScore))
      } else {
        spread_all <- rbind(spread_all, spread_data %>%
          mutate(obligorCreditScore = NA) %>%
          select(trust_name, spread, apr_pct, fed_funds_rate, obligorCreditScore))
      }
    }
  }

  # --- Term Analysis ---
  if ("originalLoanTerm" %in% names(data)) {
    term_plot_data <- data %>%
      filter(!is.na(originalLoanTerm) & originalLoanTerm > 0)
      
    if (nrow(term_plot_data) > 0) {
      curr_term <- mean(term_plot_data$originalLoanTerm, na.rm = TRUE)
      
      p_term <- ggplot(term_plot_data, aes(x = originalLoanTerm)) +
        geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
        labs(
          title = paste("Loan Term Distribution:", trust),
          x = "Original Loan Term (Months)",
          y = "Frequency"
        ) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        
      filename <- file.path(term_dir, paste0(safe_name, "_term.png"))
      ggsave(filename, p_term, width = 10, height = 6, bg = "white")
      cat("  Saved Term plot:", filename, "\n")
    }
  }
  
  # --- Verification Analysis ---
  if ("obligorIncomeVerificationLevelCode" %in% names(data)) {
    inc_counts <- data %>%
      filter(!is.na(obligorIncomeVerificationLevelCode)) %>%
      group_by(obligorIncomeVerificationLevelCode) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(pct = count / sum(count) * 100, trust_name = trust)
    
    income_dist_all <- rbind(income_dist_all, inc_counts)
  }

  if ("obligorEmploymentVerificationCode" %in% names(data)) {
    emp_counts <- data %>%
      filter(!is.na(obligorEmploymentVerificationCode)) %>%
      group_by(obligorEmploymentVerificationCode) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(pct = count / sum(count) * 100, trust_name = trust)
    
    employ_dist_all <- rbind(employ_dist_all, emp_counts)
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
    avg_spread = curr_spread,
    avg_term = curr_term,
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

  # 3b. Spread Trend
  p_trend_spread <- ggplot(trust_stats, aes(x = trust_label, y = avg_spread, group = 1)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(size = 3, color = "steelblue") +
    geom_label(aes(label = paste0(round(avg_spread, 2), "%")), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Average Spread (APR - Fed Funds Rate) by Trust Vintage", x = "Trust", y = "Average Spread (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_spread.png"), p_trend_spread, width = 12, height = 6, bg = "white")
  cat("  Saved Spread trend plot.\n")

  # 3c. Term Trend
  p_trend_term <- ggplot(trust_stats, aes(x = trust_label, y = avg_term, group = 1)) +
    geom_line(color = "orange", size = 1) +
    geom_point(size = 3, color = "orange") +
    geom_label(aes(label = round(avg_term, 1)), fill = "white", label.size = NA, vjust = -0.5, size = 3) +
    labs(title = "Average Original Loan Term Trend by Trust Vintage", x = "Trust", y = "Average Loan Term (Months)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(trend_dir, "trend_term.png"), p_trend_term, width = 12, height = 6, bg = "white")
  cat("  Saved Term trend plot.\n")
  
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
  
  # 6. Verification Trends
  # Create mapping for trust labels
  trust_label_map <- trust_stats %>% select(trust_name, trust_label)
  
  if (nrow(income_dist_all) > 0) {
    income_plot_data <- income_dist_all %>%
      inner_join(trust_label_map, by = "trust_name")
    
    # Ensure factor order matches trust_stats
    income_plot_data$trust_label <- factor(income_plot_data$trust_label, levels = levels(trust_stats$trust_label))
    
    p_inc <- ggplot(income_plot_data, aes(x = trust_label, y = pct, fill = as.factor(obligorIncomeVerificationLevelCode))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(pct > 5, paste0(round(pct, 0), "%"), "")), position = position_stack(vjust = 0.5), size = 3, color = "white") +
      labs(title = "Trend: Income Verification Level Distribution", x = "Trust", y = "Percentage of Pool (%)", fill = "Code") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(trend_dir, "trend_income_verification.png"), p_inc, width = 12, height = 6, bg = "white")
    cat("  Saved Income Verification trend plot.\n")
  }

  if (nrow(employ_dist_all) > 0) {
    employ_plot_data <- employ_dist_all %>%
      inner_join(trust_label_map, by = "trust_name")
    
    # Ensure factor order matches trust_stats
    employ_plot_data$trust_label <- factor(employ_plot_data$trust_label, levels = levels(trust_stats$trust_label))
    
    p_emp <- ggplot(employ_plot_data, aes(x = trust_label, y = pct, fill = as.factor(obligorEmploymentVerificationCode))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(pct > 5, paste0(round(pct, 0), "%"), "")), position = position_stack(vjust = 0.5), size = 3, color = "white") +
      labs(title = "Trend: Employment Verification Level Distribution", x = "Trust", y = "Percentage of Pool (%)", fill = "Code") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(trend_dir, "trend_employment_verification.png"), p_emp, width = 12, height = 6, bg = "white")
    cat("  Saved Employment Verification trend plot.\n")
  }

  # 7. Dual Axis: R^2 (FICO vs Rate) vs % Underwater
  # Scale factor for secondary axis: R2 is 0-1, Underwater is 0-100.
  scale_factor <- 100
  p_dual <- ggplot(trust_stats, aes(x = trust_label, group = 1)) +
    # Primary Axis: R^2 (Blue)
    geom_line(aes(y = reg_rate_r2, color = "R^2 (FICO vs Rate)"), size = 1) +
    geom_point(aes(y = reg_rate_r2, color = "R^2 (FICO vs Rate)"), size = 3) +
    
    # Secondary Axis: % Underwater (Red) - Scaled down
    geom_line(aes(y = pct_underwater / scale_factor, color = "% Underwater"), size = 1) +
    geom_point(aes(y = pct_underwater / scale_factor, color = "% Underwater"), size = 3) +
    
    scale_y_continuous(
      name = "Correlation (R^2)",
      limits = c(0, 1),
      sec.axis = sec_axis(~ . * scale_factor, name = "% Underwater (LTV > 100)")
    ) +
    scale_color_manual(name = "Metric", values = c("R^2 (FICO vs Rate)" = "blue", "% Underwater" = "red")) +
    labs(
      title = "Trend: Pricing Correlation vs. Negative Equity",
      x = "Trust"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  ggsave(file.path(trend_dir, "trend_r2_vs_underwater.png"), p_dual, width = 12, height = 6, bg = "white")
  cat("  Saved R^2 vs Underwater dual-axis trend plot.\n")

  # 8. Combined Spread vs FICO Scatter Plot (all vintages)
  if (nrow(spread_all) > 0) {
    # Add vintage labels to spread data
    spread_all <- spread_all %>%
      inner_join(trust_label_map, by = "trust_name")

    # Ensure factor order matches trust_stats
    spread_all$trust_label <- factor(spread_all$trust_label, levels = levels(trust_stats$trust_label))

    # Filter to valid FICO scores
    spread_fico_all <- spread_all %>%
      filter(!is.na(obligorCreditScore) & obligorCreditScore > 300)

    if (nrow(spread_fico_all) > 0) {
      # Combined scatter plot: Spread vs FICO, colored by vintage
      p_spread_fico_combined <- ggplot(spread_fico_all, aes(x = obligorCreditScore, y = spread, color = trust_label)) +
        geom_point(alpha = 0.1, size = 0.5) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        labs(
          title = "Spread vs FICO Score by Trust Vintage",
          subtitle = "Spread = APR - Federal Funds Rate | Lines show linear trend per vintage",
          x = "FICO Score",
          y = "Spread (%)",
          color = "Vintage"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "right"
        ) +
        guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
      ggsave(file.path(trend_dir, "spread_vs_fico_by_vintage.png"), p_spread_fico_combined, width = 14, height = 8, bg = "white")
      cat("  Saved Combined Spread vs FICO scatter plot.\n")

      # Faceted version for clearer view of each vintage
      p_spread_fico_facet <- ggplot(spread_fico_all, aes(x = obligorCreditScore, y = spread)) +
        geom_point(alpha = 0.1, color = "steelblue", size = 0.5) +
        geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1) +
        facet_wrap(~ trust_label, scales = "free_y", ncol = 4) +
        labs(
          title = "Spread vs FICO Score by Trust Vintage",
          subtitle = "Spread = APR - Federal Funds Rate",
          x = "FICO Score",
          y = "Spread (%)"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 10, face = "bold")
        )
      ggsave(file.path(trend_dir, "spread_vs_fico_faceted.png"), p_spread_fico_facet, width = 16, height = 12, bg = "white")
      cat("  Saved Faceted Spread vs FICO plot.\n")
    }

    # Also keep the spread distribution by vintage plots
    p_spread_combined <- ggplot(spread_all, aes(x = trust_label, y = spread)) +
      geom_jitter(width = 0.2, alpha = 0.15, color = "steelblue", size = 0.5) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5, fill = "lightblue") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      labs(
        title = "Spread Distribution by Trust Vintage",
        subtitle = "Spread = APR - Federal Funds Rate | Red diamond = mean",
        x = "Trust Vintage",
        y = "Spread (%)"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    ggsave(file.path(trend_dir, "spread_by_vintage.png"), p_spread_combined, width = 14, height = 8, bg = "white")
    cat("  Saved Combined Spread distribution plot.\n")
  }
}

dbDisconnect(conn)
cat("\nAnalysis complete.\n")