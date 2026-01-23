# Load required libraries
library(RSQLite)
library(ggplot2)
library(dplyr)
library(forcats)

# Define a dark theme
dark_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    text = element_text(color = "#e0e0e0"),
    axis.text = element_text(color = "#e0e0e0"),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#252525"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# Apply the dark theme globally
theme_set(dark_theme)

# Create output directory for plots
output_dir <- "plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Connect to the database
db_path <- "cvna-2024.db"
conn <- dbConnect(RSQLite::SQLite(), db_path)

# List available tables
tables <- dbListTables(conn)
cat("Available tables:", paste(tables, collapse=", "), "\n")

# Read the data - adjust table name as needed
# Common table names might be: data, sec_data, records, etc.
# Try to find the correct table
if ("data" %in% tables) {
  data <- dbReadTable(conn, "data")
} else if ("sec_data" %in% tables) {
  data <- dbReadTable(conn, "sec_data")
} else {
  # Use the first table if neither common name exists
  data <- dbReadTable(conn, tables[1])
  cat("Using table:", tables[1], "\n")
}

# Close the database connection
dbDisconnect(conn)

# Display basic information about the data
cat("Data dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Columns:", paste(names(data), collapse=", "), "\n")

# Calculate LTV Ratio globally if columns exist
# This is a key metric for ABS analysis (Loan Balance / Collateral Value)

# Assume 15% annual depreciation (approx 1.25% per month)
# You can calculate 'months_since_origination' using the loanOriginationDate column
annual_depreciation <- 0.15
if ("vehicleValueAmount" %in% names(data) && "reportingPeriodBeginningLoanBalanceAmount" %in% names(data)) {
  
  # Calculate current estimated value based on age if dates available
  if ("originationDate" %in% names(data) && "reportingPeriodBeginningDate" %in% names(data)) {
    # Try to calculate age in years
    data$loan_age_years <- as.numeric(difftime(as.Date(data$reportingPeriodBeginningDate), as.Date(data$originationDate), units = "days")) / 365.25
    # Handle NAs or invalid dates by defaulting to 1 year
    data$loan_age_years[is.na(data$loan_age_years) | data$loan_age_years < 0] <- 1
    # Depreciate
    data$currentEstimatedVehicleValue <- data$vehicleValueAmount * ((1 - annual_depreciation) ^ data$loan_age_years)
  } else {
    # Fallback: assuming 12 months of seasoning
    data$currentEstimatedVehicleValue <- data$vehicleValueAmount * (1 - annual_depreciation)
  }
  
  # Calculate Mark-to-Market LTV
  data$ltvRatio <- (data$reportingPeriodBeginningLoanBalanceAmount / data$currentEstimatedVehicleValue) * 100
}

# Check for paymentToIncomePercentage column
if ("paymentToIncomePercentage" %in% names(data)) {
  # Display summary statistics
  cat("\nSummary statistics for paymentToIncomePercentage:\n")
  print(summary(data$paymentToIncomePercentage))
  
  # Remove NA values
  payment_data <- data$paymentToIncomePercentage %>%
    na.omit()
  
  cat("Non-NA observations:", length(payment_data), "\n")
  
  # Create distribution plot
  p <- ggplot(data, aes(x = paymentToIncomePercentage)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(
      title = "Distribution of Payment to Income Percentage",
      x = "Payment to Income Percentage (%)",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12)
    )
  
  # Display the plot
  print(p)
  
  # Save the plot
  ggsave(file.path(output_dir, "paymentToIncomePercentage_distribution.png"), p, width = 10, height = 6, dpi = 300)
  cat("\nPlot saved as:", file.path(output_dir, "paymentToIncomePercentage_distribution.png"), "\n")
  
  # Additional analysis: Create a density plot
  p_density <- ggplot(data, aes(x = paymentToIncomePercentage)) +
    geom_density(fill = "steelblue", alpha = 0.6, color = "cyan") +
    labs(
      title = "Density Plot: Payment to Income Percentage",
      x = "Payment to Income Percentage (%)",
      y = "Density"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12)
    )
  
  print(p_density)
  ggsave(file.path(output_dir, "paymentToIncomePercentage_density.png"), p_density, width = 10, height = 6, dpi = 300)
  cat("Density plot saved as:", file.path(output_dir, "paymentToIncomePercentage_density.png"), "\n")
  
} else {
  cat("ERROR: 'paymentToIncomePercentage' column not found in the data.\n")
  cat("Available columns:", paste(names(data), collapse=", "), "\n")
}

# ============================================================================
# LOAN-TO-VALUE (LTV) ANALYSIS
# ============================================================================

cat("\n\n=== LOAN-TO-VALUE (LTV) ANALYSIS ===\n")

if ("vehicleValueAmount" %in% names(data) && "reportingPeriodBeginningLoanBalanceAmount" %in% names(data)) {
  
  # Filter out extreme outliers for visualization (e.g., > 200%)
  ltv_plot_data <- data %>%
    filter(!is.na(ltvRatio) & ltvRatio <= 200)
    
  cat("Mean LTV:", mean(data$ltvRatio, na.rm = TRUE), "%\n")
  cat("Loans Underwater (>100% LTV):", sum(data$ltvRatio > 100, na.rm = TRUE), "\n")

  # LTV Histogram
  p_ltv <- ggplot(ltv_plot_data, aes(x = ltvRatio)) +
    geom_histogram(bins = 50, fill = "purple", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 100, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Distribution of Loan-to-Value (LTV) Ratios",
      subtitle = "Red line indicates 100% LTV (Underwater)",
      x = "LTV Ratio (%)",
      y = "Frequency"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  print(p_ltv)
  ggsave(file.path(output_dir, "ltv_distribution.png"), p_ltv, width = 10, height = 6, dpi = 300)
  cat("Plot saved as:", file.path(output_dir, "ltv_distribution.png"), "\n")
  
  # Scatter Plot: LTV vs Payment-to-Income
  if ("paymentToIncomePercentage" %in% names(data)) {
    # Filter for reasonable ranges to avoid squashing the plot
    scatter_data <- ltv_plot_data %>% 
      filter(paymentToIncomePercentage <= 0.5)
      
    p_scatter <- ggplot(scatter_data, aes(x = paymentToIncomePercentage, y = ltvRatio)) +
      geom_point(alpha = 0.2, color = "cyan") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(
        title = "Correlation: LTV vs Payment-to-Income",
        x = "Payment to Income Ratio",
        y = "LTV Ratio (%)"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    print(p_scatter)
    ggsave(file.path(output_dir, "ltv_vs_pti_scatter.png"), p_scatter, width = 10, height = 6, dpi = 300)
    cat("Plot saved as:", file.path(output_dir, "ltv_vs_pti_scatter.png"), "\n")
  }
}

# ============================================================================
# GEOGRAPHIC CONCENTRATION
# ============================================================================

if ("obligorGeographicLocation" %in% names(data)) {
  cat("\n\n=== GEOGRAPHIC CONCENTRATION ===\n")
  
  state_counts <- data %>%
    filter(!is.na(obligorGeographicLocation)) %>%
    count(obligorGeographicLocation) %>%
    arrange(desc(n)) %>%
    head(10)
    
  print(state_counts)
  
  p_geo <- ggplot(state_counts, aes(x = reorder(obligorGeographicLocation, n), y = n)) +
    geom_col(fill = "orange", color = "black", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Top 10 States by Loan Count",
      x = "State",
      y = "Number of Loans"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  print(p_geo)
  ggsave(file.path(output_dir, "geographic_concentration.png"), p_geo, width = 10, height = 6, dpi = 300)
  cat("Plot saved as:", file.path(output_dir, "geographic_concentration.png"), "\n")
}

# ============================================================================
# RISKY LOANS ANALYSIS
# ============================================================================

cat("\n\n=== RISKY LOANS ANALYSIS ===\n")

# Check for required columns
required_cols <- c("paymentToIncomePercentage", "vehicleValueAmount", "reportingPeriodBeginningLoanBalanceAmount", "vehicleManufacturerName")
if (all(required_cols %in% names(data))) {
  
  # Define risky loans: paymentToIncomePercentage >= 10% AND LTV > 100% (Underwater)
  data$is_risky <- (data$paymentToIncomePercentage >= .1 & data$ltvRatio > 100 & 
                    !is.na(data$paymentToIncomePercentage) & !is.na(data$ltvRatio))
  
  # Count risky loans
  total_risky <- sum(data$is_risky, na.rm = TRUE)
  cat("Total risky loans (Payment/Income >= 10% AND LTV > 100%):", total_risky, "\n")
  
  # Filter for risky loans and count by manufacturer
  risky_by_manufacturer <- data %>%
    filter(is_risky == TRUE) %>%
    group_by(vehicleManufacturerName) %>%
    summarise(risky_count = n(), .groups = 'drop') %>%
    arrange(desc(risky_count))
  
  cat("\nRisky loans by manufacturer (top 20):\n")
  print(head(risky_by_manufacturer, 20))
  
  # Create bar chart of risky loans by manufacturer (top 20)
  risky_top_20 <- head(risky_by_manufacturer, 20) %>%
    mutate(vehicleManufacturerName = fct_reorder(vehicleManufacturerName, risky_count))
  
  p_risky_loans <- ggplot(risky_top_20, aes(x = vehicleManufacturerName, y = risky_count, fill = risky_count)) +
    geom_col(color = "black", alpha = 0.8) +
    scale_fill_gradient(low = "lightyellow", high = "darkred") +
    labs(
      title = "Top 20 Vehicle Manufacturers by Number of Risky Loans\n(Payment/Income >= 10% AND LTV > 100%)",
      x = "Manufacturer",
      y = "Number of Risky Loans",
      fill = "Count"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "right"
    )
  
  # Display the plot
  print(p_risky_loans)
  
  # Save the plot
  ggsave(file.path(output_dir, "risky_loans_by_manufacturer.png"), p_risky_loans, width = 12, height = 7, dpi = 300)
  cat("\nPlot saved as:", file.path(output_dir, "risky_loans_by_manufacturer.png"), "\n")
  
  # Optional: Also create a version with all manufacturers
  risky_all <- risky_by_manufacturer %>%
    arrange(desc(risky_count)) %>%
    mutate(vehicleManufacturerName = fct_reorder(vehicleManufacturerName, risky_count))
  
  cat("\nTotal manufacturers with risky loans:", nrow(risky_all), "\n")
  
} else {
  cat("ERROR: Required columns not found for risky loans analysis.\n")
  missing <- setdiff(required_cols, names(data))
  cat("Missing columns:", paste(missing, collapse=", "), "\n")
  cat("Available columns:", paste(names(data), collapse=", "), "\n")
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n\n=== SUMMARY STATISTICS ===\n")

# 1. Percentage of vehicles with LTV > 100% (Underwater)
if ("ltvRatio" %in% names(data)) {
  total_with_ltv <- sum(!is.na(data$ltvRatio))
  underwater_loans <- sum(data$ltvRatio > 100, na.rm = TRUE)
  pct_underwater <- (underwater_loans / total_with_ltv) * 100
  
  cat("\nUnderwater Loans (LTV > 100%):\n")
  cat("  Total vehicles with LTV data:", total_with_ltv, "\n")
  cat("  Vehicles underwater:", underwater_loans, "\n")
  cat("  Percentage underwater:", sprintf("%.2f%%", pct_underwater), "\n")
}

# 2. Percentage of payment to income > 10%
if ("paymentToIncomePercentage" %in% names(data)) {
  total_with_payment_income <- sum(!is.na(data$paymentToIncomePercentage))
  high_payment_income <- sum(data$paymentToIncomePercentage > 0.1, na.rm = TRUE)
  pct_high_payment_income <- (high_payment_income / total_with_payment_income) * 100
  
  cat("\nPayment to Income Ratio > 10%:\n")
  cat("  Total vehicles with payment/income data:", total_with_payment_income, "\n")
  cat("  Vehicles with payment/income > 10%:", high_payment_income, "\n")
  cat("  Percentage with payment/income > 10%:", sprintf("%.2f%%", pct_high_payment_income), "\n")
}

# 3. Percentage of risky loans (payment/income >= 10% AND LTV > 100%)
if ("paymentToIncomePercentage" %in% names(data) && "ltvRatio" %in% names(data)) {
  # Calculate total valid loans (both metrics present)
  total_valid_loans <- sum(!is.na(data$paymentToIncomePercentage) & !is.na(data$ltvRatio))
  
  # Count risky loans
  risky_loans <- sum((data$paymentToIncomePercentage >= .1 & data$ltvRatio > 100 &
                      !is.na(data$paymentToIncomePercentage) & !is.na(data$ltvRatio)), na.rm = TRUE)
  
  pct_risky_loans <- (risky_loans / total_valid_loans) * 100
  
  cat("\nRisky Loans (Payment/Income >= 10% AND LTV > 100%):\n")
  cat("  Total loans with both metrics:", total_valid_loans, "\n")
  cat("  Risky loans:", risky_loans, "\n")
  cat("  Percentage of risky loans:", sprintf("%.2f%%", pct_risky_loans), "\n")
}

# ============================================================================
# DEFAULT SCENARIO ANALYSIS
# ============================================================================

cat("\n\n=== DEFAULT SCENARIO ANALYSIS ===\n")

# Calculate 60+ Day Delinquency Rate
if ("currentDelinquencyStatus" %in% names(data)) {
  # Assuming currentDelinquencyStatus is numeric days
  delinq_60_plus_balance <- sum(data$reportingPeriodBeginningLoanBalanceAmount[data$currentDelinquencyStatus >= 60], na.rm = TRUE)
  total_balance <- sum(data$reportingPeriodBeginningLoanBalanceAmount, na.rm = TRUE)
  cat(sprintf("Current 60+ Day Delinquency Rate (by Balance): %.2f%%\n", (delinq_60_plus_balance / total_balance) * 100))
}

if ("paymentToIncomePercentage" %in% names(data) && "ltvRatio" %in% names(data) && "reportingPeriodBeginningLoanBalanceAmount" %in% names(data)) {
  
  # Create a risk analysis dataframe
  risk_data <- data %>%
    filter(!is.na(paymentToIncomePercentage) & !is.na(ltvRatio) & !is.na(reportingPeriodBeginningLoanBalanceAmount) & !is.na(currentEstimatedVehicleValue)) %>%
    select(assetNumber, paymentToIncomePercentage, ltvRatio, reportingPeriodBeginningLoanBalanceAmount, currentEstimatedVehicleValue)
  
  # Normalize metrics to create a composite risk score (Z-score sum)
  # Higher Score = Higher Risk (High PTI + High LTV)
  risk_data$pti_scaled <- as.vector(scale(risk_data$paymentToIncomePercentage))
  risk_data$ltv_scaled <- as.vector(scale(risk_data$ltvRatio))
  risk_data$risk_score <- risk_data$pti_scaled + risk_data$ltv_scaled
  
  # Sort by risk score descending
  risk_data <- risk_data %>%
    arrange(desc(risk_score))
  
  # Calculate cumulative loss
  # Stressed Loss Severity Model
  # Net Loss = max(0, Balance - (Estimated Value * 0.85) + 2000)
  liquidation_haircut <- 0.85  # 15% drop from book value at auction
  repo_fees <- 2000            # Cost to seize and sell
  
  risk_data$net_loss <- pmax(0, 
      risk_data$reportingPeriodBeginningLoanBalanceAmount - 
      (risk_data$currentEstimatedVehicleValue * liquidation_haircut) + 
      repo_fees
  )
  risk_data$cumulative_loss <- cumsum(risk_data$net_loss)
  risk_data$percent_borrowers <- (seq_len(nrow(risk_data)) / nrow(risk_data)) * 100
  
  total_portfolio_value <- sum(data$reportingPeriodBeginningLoanBalanceAmount, na.rm = TRUE)
  
  # Output Summary Table for 1%, 5%, 10%
  cat("\nStressed Loss Scenario Summary:\n")
  cat(sprintf("%-35s %-20s %-20s\n", "Scenario", "Estimated Loss ($)", "Impairment % of Pool"))
  cat(paste(rep("-", 75), collapse=""), "\n")
  
  for (pct in c(1, 5, 10)) {
    cutoff_index <- which.min(abs(risk_data$percent_borrowers - pct))
    loss_amount <- risk_data$cumulative_loss[cutoff_index]
    impairment_pct <- (loss_amount / total_portfolio_value) * 100
    
    cat(sprintf("Top %d%% Riskiest Borrowers Default   $%-19.2f %.2f%%\n", pct, loss_amount, impairment_pct))
  }
  cat("\n")
  
  # Plot the Loss Curve
  # Filter for top 10% to ensure y-axis scales correctly
  plot_data <- risk_data %>% filter(percent_borrowers <= 10)
  
  p_loss_curve <- ggplot(plot_data, aes(x = percent_borrowers, y = cumulative_loss)) +
    geom_line(color = "#ef5350", size = 1) + # Red color for loss
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title = "Potential Net Loss from Top 10% Riskiest Borrowers",
         subtitle = "Stressed Model: Balance - (Depreciated Value * 0.85) + $2k Fees",
         x = "Percentage of Borrowers Defaulting (Sorted by Risk)",
         y = "Cumulative Net Loss Amount ($)")
    
  print(p_loss_curve)
  ggsave(file.path(output_dir, "potential_loss_curve.png"), p_loss_curve, width = 10, height = 6, dpi = 300)
  cat("Plot saved as:", file.path(output_dir, "potential_loss_curve.png"), "\n")
}

# ============================================================================
# CREDIT SCORE ANALYSIS
# ============================================================================

cat("\n\n=== CREDIT SCORE ANALYSIS ===\n")

if ("obligorCreditScore" %in% names(data)) {
  
  # Filter valid credit scores for plotting
  credit_data <- data %>%
    filter(!is.na(obligorCreditScore))

  # Plot A: Credit vs PTI
  if ("paymentToIncomePercentage" %in% names(data)) {
    p_credit_pti <- ggplot(credit_data, aes(x = obligorCreditScore, y = paymentToIncomePercentage)) +
      geom_point(alpha = 0.1, color = "cyan") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_x_continuous(limits = c(400, 850)) +
      labs(
        title = "Credit Score vs. Payment-to-Income",
        x = "Obligor Credit Score",
        y = "Payment to Income Ratio"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    print(p_credit_pti)
    ggsave(file.path(output_dir, "credit_vs_pti_scatter.png"), p_credit_pti, width = 10, height = 6, dpi = 300)
    cat("Plot saved as:", file.path(output_dir, "credit_vs_pti_scatter.png"), "\n")
  }

  # Plot B: Credit vs LTV
  if ("ltvRatio" %in% names(data)) {
    p_credit_ltv <- ggplot(credit_data, aes(x = obligorCreditScore, y = ltvRatio)) +
      geom_point(alpha = 0.1, color = "cyan") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_x_continuous(limits = c(400, 850)) +
      scale_y_continuous(limits = c(50, 150)) +
      labs(
        title = "Credit Score vs. Mark-to-Market LTV",
        x = "Obligor Credit Score",
        y = "Mark-to-Market LTV (%)"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    print(p_credit_ltv)
    ggsave(file.path(output_dir, "credit_vs_ltv_scatter.png"), p_credit_ltv, width = 10, height = 6, dpi = 300)
    cat("Plot saved as:", file.path(output_dir, "credit_vs_ltv_scatter.png"), "\n")
  }
} else {
  cat("ERROR: 'obligorCreditScore' column not found.\n")
}

# ============================================================================
# REGRESSION & THRESHOLD ANALYSIS
# ============================================================================

cat("\n\n=== REGRESSION & THRESHOLD ANALYSIS ===\n")

if ("obligorCreditScore" %in% names(data) && "ltvRatio" %in% names(data) && "paymentToIncomePercentage" %in% names(data)) {

  # Filter for complete cases for regression
  reg_data <- data %>%
    filter(!is.na(obligorCreditScore) & !is.na(ltvRatio) & !is.na(paymentToIncomePercentage))

  # 1. Regression Analysis
  # Model A: LTV ~ Credit Score
  model_ltv <- lm(ltvRatio ~ obligorCreditScore, data = reg_data)
  sum_ltv <- summary(model_ltv)
  
  # Model B: PTI ~ Credit Score
  model_pti <- lm(paymentToIncomePercentage ~ obligorCreditScore, data = reg_data)
  sum_pti <- summary(model_pti)
  
  # Pearson Correlations
  cor_ltv <- cor(reg_data$ltvRatio, reg_data$obligorCreditScore, method = "pearson")
  cor_pti <- cor(reg_data$paymentToIncomePercentage, reg_data$obligorCreditScore, method = "pearson")
  
  cat("\nRegression Model A: Mark-to-Market LTV ~ Credit Score\n")
  cat(sprintf("  Slope (Beta): %.6f\n", coef(model_ltv)[2]))
  cat(sprintf("  Intercept: %.4f\n", coef(model_ltv)[1]))
  cat(sprintf("  R-squared: %.6f\n", sum_ltv$r.squared))
  cat(sprintf("  Pearson Correlation: %.6f\n", cor_ltv))
  
  cat("\nRegression Model B: Payment-to-Income ~ Credit Score\n")
  cat(sprintf("  Slope (Beta): %.6f\n", coef(model_pti)[2]))
  cat(sprintf("  Intercept: %.4f\n", coef(model_pti)[1]))
  cat(sprintf("  R-squared: %.6f\n", sum_pti$r.squared))
  cat(sprintf("  Pearson Correlation: %.6f\n", cor_pti))
  
  # 2. Threshold "Danger Zone" Counting
  total_pool_count <- nrow(data)
  
  # FICO > 740 & LTV > 110%
  prime_high_ltv_count <- sum(data$obligorCreditScore > 740 & data$ltvRatio > 110, na.rm = TRUE)
  pct_prime_high_ltv <- (prime_high_ltv_count / total_pool_count) * 100
  
  # FICO > 740 & PTI > 12% (0.12)
  prime_high_pti_count <- sum(data$obligorCreditScore > 740 & data$paymentToIncomePercentage > 0.12, na.rm = TRUE)
  pct_prime_high_pti <- (prime_high_pti_count / total_pool_count) * 100
  
  cat("\nThreshold 'Danger Zone' Analysis (Base: Total Pool):\n")
  cat(sprintf("  Prime (FICO > 740) with LTV > 110%%: %.2f%%\n", pct_prime_high_ltv))
  cat(sprintf("  Prime (FICO > 740) with PTI > 12%%:  %.2f%%\n", pct_prime_high_pti))
  
  # Compare Risk Scores for Prime Population
  # Calculate risk scores for the 740+ population using global mean/sd for consistency
  prime_data <- data %>%
    filter(obligorCreditScore > 740 & !is.na(ltvRatio) & !is.na(paymentToIncomePercentage))
    
  if (nrow(prime_data) > 0) {
    mean_pti <- mean(data$paymentToIncomePercentage, na.rm=TRUE)
    sd_pti <- sd(data$paymentToIncomePercentage, na.rm=TRUE)
    mean_ltv <- mean(data$ltvRatio, na.rm=TRUE)
    sd_ltv <- sd(data$ltvRatio, na.rm=TRUE)
    
    prime_data$risk_score <- ((prime_data$paymentToIncomePercentage - mean_pti)/sd_pti) + 
                             ((prime_data$ltvRatio - mean_ltv)/sd_ltv)
                             
    # Define Prime Outliers: (LTV > 110 OR PTI > 0.12)
    prime_data$is_outlier <- (prime_data$ltvRatio > 110 | prime_data$paymentToIncomePercentage > 0.12)
    
    avg_risk_outlier <- mean(prime_data$risk_score[prime_data$is_outlier], na.rm=TRUE)
    avg_risk_rest <- mean(prime_data$risk_score[!prime_data$is_outlier], na.rm=TRUE)
    
    cat("\nRisk Score Comparison (Prime Population FICO > 740):\n")
    cat(sprintf("  Avg Risk Score - Prime Outliers (LTV>110 or PTI>12%%): %.4f\n", avg_risk_outlier))
    cat(sprintf("  Avg Risk Score - Rest of Prime: %.4f\n", avg_risk_rest))
  }
  
  # Summary Table
  cat("\nSensitivity Summary:\n")
  cat(sprintf("%-20s %-15s\n", "Metric", "Slope (Beta)"))
  cat(paste(rep("-", 35), collapse=""), "\n")
  cat(sprintf("%-20s %-15.6f\n", "LTV Sensitivity", coef(model_ltv)[2]))
  cat(sprintf("%-20s %-15.6f\n", "PTI Sensitivity", coef(model_pti)[2]))

} else {
  cat("ERROR: Required columns for regression analysis not found.\n")
}

# ============================================================================
# CONTRACTUAL VS. ECONOMIC REPORTING (SEC WHISTLEBLOWER MODULE)
# ============================================================================

cat("\n\n=== CONTRACTUAL VS. ECONOMIC REPORTING ===\n")

# Ensure paymentExtendedNumber is available and numeric
if ("paymentExtendedNumber" %in% names(data)) {
  data$paymentExtendedNumber <- as.numeric(as.character(data$paymentExtendedNumber))
} else {
  cat("WARNING: 'paymentExtendedNumber' not found. Assuming 0 extensions.\n")
  data$paymentExtendedNumber <- 0
}

# 1. Calculate Balances
total_pool_balance <- sum(data$reportingPeriodBeginningLoanBalanceAmount, na.rm = TRUE)

# Reported 60+ Delinquency
# Logic: currentDelinquencyStatus >= 60
delinq_60_mask <- !is.na(data$currentDelinquencyStatus) & data$currentDelinquencyStatus >= 60
reported_delinq_balance <- sum(data$reportingPeriodBeginningLoanBalanceAmount[delinq_60_mask], na.rm = TRUE)
reported_delinq_rate <- (reported_delinq_balance / total_pool_balance) * 100

# Extension-Adjusted Delinquency
# Logic: currentDelinquencyStatus >= 60 OR paymentExtendedNumber > 0
extended_mask <- !is.na(data$paymentExtendedNumber) & data$paymentExtendedNumber > 0
adjusted_mask <- delinq_60_mask | extended_mask

adjusted_delinq_balance <- sum(data$reportingPeriodBeginningLoanBalanceAmount[adjusted_mask], na.rm = TRUE)
adjusted_delinq_rate <- (adjusted_delinq_balance / total_pool_balance) * 100

# Trigger
trigger_rate <- 2.20

# Table 1 Output
cat("\nTable 1: The Contractual Mechanical Breach (SEC Yardstick)\n")
cat(sprintf("%-45s %10s\n", "Metric", "Value"))
cat(paste(rep("-", 57), collapse=""), "\n")
cat(sprintf("%-45s %9.2f%%\n", "Reported 60+ Day Delinquency Rate", reported_delinq_rate))
cat(sprintf("%-45s %9.2f%%\n", "Extension-Adjusted 60+ Day Delinquency", adjusted_delinq_rate))
cat(sprintf("%-45s %9.2f%%\n", "Year 1 Trigger Threshold", trigger_rate))
cat(sprintf("%-45s %10s\n", "Breach Status", ifelse(adjusted_delinq_rate > trigger_rate, "BREACHED", "SAFE")))

# Table 2: Economic Reality
cat("\nTable 2: The Economic Reality (Loss Severity)\n")
if (exists("risk_data")) {
  cat(sprintf("%-35s %-20s %-20s\n", "Scenario", "Estimated Loss ($)", "Impairment % of Pool"))
  cat(paste(rep("-", 75), collapse=""), "\n")
  for (pct in c(1, 5, 10)) {
    cutoff_idx <- which.min(abs(risk_data$percent_borrowers - pct))
    loss_amt <- risk_data$cumulative_loss[cutoff_idx]
    imp_pct <- (loss_amt / total_pool_balance) * 100
    cat(sprintf("Top %d.0%% Riskiest Borrowers Default   $%-19.2f %.2f%%\n", pct, loss_amt, imp_pct))
  }
} else {
  cat("Risk data model not available (missing required columns).\n")
}

# Visualization
plot_df <- data.frame(Category = factor(c("Reported Delinquency", "Extension-Adjusted", "Trigger Threshold"), levels = c("Reported Delinquency", "Extension-Adjusted", "Trigger Threshold")), Rate = c(reported_delinq_rate, adjusted_delinq_rate, trigger_rate), Type = c("Actual", "Adjusted", "Limit"))

p_breach <- ggplot(plot_df, aes(x = Category, y = Rate, fill = Type)) + geom_col(width = 0.6, alpha = 0.9) + geom_text(aes(label = sprintf("%.2f%%", Rate)), vjust = -0.5, color = "white", fontface = "bold") + scale_fill_manual(values = c("Actual" = "steelblue", "Adjusted" = "#ef5350", "Limit" = "orange")) + labs(title = "Delinquency Trigger Analysis: Reported vs. Extension-Adjusted", subtitle = "Comparison against Year 1 2.20% Contractual Trigger", y = "Rate (%)", x = "") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

print(p_breach)
ggsave(file.path(output_dir, "delinquency_trigger_analysis.png"), p_breach, width = 10, height = 6, dpi = 300)
cat("Plot saved as:", file.path(output_dir, "delinquency_trigger_analysis.png"), "\n")
