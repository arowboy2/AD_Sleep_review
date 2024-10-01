# Load necessary library
library(stats) 

# Convert Beta Coefficients to Z-Scores when Standard Error (SE) is provided
# Define beta and SE values
beta <- c()  # Fill in with your beta coefficient
SE <- c()    # Fill in with your standard error

# Calculate Z-scores from Beta and SE
z_beta <- beta / SE
print("This is the Z-score made from Beta and SE")
print(z_beta)

# If you don't have SE, calculate it from Confidence Intervals (CI)
upper_CI_no_SE <- c()   # Fill in with your upper confidence intervals
lower_CI_no_SE <- c()   # Fill in with your lower confidence intervals
SE <- (upper_CI_no_SE - lower_CI_no_SE) / (2 * 1.96)
print("Standard Errors from CIs")
print(SE)

# Convert P-Values to Z-Scores (last resort method)
# Define p-values
p_values <- c()  # Fill in with your p-values

# Calculate Z-scores for two-tailed p-values
z_p_value_two_tailed <- qnorm(1 - p_values / 2)
print("Z-scores from Two-Tailed P-Values:")
print(z_p_value_two_tailed)

# Calculate Z-scores for one-tailed p-values
z_p_value_one_tailed <- qnorm(1 - p_values)
print("Z-scores from One-Tailed P-Values:")
print(z_p_value_one_tailed)

# Convert Odds Ratios (OR) to Z-Scores
# Define odds ratios and confidence intervals
OR <- c()          # Fill in with your odds ratios
upper_CI <- c()   # Fill in with your upper confidence interval
lower_CI <- c()   # Fill in with your lower confidence interval

# Calculate Log Odds Ratios
log_OR <- log(OR)
print("Log Odds Ratios")
print(log_OR)

# Calculate SE from Confidence Intervals
SE_OR <- (log(upper_CI) - log(lower_CI)) / (2 * 1.96)
print("Standard Errors from CIs for Odds Ratios")
print(SE_OR)

# Calculate Z-scores from Log OR
z_OR <- log_OR / SE_OR
print("Z-scores from Odds Ratios")
print(z_OR)

# Convert Hazard Ratios (HR) to Z-Scores
# Define hazard ratios and confidence intervals
HR <- c()         # Fill in with the hazard ratios
upper_CI_HR <- c()  # Fill in with upper confidence intervals
lower_CI_HR <- c()  # Fill in with lower confidence intervals

# Calculate Log Hazard Ratios
log_HR <- log(HR)
print("Log Hazard Ratios:")
print(log_HR)

# Calculate SE from Confidence Intervals for HR
SE_HR <- (log(upper_CI_HR) - log(lower_CI_HR)) / (2 * 1.96)
print("Standard Errors from CIs for Hazard Ratios")
print(SE_HR)

# Calculate Z-scores from Log HR
z_HR <- log_HR / SE_HR
print("Z-scores from Hazard Ratios")
print(z_HR)

# Converting Test Statistics to Z-Scores
# T-Statistics
t_stat <- c()  # Fill in with the t-statistics
df <- c()      # Fill in with the degrees of freedom

# Approximate Z-scores from T-statistics
z_t <- t_stat   # For large sample sizes, Z approximates the t-statistic
print("Z-scores made from T-Statistics")
print(z_t)

# F-Test
F_stat <- c()  # Fill in with F-statistics
df1 <- c()     # Fill in with the numerator degrees of freedom
df2 <- c()     # Fill in with the denominator degrees of freedom

# Calculate Z-scores from F-Statistics
z_F <- qnorm(1 - pf(F_stat, df1, df2))
print("Z-scores from F-Statistics")
print(z_F)

# Chi-Square Test
chi2_stat <- c()  # Fill in with chi-square statistics

# Z-score approximation for chi-square statistics with df = 1
z_chi2 <- sqrt(chi2_stat)
print("Z-scores from Chi-Square Statistics (df = 1)")
print(z_chi2)

# For df > 1: Exact conversion using normal approximation
df_chi2 <- c()  # Fill in with the degrees of freedom
p_chi2 <- pchisq(chi2_stat, df_chi2, lower.tail = FALSE)
z_chi2_2 <- qnorm(p_chi2)
print("Z-scores from Chi-Square Statistics (df > 1):")
print(z_chi2_2)

# Non-Parametric Tests (Mann–Whitney U, Kruskal-Wallis)
# Mann–Whitney U Test
U <- c()  # Fill in with the U statistic
n1 <- c() # Sample size of group 1
n2 <- c() # Sample size of group 2

# Calculate mean and standard deviation for U
mean_U <- (n1 * n2) / 2
SD_U <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)

# Calculate Z-scores for U
z_U <- (U - mean_U) / SD_U
print("Z-scores from Mann–Whitney U Test")
print(z_U)

# Kruskal-Wallis Test
H <- c()  # Fill in with your Kruskal-Wallis H statistics

# Z-score approximation for large sample sizes
z_H <- sqrt(H)
print("Z-scores from Kruskal-Wallis Test (large sample sizes):")
print(z_H)

# For smaller sample sizes, use p-value conversion
k <- c()  # Number of groups
df_KW <- k - 1
p_H <- pchisq(H, df_KW, lower.tail = FALSE)
z_H_2 <- qnorm(p_H)
print("Z-scores from Kruskal-Wallis Test for smaller sample size")
print(z_H_2)

# For Spearman’s Rank Correlation Coefficient and Pearson’s r
# Define correlation coefficients (r) and sample sizes (n)
r <- c()  # Fill in with your Pearson’s r or Spearman’s ρ
n <- c()  # Fill in with your sample sizes

# Fisher’s Z Transformation
fisher_Z <- 0.5 * log((1 + r) / (1 - r))

# Calculate Standard Error
SE_r <- 1 / sqrt(n - 3)

# Calculate Z-scores
z_r <- fisher_Z / SE_r

# Print results
print("Z-scores from Correlation Coefficients")
print(z_r)