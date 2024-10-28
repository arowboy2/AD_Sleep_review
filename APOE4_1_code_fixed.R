# Load necessary libraries
library(meta)
library(metafor)
library(ggplot2)

# Load the dataset and ensure proper handling of missing values
data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Rename column for z_score with extra space to correct name
colnames(data)[colnames(data) == "z_score "] <- "z_score"

# Filter out PRS and APOE = 0, focusing on APOE4 >=1 only
data_apoe4 <- subset(data, genetics == "APOE4 ≥1")

# Check the number of rows in the filtered data
cat("Number of rows for APOE4 ≥1:", nrow(data_apoe4), "\n")

# Run meta-regression for APOE4 ≥1
if (nrow(data_apoe4) > 0) {
  meta_result_apoe4 <- rma(yi = data_apoe4$z_score, 
                           sei = data_apoe4$SE, 
                           method = "REML", 
                           mods = ~ type_of_sleep_measure,  # Add relevant covariates here
                           test = "z",  # Using the z-test
                           data = data_apoe4)
  # Display results
  print(meta_result_apoe4)
} else {
  cat("No data found for APOE4 ≥ 1. Please check the data.")
}
##### combined covary apoe
# Load necessary libraries
# Load necessary libraries
library(meta)
library(metafor)
library(ggplot2)

# Load the dataset and ensure proper handling of missing values
data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Rename column for z_score with extra space to correct name
colnames(data)[colnames(data) == "z_score "] <- "z_score"

data_apoe4 <- subset(data, genetics == "APOE4 ≥ 1")

cat("Number of rows for APOE4 ≥ 1:", nrow(data_apoe4), "\n")

cat("Available columns in the data:\n")
print(colnames(data_apoe4))

# Run meta-regression for APOE4 ≥ 1 with covariates
if (nrow(data_apoe4) > 0) {
  meta_result_apoe4_covariates <- rma(yi = data_apoe4$z_score, 
                                      sei = data_apoe4$SE, 
                                      mods = ~ N + age + HC_NHC + race + type_of_sleep_measure,  # Add relevant covariates here
                                      method = "REML", 
                                      test = "z",  # Using the z-test
                                      data = data_apoe4)
  
  # Display results
  print(meta_result_apoe4_covariates)
} else {
  cat("No data found for APOE4 ≥ 1. Please check the data.")
}





#####paired with covary
# Load necessary libraries
library(meta)
library(metafor)
library(ggplot2)

# Load the dataset
data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Rename column for z_score due to spacing issue
colnames(data)[colnames(data) == "z_score "] <- "z_score"

data_apoe4 <- subset(data, genetics == "APOE4 ≥1")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Function to check if a factor has 2 or more levels
check_factor_levels <- function(df, var) {
  if (is.factor(df[[var]]) || is.character(df[[var]])) {
    return(length(unique(df[[var]])) > 1)
  }
  return(TRUE)  # If not a factor, return TRUE
}

for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_apoe4, sleep_main_grouping %in% pair_categories)
  
  # Check if there's enough data for running analysesz
  if (nrow(pair_data) > 0) {
    
    # Run meta-regression for APOE4 ≥1 without covarieates
    meta_result <- rma(yi = pair_data$z_score, 
                       sei = pair_data$SE, 
                       method = "REML", 
                       test = "z", 
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    # Meta-regression with covariates if all factors have 2 or more levels
    covariates <- c("N", "age", "HC_NHC", "race", "type_of_sleep_measure")
    if (all(sapply(covariates, function(var) check_factor_levels(pair_data, var)))) {
      meta_result_covariates <- rma(yi = pair_data$z_score, 
                                    sei = pair_data$SE, 
                                    mods = ~ N + HC_NHC + race + type_of_sleep_measure,  # Add covariates
                                    method = "REML", 
                                    test = "z", 
                                    data = pair_data)
      
      cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
      print(meta_result_covariates)
    } else {
      cat("Skipping covariate model for", pair_name, "due to insufficient levels in covariates.\n")
    }
    
  } else {
    cat("Skipping pair:", pair_name, "due to insufficient data.\n")
  }
}
#####
#####paired with nocovary
# Load necessary libraries
library(meta)
library(metafor)
library(ggplot2)

# Load the dataset
data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Rename column for z_score due to spacing issue
colnames(data)[colnames(data) == "z_score "] <- "z_score"

# Filter the dataset to include only APOE4 ≥1 (exclude PRS and APOE = 0)
data_apoe4 <- subset(data, genetics == "APOE4 ≥1")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Function to check if a factor has 2 or more levels
check_factor_levels <- function(df, var) {
  if (is.factor(df[[var]]) || is.character(df[[var]])) {
    return(length(unique(df[[var]])) > 1)
  }
  return(TRUE)  # If not a factor, return TRUE
}

# Loop through each pair of sleep categories for APOE4 ≥1
for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  # Filter the data for the current sleep categories in the pair
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_apoe4, sleep_main_grouping %in% pair_categories)
  
  # Check if there's enough data for analysis
  if (nrow(pair_data) > 0) {
    
    meta_result <- rma(yi = pair_data$z_score, 
                       sei = pair_data$SE, 
                       method = "REML", 
                       test = "z", 
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    covariates <- c("N", "age", "HC_NHC", "race", "type_of_sleep_measure")
    if (all(sapply(covariates, function(var) check_factor_levels(pair_data, var)))) {
      meta_result_covariates <- rma(yi = pair_data$z_score, 
                                    sei = pair_data$SE, 
                                    method = "REML", 
                                    test = "z", 
                                    data = pair_data)
      
      cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
      print(meta_result_covariates)
    } else {
      cat("Skipping covariate model for", pair_name, "due to insufficient levels in covariates.\n")
    }
    
  } else {
    cat("Skipping pair:", pair_name, "due to insufficient data.\n")
  }
}

#single:





# Loop through each unique sleep category
categories <- unique(data_apoe4$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the broad sleep category
  category_data <- subset(data_apoe4, sleep_main_grouping == category)
  

    # Run meta-regression for the current category with only genetic risk of AD
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE, 
                       method = "REML", 
                       test = "z", 
                       data = category_data)
    
    cat("\nMeta-Analysis Results for", category, ":\n")
    print(meta_result)
  } else {
    cat("Skipping category:", category, "due to lack of variation in one or more factors.\n")
  }
}





library(meta)
library(metafor)
library(ggplot2)

# Load the dataset
data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Rename column for z_score due to spacing issue
colnames(data)[colnames(data) == "z_score "] <- "z_score"

data_apoe4 <- subset(data, genetics == "APOE4 ≥1")

# Define the sleep categories
categories <- unique(data_apoe4$sleep_main_grouping)

# Loop through each sleep category for APOE4 ≥1
for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the current category
  category_data <- subset(data_apoe4, sleep_main_grouping == category)
  
  # Check if there's enough data for analysis
  if (nrow(category_data) > 0) {
    
    # Run random effects meta-analysis
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE,
                       mods = ~ N + age + HC_NHC + race + type_of_sleep_measure,  # Add covariates
                       method = "REML", 
                       test = "z", 
                       data = category_data)
    
    cat("\nMeta-analysis results for category", category, ":\n")
    print(meta_result)
    
  } else {
    cat("Skipping category:", category, "due to insufficient data.\n")
  }
}










library(meta)
library(metafor)
library(ggplot2)

data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

colnames(data)[colnames(data) == "z_score "] <- "z_score"

data_apoe4 <- subset(data, genetics == "APOE4 ≥1")

# Define the sleep categories
categories <- unique(data_apoe4$sleep_main_grouping)

# Function to check if a factor has 2 or more levels
check_factor_levels <- function(df, var) {
  if (is.factor(df[[var]]) || is.character(df[[var]])) {
    return(length(unique(df[[var]])) > 1)
  }
  return(TRUE)  # If not a factor, return TRUE
}

# Loop through each sleep category for APOE4 ≥1
for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the current category
  category_data <- subset(data_apoe4, sleep_main_grouping == category)
  
  # Check if there's enough data for analysis
  if (nrow(category_data) > 0) {
    
    # List of potential covariates
    covariates <- c("genetics", "N", "HC_NHC", "race", "age", "type_of_sleep_measure")
    
    # Filter out covariates that don't have 2 or more levels
    valid_covariates <- covariates[sapply(covariates, function(var) check_factor_levels(category_data, var))]
    
    # Build the formula dynamically based on valid covariates
    if (length(valid_covariates) > 0) {
      formula <- as.formula(paste("~", paste(valid_covariates, collapse = " + ")))
      
      # Run random effects meta-analysis with valid covariates
      meta_result <- rma(yi = category_data$z_score, 
                         sei = category_data$SE,
                         mods = formula, 
                         method = "REML", 
                         test = "z", 
                         data = category_data)
      
      cat("\nMeta-analysis results for category", category, ":\n")
      print(meta_result)
      
    } else {
      cat("Skipping covariate model for category:", category, "due to insufficient levels in covariates.\n")
    }
    
  } else {
    cat("Skipping category:", category, "due to insufficient data.\n")
  }
}

