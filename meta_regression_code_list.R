library(meta)
library(metafor)
library(ggplot2)

data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

colnames(data)[colnames(data) == "z_score "] <- "z_score"

# Remove PRS
data_filtered <- subset(data, genetics != "PRS")

#confirm filtering
unique(data_filtered$genetics)


# Run meta-regression - genetics (APOE =0 sign flipped and APOE 4>= 1 kept)
meta_result_combined <- rma(yi = data_filtered$z_score, 
                            sei = data_filtered$SE, 
                            mods = ~ genetics, 
                            method = "REML", 
                            test = "z",  # Using the z-test rather than knapp
                            data = data_filtered)

# Show results
cat("\nMeta-Analysis Results for Combined Sleep Categories:\n")
print(meta_result_combined)
##########
#covarying for things for combined for APOE
meta_result_combined <- rma(yi = data_filtered$z_score, 
                            sei = data_filtered$SE, 
                            mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure, 
                            method = "REML", 
                            test = "z",  # Using the z-test
                            data = data_filtered)

cat("\nMeta-Analysis Results for Combined Sleep Categories:\n")
print(meta_result_combined)







###combined PRS

colnames(data)[colnames(data) == "z_score "] <- "z_score"

data_filtered <- subset(data, genetics == "PRS")

data_filtered$genetics <- factor(data_filtered$genetics, levels = "PRS")

meta_result_prs <- rma(yi = data_filtered$z_score, 
                       sei = data_filtered$SE, 
                       method = "REML", 
                       test = "z", 
                       data = data_filtered)

cat("\nMeta-Analysis Results for PRS in Genetics Category:\n")
print(meta_result_prs)

#covarying for things combined- PRS
meta_result_prs_with_covariates <- rma(yi = data_filtered$z_score, 
                                       sei = data_filtered$SE, 
                                       mods = ~ N + HC_NHC + race + age + type_of_sleep_measure, 
                                       method = "REML", 
                                       test = "z",  # Using the z-test
                                       data = data_filtered)

# Show results
cat("\nMeta-Analysis Results for PRS in Genetics Category with Covariates:\n")
print(meta_result_prs_with_covariates)


######
##covarying + LOOPING SINGLE

# Remove PRS
data_filtered <- subset(data, genetics != "PRS")

# Confirm it worked
unique(data_filtered$genetics)

# Loopy- through each unique sleep category
categories <- unique(data_filtered$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # sort by broad sleep category
  category_data <- subset(data_filtered, sleep_main_grouping == category)
  
  # sanity check for if there are any categorical variables with only one level- got this from stack, let me know if not kosher
  if (length(unique(category_data$genetics)) > 1 &&
      length(unique(category_data$race)) > 1 &&
      length(unique(category_data$HC_NHC)) > 1 &&
      length(unique(category_data$type_of_sleep_measure)) > 1) {
    
    # Run meta-regression for  X category
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE, 
                       mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure, 
                       method = "REML", 
                       test = "z",  
                       data = category_data)
    
    cat("\nMeta-Analysis Results for", category, ":\n")
    print(meta_result)
  } else {
    cat("Skipping category:", category, "due to lack of variation in one or more factors.\n")
  }
}
#####single no covary APOE
# Filter out PRS from the data
data_filtered <- subset(data, genetics != "PRS")

# Confirm the filtering worked
unique(data_filtered$genetics)

# Loop through each unique sleep category
categories <- unique(data_filtered$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the broad sleep category
  category_data <- subset(data_filtered, sleep_main_grouping == category)
  
  # Check if there are enough levels for categorical variables
  if (length(unique(category_data$genetics)) > 1) {
    
    # Run meta-regression for the current category with only the genetic risk of AD
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE, 
                       mods = ~ genetics,  # Only genetic risk of AD as moderator
                       method = "REML", 
                       test = "z", 
                       data = category_data)
    
    cat("\nMeta-Analysis Results for", category, ":\n")
    print(meta_result)
  } else {
    cat("Skipping category:", category, "due to lack of variation in genetics.\n")
  }
}

###
# Remove PRS
data_filtered <- subset(data, genetics != "PRS")

# Confirm it worked
unique(data_filtered$genetics)

# Loopy- through each unique sleep category
categories <- unique(data_filtered$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # sort by broad sleep category
  category_data <- subset(data_filtered, sleep_main_grouping == category)
  
  # sanity check for if there are any categorical variables with only one level- got this from stack, let me know if not kosher
  if (length(unique(category_data$genetics)) > 1 &&
      length(unique(category_data$race)) > 1 &&
      length(unique(category_data$HC_NHC)) > 1 &&
      length(unique(category_data$type_of_sleep_measure)) > 1) {
    
    # Run meta-regression for  X category
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE, 
                       mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure, 
                       method = "REML", 
                       test = "z",  
                       data = category_data)
    
    cat("\nMeta-Analysis Results for", category, ":\n")
    print(meta_result)
  } else {
    cat("Skipping category:", category, "due to lack of variation in one or more factors.\n")
  }
}
#####single no covary PRS

data_prs <- subset(data, genetics == "PRS")

data_prs$genetics <- factor(data_prs$genetics, levels = "PRS")

# Loop through each unique sleep category
categories <- unique(data_prs$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the broad sleep category
  category_data <- subset(data_prs, sleep_main_grouping == category)
  
  # Check if there is sufficient data in the category
  if (nrow(category_data) > 0) {
    # Run meta-regression for the current category without moderators (since this is a PRS-only analysis)
    meta_result_prs <- rma(yi = category_data$z_score, 
                           sei = category_data$SE, 
                           method = "REML", 
                           test = "z", 
                           data = category_data)
    
    cat("\nMeta-Analysis Results for PRS in", category, ":\n")
    print(meta_result_prs)
  } else {
    cat("Skipping category:", category, "due to insufficient data.\n")
  }
}

## paired APOE with covary

data_filtered <- subset(data, genetics != "PRS")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Loop through each pair of sleep categories
for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  # Filter the data to include only the selected sleep categories for the current pair
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_filtered, sleep_main_grouping %in% pair_categories)
  
  # Check if there is enough variation in the genetics category
  if (length(unique(pair_data$genetics)) > 1) {
    
    # Run meta-regression with genetic risk of AD
    meta_result <- rma(yi = pair_data$z_score, 
                       sei = pair_data$SE, 
                       mods = ~ genetics,  # Only genetic risk of AD as moderator
                       method = "REML", 
                       test = "z", 
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    # Meta-regression with covariates
    meta_result_covariates <- rma(yi = pair_data$z_score, 
                                  sei = pair_data$SE, 
                                  mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure,  # Include covariates
                                  method = "REML", 
                                  test = "z", 
                                  data = pair_data)
    
    cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
    print(meta_result_covariates)
    
  } else {
    cat("Skipping pair:", pair_name, "due to lack of variation in genetics.\n")
  }
}

## paired APOE with covary

data_filtered <- subset(data, genetics != "PRS")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Loop through each pair of sleep categories
for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  # Filter the data to include only the selected sleep categories for the current pair
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_filtered, sleep_main_grouping %in% pair_categories)
  
  # Check if there is enough variation in the genetics category
  if (length(unique(pair_data$genetics)) > 1) {
    
    # Run meta-regression with genetic risk of AD
    meta_result <- rma(yi = pair_data$z_score, 
                       sei = pair_data$SE, 
                       mods = ~ genetics,  # Only genetic risk of AD as moderator
                       method = "REML", 
                       test = "z", 
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    # Meta-regression with covariates
    meta_result_covariates <- rma(yi = pair_data$z_score, 
                                  sei = pair_data$SE, 
                                  mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure,  # Include covariates
                                  method = "REML", 
                                  test = "z", 
                                  data = pair_data)
    
    cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
    print(meta_result_covariates)
    
  } else {
    cat("Skipping pair:", pair_name, "due to lack of variation in genetics.\n")
  }
}


## paired APOE with no covary

data_filtered <- subset(data, genetics != "PRS")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Loop through each pair of sleep categories
for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  # Filter the data to include only the selected sleep categories for the current pair
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_filtered, sleep_main_grouping %in% pair_categories)
  
  # Check if there is enough variation in the genetics category
  if (length(unique(pair_data$genetics)) > 1) {
    
    # Run meta-regression with genetic risk of AD
    meta_result <- rma(yi = pair_data$z_score, 
                       sei = pair_data$SE, 
                       mods = ~ genetics,  # Only genetic risk of AD as moderator
                       method = "REML", 
                       test = "z", 
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    # Meta-regression with covariates
    meta_result_covariates <- rma(yi = pair_data$z_score, 
                                  sei = pair_data$SE, 
                                  mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure,  # Include covariates
                                  method = "REML", 
                                  test = "z", 
                                  data = pair_data)
    
    cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
    print(meta_result_covariates)
    
  } else {
    cat("Skipping pair:", pair_name, "due to lack of variation in genetics.\n")
  }
}


######paired prs

data_filtered <- subset(data, genetics != "PRS")

# Define the sleep category pairs
category_pairs <- list(
  "Pair 1" = c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance"),
  "Pair 2" = c("Sleep Duration & Sleep Efficiency", "Circadian Rhythm, Sleep Architecture, & Disorders"),
  "Pair 3" = c("Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")
)

# Loop through each pair of sleep categories
for (pair_name in names(category_pairs)) {
  cat("\nProcessing:", pair_name, "\n")
  
  # Filter the data to include only the selected sleep categories for the current pair
  pair_categories <- category_pairs[[pair_name]]
  pair_data <- subset(data_filtered, sleep_main_grouping %in% pair_categories)
  
  # Check if there is enough variation in the genetics category
  if (length(unique(pair_data$genetics)) > 1) {
    
    # Run meta-regression with genetic risk of AD (excluding PRS)
    meta_result <- rma(yi = pair_data$z_score,
                       sei = pair_data$SE,
                       mods = ~ genetics,  # Only genetic risk of AD as moderator
                       method = "REML",
                       test = "z",
                       data = pair_data)
    
    cat("\nMeta-Analysis Results for", pair_name, ":\n")
    print(meta_result)
    
    # Meta-regression with covariates
    meta_result_covariates <- rma(yi = pair_data$z_score,
                                  sei = pair_data$SE,
                                  mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure,  # Include covariates
                                  method = "REML",
                                  test = "z",
                                  data = pair_data)
    
    cat("\nMeta-Analysis Results with Covariates for", pair_name, ":\n")
    print(meta_result_covariates)
    
  } else {
    cat("Skipping pair:", pair_name, "due to lack of variation in genetics.\n")
  }
}


################APOE >= 1 only

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
                           test = "z",  # Using the z-test
                           data = data_apoe4)
  # Display results
  print(meta_result_apoe4)
} else {
  cat("No data found for APOE4 ≥ 1. Please check the data.")
}
##### combined covary apoe

data_apoe4 <- subset(data, genetics == "APOE4 ≥ 1")

# Check the column names for covariates in the dataset
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
    
    # Run meta-regression for APOE4 ≥1 without genetics as a moderator
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
                                    mods = ~ N + age + HC_NHC + race + type_of_sleep_measure,  # Add covariates
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
    
    # Run meta-regression for APOE4 ≥1 without genetics as a moderator
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
categories <- unique(data_filtered$sleep_main_grouping)

for (category in categories) {
  cat("\nProcessing category:", category, "\n")
  
  # Filter by the broad sleep category
  category_data <- subset(data_apoe4, sleep_main_grouping == category)
  
  # Check if there are enough levels for categorical variables
  if (length(unique(category_data$genetics)) > 1 && 
      length(unique(category_data$race)) > 1 && 
      length(unique(category_data$HC_NHC)) > 1 && 
      length(unique(category_data$type_of_sleep_measure)) > 1) {
    
    # Run meta-regression for the current category with only genetic risk of AD
    meta_result <- rma(yi = category_data$z_score, 
                       sei = category_data$SE, 
                       mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure, 
                       method = "REML", 
                       test = "z", 
                       data = category_data)
    
    cat("\nMeta-Analysis Results for", category, ":\n")
    print(meta_result)
  } else {
    cat("Skipping category:", category, "due to lack of variation in one or more factors.\n")
  }
}


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
                       mods = ~ genetics + N + HC_NHC + race + age + type_of_sleep_measure, 
                       method = "REML", 
                       test = "z", 
                       data = category_data)
    
    cat("\nMeta-analysis results for category", category, ":\n")
    print(meta_result)
    
  } else {
    cat("Skipping category:", category, "due to insufficient data.\n")
  }
}


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









