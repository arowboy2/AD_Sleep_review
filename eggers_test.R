# Load the libraries
library(meta)
library(metafor)

# Function to run meta-analysis, heterogeneity, Egger's test, and trim-and-fill for each category
run_eggers_test <- function(category_data, category_name) {
  
  # Grouping genetic risks
  category_data$genetics_grouped <- ifelse(category_data$genetics %in% c('APOE4 ≥ 1', 'PRS', 'Other'), 'Genetic Risk of AD', 'No Genetic Risk of AD')
  
  category_data$variance <- category_data$SE^2
  
  meta_result <- rma(yi = category_data$z_score, vi = category_data$variance, method = "REML")
  
  # Print the meta-analysis results
  cat("\nMeta-Analysis Results for:", category_name, "\n")
  
  # Heterogeneity assessment
  cat("\nHeterogeneity for:", category_name, "\n")
  cat("Q-statistic:", meta_result$Q, "\n")
  cat("I² statistic:", meta_result$I2, "\n")
  
  # Publication bias assessment
  # Save funnel plot to a file
  png(filename = paste0("Funnel_Plot_", gsub(" ", "_", category_name), ".png"))
  funnel(meta_result, main = paste("Funnel Plot for", category_name))
  dev.off()  
  
  egger_test <- regtest(meta_result)
  cat("Egger's Test for Publication Bias:", "\n")
  print(egger_test)
  
  # Run trim-and-fill analysis if Egger's test is significant or if you suspect publication bias
  if (category_name == "Sleep Duration & Sleep Efficiency" && egger_test$pval < 0.05) {
    cat("\nRunning Trim-and-Fill for:", category_name, "\n")
    trimfill_result <- trimfill(meta_result)
    print(trimfill_result)
    
    # Save trim-and-fill plot to a file
    png(filename = paste0("Trim_Fill_Plot_", gsub(" ", "_", category_name), ".png"))
    funnel(trimfill_result, main = paste("Trim and Fill Funnel Plot for", category_name))
    dev.off()  # Close the graphics device
  }
}

# Categories to loop through
categories <- c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")

data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

# Loop through each category, running the Egger's test and possibly trim-and-fill
for (category in categories) {
  # Subset data for the current sleep category
  print(category)
  category_data <- subset(data, sleep_main_grouping == category)
  
  # Run the Egger's test for the current category
  run_eggers_test(category_data, category)
}
