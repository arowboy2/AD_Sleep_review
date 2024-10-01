# Load the libraries
library(meta)
library(metafor)
library(ggplot2)

# Define the genetic risk labels and shapes for legend 
genetic_risk_labels <- c("APOE4 = 0", "APOE4 ≥ 1", "PRS", "Other")
genetic_risk_shapes <- c(16, 17, 18, 15, 1)  # Different shapes for each category

# Define the colors for subjective vs. objective sleep measures
colors <- c("Objective" = "#435F90", "Subjective" = "#FEB359")

run_full_analysis <- function(category_data, category_name) {
  
  # Calculate variance from standard errors
  category_data$variance <- category_data$SE^2
  
  # Run random-effects meta-analysis using the z-scores and calculated variance
  meta_result <- rma(yi = category_data$z_score, vi = category_data$variance, method = "REML")
  
  cat("\nMeta-Analysis Results for:", category_name, "\n")
  print(meta_result)
  
  # Heterogeneity assessment
  cat("\nHeterogeneity for:", category_name, "\n")
  cat("Q-statistic:", meta_result$Q, "\n")
  cat("I² statistic:", meta_result$I2, "\n")
  
  # Publication bias assessment
  if (nrow(category_data) >= 10) {  # Egger's test 
    funnel(meta_result, main = paste("Funnel Plot for", category_name))
    egger_test <- regtest(meta_result)
    cat("Egger's Test for Publication Bias:", "\n")
    print(egger_test)
  } else {
    cat("Publication bias assessment (Egger's test) not performed due to fewer than 10 studies.\n")
  }
  
  # Sensitivity analysis (leave-one-out)
  cat("\nLeave-One-Out Sensitivity Analysis for:", category_name, "\n")
  sensitivity <- leave1out(meta_result)
  print(sensitivity)
  
  plot_data <- data.frame(
    Study = category_data$Study_Identifier,
    MeasureID = paste0(category_data$Study_Identifier, "_", seq_len(nrow(category_data))),
    EffectSize = category_data$z_score, # Use z_score directly
    GeneticRisk = factor(category_data$genetics),
    MeasureType = category_data$type_of_sleep_measure,
    ParticipantN = category_data$N,
    sleep_outcome = category_data$sleep_main_outcomes
  )
  
  total_N <- sum(category_data$N, na.rm = TRUE)
  
  x_limits <- range(plot_data$EffectSize, na.rm = TRUE)
  
  p <- ggplot(plot_data, aes(x = EffectSize, y = paste(Study, ", N=", ParticipantN, ",", sleep_outcome))) +
    geom_point(aes(shape = GeneticRisk, color = MeasureType), size = 5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add dashed vertical line at zero
    scale_shape_manual(values = genetic_risk_shapes) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  
      panel.grid.major.y = element_line(color = "gray", size = 0.5),  
      panel.grid.minor.x = element_line(color = "lightgray", linetype = "dotted"),  
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 8, color = "black"),  
      legend.key.size = unit(0.6, "lines"),
      plot.margin = unit(c(1, 1, 4, 1), "lines"),
      axis.text = element_text(color = "black"),  
      axis.title.x = element_text(color = "black")  
    ) +
    guides(
      shape = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    labs(x = "z-score", y = NULL) +  
    xlim(x_limits)
  
  print(p)
  
  ggsave(filename = paste0("forest_plot_", gsub(" ", "_", category_name), ".png"), plot = p, width = 10, height = 8, dpi = 300)
}

data <- read.csv("~/Downloads/AD_no_interaction_plots.csv", na.strings = c("", "NA"))

categories <- c("Sleep Duration & Sleep Efficiency", "Sleep Quality & Sleep Disturbance", "Circadian Rhythm, Sleep Architecture, & Disorders")

for (category in categories) {
  category_data <- subset(data, sleep_main_grouping == category)
  
  run_full_analysis(category_data, category)
}

