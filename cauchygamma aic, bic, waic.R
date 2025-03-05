# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(gridExtra)
library(knitr)  # For table formatting

# Function to calculate WAIC
calculate_waic <- function(log_likelihood, n, k) {
  waic <- -3 * (log_likelihood - k)  # Correct WAIC calculation
  return(waic)
}

# Assuming df_filtered is already defined and contains the relevant data
# Fit distributions with specified model names
Norm <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
Weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
Gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
Cauchy <- fitdist(df_filtered$Max_Voltage_Dischar, "cauchy")
Lognorm <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")
Logarit <- fitdist(log(df_filtered$Max_Voltage_Dischar), "norm")

# Function to calculate model metrics
calculate_model_metrics <- function(model) {
  log_likelihood <- logLik(model)
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  waic_value <- calculate_waic(log_likelihood, length(df_filtered$Max_Voltage_Dischar), length(coef(model)))
  
  summary_df <- data.frame(
    Model = deparse(substitute(model)),
    AIC = aic_value,
    BIC = bic_value,
    WAIC = waic_value,
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

# Calculate metrics for each model
model_summaries <- bind_rows(
  calculate_model_metrics(Norm),
  calculate_model_metrics(Weibull),
  calculate_model_metrics(Gamma),
  calculate_model_metrics(Cauchy),
  calculate_model_metrics(Lognorm),
  calculate_model_metrics(Logarit)
)

# Print the model summaries as a table
print(kable(model_summaries, format = "markdown"))

# Calculate summary statistics
summary_stats <- data.frame(
  Statistic = c("Min", "3Q", "Median", "5Q", "Max"),
  Value = c(
    min(df_filtered$Max_Voltage_Dischar),
    quantile(df_filtered$Max_Voltage_Dischar, 0.75),
    median(df_filtered$Max_Voltage_Dischar),
    quantile(df_filtered$Max_Voltage_Dischar, 0.95),
    max(df_filtered$Max_Voltage_Dischar)
  )
)

# Create individual plots for each statistic
plots <- lapply(1:nrow(summary_stats), function(i) {
  p <- ggplot(data = summary_stats[i, ], aes(x = Statistic, y = Value)) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
    labs(title = paste("Value of", summary_stats$Statistic[i]), x = "Statistic", y = "Value") +
    theme_minimal() +
    ylim(0, max(summary_stats$Value) * 1.1)  # Adjust y-axis limit for better visibility
  
  return(p)
})

# Arrange plots in a grid with 1 column and 5 rows
print(grid.arrange(grobs = plots, nrow = 5))

# Create a point plot for AIC, BIC, and WAIC
metrics_to_plot <- c("AIC", "BIC", "WAIC")

# Create plots for model metrics
metric_plots <- lapply(metrics_to_plot, function(metric) {
  p <- ggplot(model_summaries, aes_string(x = "Model", y = metric)) +
    geom_point(size = 4, color = "blue") +
    geom_line(aes(group = 1), color = "blue") +
    geom_text(aes(label = round(get(metric), 2)), vjust = -0.5, size = 4) +
    labs(title = paste(metric, "for Each Model"), x = "Model", y = metric) +
    theme_minimal()
  
  # Set y-axis limits based on the metric
  if (metric == "AIC" || metric == "BIC") {
    p <- p + ylim(-80000, -18000)
  } else if (metric == "WAIC") {
    p <- p + ylim(-120000, 35000)
  }
  
  return(p)
})

# Arrange plots in a grid with 1 column and 3 rows
print(grid.arrange(grobs = metric_plots, nrow = 3))