# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(gridExtra)

# Function to calculate WAIC
calculate_waic <- function(log_likelihood, n, k) {
  waic <- -2 * (log_likelihood - k)
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

# Function to calculate Log-Likelihood, SD Error, and Error
calculate_error_metrics <- function(model) {
  log_likelihood <- logLik(model)
  sd_error <- sqrt(diag(vcov(model)))
  
  summary_df <- data.frame(
    Model = deparse(substitute(model)),
    LogLikelihood = as.numeric(log_likelihood),
    SD_Error = sd_error[1],  # Assuming the first parameter is the intercept
    Error = sd_error[2],      # Assuming the second parameter is the standard deviation
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

# Calculate metrics for each model
error_summaries <- bind_rows(
  calculate_error_metrics(Norm),
  calculate_error_metrics(Weibull),
  calculate_error_metrics(Gamma),
  calculate_error_metrics(Cauchy),
  calculate_error_metrics(Lognorm),
  calculate_error_metrics(Logarit)
)

# Print the error summaries table
print("Error Summaries Table:")
print(error_summaries)

# Create a point plot for Log-Likelihood, SD Error, and Error
error_metrics_to_plot <- c("LogLikelihood", "SD_Error", "Error")

error_plots <- lapply(error_metrics_to_plot, function(metric) {
  p <- ggplot(error_summaries, aes_string(x = "Model", y = metric)) +
    geom_point(size = 4, color = "blue") +
    geom_line(aes(group = 1), color = "blue") +
    geom_text(aes(label = round(get(metric), 2)), vjust = -0.5, size = 4) +
    labs(title = paste(metric, "for Each Model"), x = "Model", y = metric) +
    theme_minimal()
  
  # Set y-axis limits based on the metric
  if (metric == "LogLikelihood") {
    p <- p + ylim(10000, 45000)  # Set y-axis limits for Log-Likelihood
  } else if (metric == "Error") {
    p <- p + ylim(0, 10)  # Set y-axis limits for Error
  } else {
    p <- p + ylim(0, 40)  # Set y-axis limits for SD Error
  }
  
  return(p)
})

# Arrange plots in a grid with 1 column and 3 rows
print(grid.arrange(grobs = error_plots, nrow = 3))
