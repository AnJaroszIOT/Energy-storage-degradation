# Load necessary libraries
library(MCMCpack)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Read the Excel file
data <- read_excel("Dataset.xlsx")

# Create the dataframe with the specified columns
df <- data.frame(
  Discharge_Time = data[[2]],
  Decrement_3.6_3.4V = data[[3]],
  Max_Voltage_Dischar = data[[4]],
  Min_Voltage_Charg = data[[5]],
  Time_at_4.15V = data[[6]],
  Time_constant_current = data[[7]],
  Charging_time = data[[8]] * 10  # Reduce charging time by a factor of 10
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Fit the MCMC model using MCMCpack with adjusted parameters
result <- MCMCregress(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                      data = df_filtered,
                      b0.start = 0, b1.start = 0, b2.start = 0, sigma.start = 1,
                      mcmc = 20000, burnin = 5000, thin = 10, 
                      adapt_delta = 0.95, max_treedepth = 15)

# Extract the posterior samples
posterior_samples <- as.data.frame(result)

# Check the names of the posterior samples
print(names(posterior_samples))

# Function to calculate metrics and summary statistics
calculate_metrics <- function(data, predictions, posterior_samples) {
  sigma_est <- mean(posterior_samples$sigma)
  n <- length(data)
  
  # Log-likelihood
  ll <- -0.5 * n * log(2 * pi * sigma_est^2) - sum((data - predictions)^2) / (2 * sigma_est^2)
  
  # AIC, BIC, WAIC
  k <- ncol(posterior_samples) - 1  # Number of parameters
  aic <- -2 * ll + 2 * k
  bic <- -2 * ll + log(n) * k
  
  log_lik_matrix <- matrix(NA, nrow = nrow(posterior_samples), ncol = n)
  for (i in 1:nrow(posterior_samples)) {
    sigma <- posterior_samples$sigma[i]
    predictions_i <- posterior_samples[i, "b0"] + 
      posterior_samples[i, "Discharge_Time"] * df_filtered$Discharge_Time + 
      posterior_samples[i, "Decrement_3.6_3.4V"] * df_filtered$Decrement_3.6_3.4V + 
      posterior_samples[i, "Time_constant_current"] * df_filtered$Time_constant_current
    log_lik_matrix[i, ] <- dnorm(data, mean = predictions_i, sd = sigma, log = TRUE)
  }
  waic <- -2 * (mean(rowSums(exp(log_lik_matrix))) - sum(log(rowSums(exp(log_lik_matrix)))))
  
  # Summary statistics
  summary_stats <- data.frame(
    Metric = c("Min", "Q1", "Median", "Q3", "Max"),
    Value = c(min(data), quantile(data, 0.25), median(data), quantile(data, 0.75), max(data))
  )
  
  return(list(AIC = aic, BIC = bic, WAIC = waic, Likelihood = ll, SD_Error = sd(predictions - data), Summary_Stats = summary_stats))
}

# Calculate predictions using the posterior mean coefficients
predictions <- rowMeans(posterior_samples[, c("b0", "Discharge_Time", "Decrement_3.6_3.4V", "Time_constant_current")], na.rm = TRUE) + 
  df_filtered$Discharge_Time * rowMeans(posterior_samples[, "Discharge_Time"], na.rm = TRUE) + 
  df_filtered$Decrement_3.6_3.4V * rowMeans(posterior_samples[, "Decrement_3.6_3.4V"], na.rm = TRUE) + 
  df_filtered$Time_constant_current * rowMeans(posterior_samples[, "Time_constant_current"], na.rm = TRUE)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions)

# Plot 1: Actual vs Predicted
p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate metrics for Max Voltage Discharge
metrics <- calculate_metrics(df_filtered$Max_Voltage_Dischar, predictions, posterior_samples)

# Print summary table for plot 1
print("Summary Table for Actual vs Predicted Values:")
summary_table1 <- data.frame(
  Metric = c("AIC", "BIC", "WAIC", "Likelihood", "SD Error"),
  Value = c(metrics$AIC, metrics$BIC, metrics$WAIC, metrics$Likelihood, metrics$SD_Error)
)
print(summary_table1)
print(metrics$Summary_Stats)

# Function to create plots and summary tables for each variable
create_plot_with_summary <- function(x, y, x_label, y_label, title) {
  plot_data <- data.frame(Actual = df_filtered[[y]], Predicted = predictions)
  metrics <- calculate_metrics(df_filtered[[y]], predictions, posterior_samples)
  
  p <- ggplot(plot_data, aes_string(x = "Actual", y = "Predicted")) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "darkred", se = FALSE) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))
  
  summary_table <- data.frame(
    Metric = c("AIC", "BIC", "WAIC", "Likelihood", "SD Error"),
    Value = c(metrics$AIC, metrics$BIC, metrics$WAIC, metrics$Likelihood, metrics$SD_Error)
  )
  
  return(list(plot = p, summary_table = summary_table, summary_stats = metrics$Summary_Stats))
}

# Create plots and summary tables for each variable
results_decrement <- create_plot_with_summary("Decrement_3.6_3.4V", "Max_Voltage_Dischar", 
                                              "Decrement 3.6-3.4V", "Max Voltage Discharge", 
                                              "Max Voltage Discharge vs Decrement 3.6-3.4V")
results_discharge_time <- create_plot_with_summary("Discharge_Time", "Max_Voltage_Dischar", 
                                                   "Discharge Time", "Max Voltage Discharge", 
                                                   "Max Voltage Discharge vs Discharge Time")
results_time_constant <- create_plot_with_summary("Time_constant_current", "Max_Voltage_Dischar", 
                                                  "Time Constant Current", "Max Voltage Discharge", 
                                                  "Max Voltage Discharge vs Time Constant Current")

# Arrange all plots in a grid
grid.arrange(p1, results_decrement$plot, results_discharge_time$plot, results_time_constant$plot, ncol = 2)

# Print summary tables for each plot
print("Summary Table for Max Voltage Discharge vs Decrement 3.6-3.4V:")
print(results_decrement$summary_table)
print(results_decrement$summary_stats)

print("Summary Table for Max Voltage Discharge vs Discharge Time:")
print(results_discharge_time$summary_table)
print(results_discharge_time$summary_stats)

print("Summary Table for Max Voltage Discharge vs Time Constant Current:")
print(results_time_constant$summary_table)
print(results_time_constant$summary_stats)

# Summary for Actual and Predicted Charging Time
charging_time_summary <- calculate_metrics(df_filtered$Charging_time, predictions, posterior_samples)

# Print the charging time summary table
print("Summary Table for Actual and Predicted Charging Time:")
charging_time_table <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Actual = charging_time_summary$Summary_Stats$Value,
  Predicted = calculate_metrics(predictions, predictions, posterior_samples)$Summary_Stats$Value
)
print(charging_time_table)