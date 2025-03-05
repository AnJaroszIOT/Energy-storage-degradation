# Load necessary libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(bayesm)

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

# Prepare data for bayesm
data_list <- list(
  y = df_filtered$Max_Voltage_Dischar,
  X = as.matrix(df_filtered[, c("Discharge_Time", "Decrement_3.6_3.4V", "Time_constant_current")])
)

# Fit the model using Gibbs sampling
mcmc_fit <- runireg(Data = data_list, Mcmc = list(R = 10000, nprint = 1000))

# Summarize the results
summary(mcmc_fit)

# Extract posterior samples
posterior_samples <- mcmc_fit$betadraw

# Function to calculate AIC
calculate_aic <- function(log_likelihood, num_params) {
  return(-2 * log_likelihood + 2 * num_params)
}

# Function to calculate BIC
calculate_bic <- function(log_likelihood, num_params, num_obs) {
  return(-2 * log_likelihood + log(num_obs) * num_params)
}

# Function to calculate WAIC
calculate_waic <- function(log_likelihood_matrix) {
  log_likelihood_mean <- rowMeans(log_likelihood_matrix)
  waic <- -2 * (mean(log_likelihood_mean) - mean(apply(log_likelihood_matrix, 1, var)))
  return(waic)
}

# Function to calculate metrics and return as a data frame
calculate_metrics <- function(predictions, actual_values) {
  log_likelihood_matrix <- -0.5 * (nrow(data_list$X) * log(2 * pi) + rowSums((actual_values - predictions)^2))
  log_likelihood <- mean(log_likelihood_matrix)
  
  num_params <- ncol(posterior_samples)
  num_obs <- length(actual_values)
  
  aic <- calculate_aic(log_likelihood, num_params)
  bic <- calculate_bic(log_likelihood, num_params, num_obs)
  waic <- calculate_waic(log_likelihood_matrix)
  
  sd_error <- sd(predictions)
  prediction_error <- sqrt(mean((actual_values - rowMeans(predictions))^2))
  
  return(data.frame(AIC = aic, BIC = bic, WAIC = waic, LogLikelihood = log_likelihood, 
                    SD_Error = sd_error, Prediction_Error = prediction_error))
}

# Generate predictions
predictions <- data_list$X %*% t(posterior_samples)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = rowMeans(predictions))

p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate metrics for the first plot
metrics_p1 <- calculate_metrics(predictions, df_filtered$Max_Voltage_Dischar)

# Print metrics for the first plot
cat("Metrics for Actual vs Predicted Values:\n")
print(metrics_p1)

# Additional plots with regression lines
p2 <- ggplot(df_filtered, aes(x = Decrement_3.6_3.4V, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Decrement 3.6-3.4V", x = "Decrement 3.6-3.4V", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Generate predictions for the second plot
predictions_p2 <- data_list$X[, 2] %*% t(posterior_samples)

# Calculate metrics for the second plot
metrics_p2 <- calculate_metrics(predictions_p2, df_filtered$Max_Voltage_Dischar)

# Print metrics for the second plot
cat("Metrics for Max Voltage Discharge vs Decrement 3.6-3.4V:\n")
print(metrics_p2)

p3 <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Discharge Time", x = "Discharge Time", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Generate predictions for the third plot
predictions_p3 <- data_list$X[, 1] %*% t(posterior_samples)

# Calculate metrics for the third plot
metrics_p3 <- calculate_metrics(predictions_p3, df_filtered$Max_Voltage_Dischar)

# Print metrics for the third plot
cat("Metrics for Max Voltage Discharge vs Discharge Time:\n")
print(metrics_p3)

p4 <- ggplot(df_filtered, aes(x = Time_constant_current, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Time Constant Current", x = "Time Constant Current", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Generate predictions for the fourth plot
predictions_p4 <- data_list$X[, 3] %*% t(posterior_samples)

# Calculate metrics for the fourth plot
metrics_p4 <- calculate_metrics(predictions_p4, df_filtered$Max_Voltage_Dischar)

# Print metrics for the fourth plot
cat("Metrics for Max Voltage Discharge vs Time Constant Current:\n")
print(metrics_p4)

# Arrange all plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)