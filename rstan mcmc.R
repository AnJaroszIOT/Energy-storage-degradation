# Load necessary libraries
library(rstan)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)

# Read the Excel file
data <- read_excel("Dataset.xlsx")

# Create the dataframe with the specified columns
df <- data.frame(
  Discharge_Time = data[[2]],
  Decrement_3_6_3_4V = data[[3]],  # Changed from Decrement_3.6_3.4V
  Max_Voltage_Dischar = data[[4]],
  Min_Voltage_Charg = data[[5]],
  Time_at_4.15V = data[[6]],
  Time_constant_current = data[[7]],
  Charging_time = data[[8]] * 10  # Reduce charging time by a factor of 10
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Define the Stan model
stan_code <- "
data {
  int<lower=0> N;
  vector[N] Discharge_Time;
  vector[N] Decrement_3_6_3_4V;  // Changed from Decrement_3.6_3.4V
  vector[N] Time_constant_current;
  vector[N] Max_Voltage_Dischar;
}
parameters {
  vector[3] beta;
  real<lower=0> sigma;
}
model {
  Max_Voltage_Dischar ~ normal(Discharge_Time * beta[1] + Decrement_3_6_3_4V * beta[2] + Time_constant_current * beta[3], sigma);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
}
"

# Fit the MCMC model using rstan with increased adapt_delta and iterations
model <- stan(model_code = stan_code, data = list(
  N = nrow(df_filtered),
  Discharge_Time = df_filtered$Discharge_Time,
  Decrement_3_6_3_4V = df_filtered$Decrement_3_6_3_4V,  # Changed from Decrement_3.6_3.4V
  Time_constant_current = df_filtered$Time_constant_current,
  Max_Voltage_Dischar = df_filtered$Max_Voltage_Dischar
), iter = 4000, warmup = 2000, chains = 4, cores = 4, control = list(adapt_delta = 0.95, max_treedepth = 15))

# Check for convergence issues
print(summary(model))

# Extract the posterior samples
posterior_samples <- extract(model)

# Calculate the mean, standard deviation, and 95% credible interval for the coefficients
coef_summary <- data.frame(
  Discharge_Time_mean = mean(posterior_samples$beta[,1]),
  Discharge_Time_sd = sd(posterior_samples$beta[,1]),
  Discharge_Time_2.5 = quantile(posterior_samples$beta[,1], 0.025),
  Discharge_Time_97.5 = quantile(posterior_samples$beta[,1], 0.975),
  Decrement_3_6_3_4V_mean = mean(posterior_samples$beta[,2]),
  Decrement_3_6_3_4V_sd = sd(posterior_samples$beta[,2]),
  Decrement_3_6_3_4V_2.5 = quantile(posterior_samples$beta[,2], 0.025),
  Decrement_3_6_3_4V_97.5 = quantile(posterior_samples$beta[,2], 0.975),
  Time_constant_current_mean = mean(posterior_samples$beta[,3]),
  Time_constant_current_sd = sd(posterior_samples$beta[,3]),
  Time_constant_current_2.5 = quantile(posterior_samples$beta[,3], 0.025),
  Time_constant_current_97.5 = quantile(posterior_samples$beta[,3], 0.975)
)

# Print the coefficient summary
print(coef_summary)

# Generate predictions using the posterior samples
predictions <- sapply(1:nrow(posterior_samples$beta), function(i) {
  posterior_samples$beta[i, 1] * df_filtered$Discharge_Time +
    posterior_samples$beta[i, 2] * df_filtered$Decrement_3_6_3_4V +
    posterior_samples$beta[i, 3] * df_filtered$Time_constant_current
})

# Calculate the mean of the predictions
predictions_mean <- rowMeans(predictions)

# Calculate the mean, standard deviation, and 95% credible interval for the coefficients
coef_summary <- data.frame(
  Discharge_Time_mean = mean(posterior_samples$beta[,1]),
  Discharge_Time_sd = sd(posterior_samples$beta[,1]),
  Discharge_Time_2.5 = quantile(posterior_samples$beta[,1], 0.025),
  Discharge_Time_97.5 = quantile(posterior_samples$beta[,1], 0.975),
  Decrement_3_6_3_4V_mean = mean(posterior_samples$beta[,2]),
  Decrement_3_6_3_4V_sd = sd(posterior_samples$beta[,2]),
  Decrement_3_6_3_4V_2.5 = quantile(posterior_samples$beta[,2], 0.025),
  Decrement_3_6_3_4V_97.5 = quantile(posterior_samples$beta[,2], 0.975),
  Time_constant_current_mean = mean(posterior_samples$beta[,3]),
  Time_constant_current_sd = sd(posterior_samples$beta[,3]),
  Time_constant_current_2.5 = quantile(posterior_samples$beta[,3], 0.025),
  Time_constant_current_97.5 = quantile(posterior_samples$beta[,3], 0.975)
)

# Print the coefficient summary
print(coef_summary)

# Generate predictions using the posterior samples
predictions <- sapply(1:nrow(posterior_samples$beta), function(i) {
  posterior_samples$beta[i, 1] * df_filtered$Discharge_Time +
    posterior_samples$beta[i, 2] * df_filtered$Decrement_3_6_3_4V +
    posterior_samples$beta[i, 3] * df_filtered$Time_constant_current
})

# Calculate the mean of the predictions
predictions_mean <- rowMeans(predictions)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions_mean)

p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Additional plots with regression lines
p2 <- ggplot(df_filtered, aes(x = Decrement_3_6_3_4V, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Decrement 3.6-3.4V", x = "Decrement 3.6-3.4V", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p3 <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Discharge Time", x = "Discharge Time", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p4 <- ggplot(df_filtered, aes(x = Time_constant_current, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Time Constant Current", x = "Time Constant Current", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Arrange all plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Create summary tables for each plot
summary_stats <- function(data, predictions) {
  data_summary <- data.frame(
    Metric = c("Min", "Q1", "Median", "Q3", "Max"),
    Value = c(min(data), quantile(data, 0.25), median(data), quantile(data, 0.75), max(data))
  )
  
  error_summary <- data.frame(
    Metric = c("Standard Error", "Likelihood"),
    Value = c(sd(predictions), exp(-sum((data - predictions)^2) / (2 * length(data))))
  )
  
  aic_bic_waic_summary <- data.frame(
    Metric = c("AIC", "BIC"),
    Value = c(AIC(model), BIC(model))
  )
  
  return(list(data_summary = data_summary, error_summary = error_summary, aic_bic_waic_summary = aic_bic_waic_summary))
}

# Generate summaries for each plot
summary_p1 <- summary_stats(plot_data$Actual, predictions_mean)
summary_p2 <- summary_stats(df_filtered$Decrement_3_6_3_4V, predictions_mean)
summary_p3 <- summary_stats(df_filtered$Discharge_Time, predictions_mean)
summary_p4 <- summary_stats(df_filtered$Time_constant_current, predictions_mean)

# Print summaries for each plot
print("Summary for Actual vs Predicted Values:")
print(summary_p1$data_summary)
print(summary_p1$error_summary)
print(summary_p1$aic_bic_waic_summary)

print("Summary for Max Voltage Discharge vs Decrement 3.6-3.4V:")
print(summary_p2$data_summary)
print(summary_p2$error_summary)
print(summary_p2$aic_bic_waic_summary)

print("Summary for Max Voltage Discharge vs Discharge Time:")
print(summary_p3$data_summary)
print(summary_p3$error_summary)
print(summary_p3$aic_bic_waic_summary)

print("Summary for Max Voltage Discharge vs Time Constant Current:")
print(summary_p4$data_summary)
print(summary_p4$error_summary)
print(summary_p4$aic_bic_waic_summary)