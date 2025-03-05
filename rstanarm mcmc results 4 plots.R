library(rstanarm)  # For Bayesian regression
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(bayesplot)  # For MCMC diagnostics

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
df_filtered <- df %>% 
  filter(Max_Voltage_Dischar >= 3.5, 
         Discharge_Time > 0, 
         Decrement_3.6_3.4V > 0, 
         Time_constant_current > 0)

# Fit the Bayesian regression model using rstanarm
bayesian_model <- stan_glm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                           data = df_filtered,
                           family = gaussian(),
                           prior = normal(0, 10),  # Set a normal prior
                           chains = 4,              # Number of chains
                           iter = 2000,             # Number of iterations
                           warmup = 1000)           # Warmup iterations

# Generate predictions
predictions <- predict(bayesian_model, newdata = df_filtered)

# Calculate summary statistics for Max Voltage Discharge
data_summary <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Calculate error summary
errors <- predictions - df_filtered$Max_Voltage_Dischar
error_summary <- data.frame(
  Metric = c("Mean Error", "Standard Deviation of Error"),
  Value = c(mean(errors), sd(errors))
)

# Calculate AIC and BIC
n <- nrow(df_filtered)
log_likelihood <- sum(dnorm(df_filtered$Max_Voltage_Dischar, mean = predictions, sd = sd(errors), log = TRUE))
aic <- -2 * log_likelihood + 2 * length(bayesian_model$coefficients)  # AIC approximation
bic <- -2 * log_likelihood + log(n) * length(bayesian_model$coefficients)  # BIC approximation

# Calculate WAIC
waic_value <- waic(bayesian_model)

# Summaries for AIC, BIC, and WAIC
aic_bic_waic_summary <- data.frame(
  Metric = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, waic_value$waic)
)

# Print summaries
print("Data Summary:")
print(data_summary)

print("Error Summary:")
print(error_summary)

print("AIC, BIC, WAIC Summary:")
print(aic_bic_waic_summary)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions)

# Calculate quantiles for predictions
prediction_summary <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(predictions, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Print summary for predictions
print("Summary for Predictions:")
print(prediction_summary)

p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Additional plots with regression lines
p2 <- ggplot(df_filtered, aes(x = Decrement_3.6_3.4V, y = Max_Voltage_Dischar)) +
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

# R-hat statistics
rhat_values <- rhat(bayesian_model)
print("R-hat values:")
print(rhat_values)

# Effective sample size
ess_values <- ess_bulk(bayesian_model)
print("Effective Sample Size:")
print(ess_values)