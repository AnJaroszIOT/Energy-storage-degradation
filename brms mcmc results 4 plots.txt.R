# Load necessary libraries
library(brms)  # For Bayesian regression using MCMC
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
df_filtered <- df %>% 
  filter(Max_Voltage_Dischar >= 3.5, 
         Discharge_Time > 0, 
         Decrement_3.6_3.4V > 0, 
         Time_constant_current > 0)

# Fit the Bayesian model using MCMC
mcmc_model <- brm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                  data = df_filtered, 
                  family = gaussian(),
                  prior = c(set_prior("normal(0, 1)", class = "b")),  # Example of an informative prior
                  iter = 4000,  # Number of iterations
                  chains = 4,   # Number of chains
                  control = list(max_treedepth = 15),  # Increase max_treedepth
                  seed = 123)

# Print the model summary
print(summary(mcmc_model))

# Generate predictions
predictions <- predict(mcmc_model, newdata = df_filtered)

# Calculate summary statistics for Max Voltage Discharge
data_summary <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Calculate error summary
errors <- predictions[, "Estimate"] - df_filtered$Max_Voltage_Dischar
error_summary <- data.frame(
  Metric = c("Mean Error", "Standard Deviation of Error"),
  Value = c(mean(errors), sd(errors))
)

# Calculate WAIC
waic_result <- waic(mcmc_model)

# Summaries for AIC and BIC (using the model's log-likelihood)
aic <- AIC(mcmc_model)
bic <- BIC(mcmc_model)

# Summaries for AIC, BIC, and WAIC
aic_bic_waic_summary <- data.frame(
  Metric = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, waic_result$waic)
)

# Print summaries
print("Data Summary:")
print(data_summary)

print("Error Summary:")
print(error_summary)

print("AIC, BIC, WAIC Summary:")
print(aic_bic_waic_summary)

# Summaries for plots p1, p2, p3, and p4
summary_p1 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(predictions[, "Estimate"], probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p2 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Decrement_3.6_3.4V, probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p3 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Discharge_Time, probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p4 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Time_constant_current, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Print summaries for plots
print("Summary for Plot 1 (Predictions):")
print(summary_p1)

print("Summary for Plot 2 (Decrement vs Max Voltage Discharge):")
print(summary_p2)

print("Summary for Plot 3 (Discharge Time vs Max Voltage Discharge):")
print(summary_p3)

print("Summary for Plot 4 (Time Constant Current vs Max Voltage Discharge):")
print(summary_p4)