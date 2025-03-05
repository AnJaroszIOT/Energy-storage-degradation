# Load necessary libraries
library(MCMCpack)      # For Bayesian regression using MCMCpack
library(dplyr)         # For data manipulation
library(readxl)        # For reading Excel files
library(ggplot2)       # For plotting
library(gridExtra)     # For arranging plots

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
  filter(Max_Voltage_Dischar >= 3.5, Discharge_Time > 0, Decrement_3.6_3.4V > 0, Time_constant_current > 0)

# Check if the filtered data is empty
if (nrow(df_filtered) == 0) {
  stop("Filtered data is empty. Please check your filtering criteria.")
}

# Fit the Bayesian model using MCMCpack
mcmc_model <- MCMCregress(
  Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
  data = df_filtered,
  mcmc = 4000,  # Number of MCMC iterations
  burnin = 1000,  # Number of burn-in iterations
  thin = 10  # Thinning factor
)

# Print the model summary
print(summary(mcmc_model))

# Generate predictions for the filtered data
# Create a design matrix for predictions
X <- model.matrix(~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, data = df_filtered)

# Check dimensions of X and mcmc_model
cat("Dimensions of design matrix X:", dim(X), "\n")
cat("Dimensions of MCMC model:", dim(mcmc_model), "\n")

# Ensure that the number of columns in X matches the number of coefficients in mcmc_model
num_coefficients <- ncol(mcmc_model)

if (ncol(X) != num_coefficients) {
  stop("The number of columns in the design matrix X does not match the number of coefficients in the MCMC model.")
}

# Print the design matrix to inspect its structure
print("Design matrix X:")
print(head(X))

# Calculate predictions by multiplying the design matrix with the posterior samples
predictions <- X %*% t(mcmc_model)

# Take the mean of the predictions across the posterior samples
predictions_mean <- rowMeans(predictions)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions_mean)

# Calculate summary statistics for Max Voltage Discharge
data_summary <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Calculate error summary
errors <- predictions_mean - df_filtered$Max_Voltage_Dischar
error_summary <- data.frame(
  Metric = c("Mean Error", "Standard Deviation of Error"),
  Value = c(mean(errors), sd(errors))
)

# Calculate AIC and BIC (using residuals)
n <- nrow(df_filtered)
rss <- sum((errors)^2)  # Residual sum of squares
aic <- n * log(rss/n) + 2 * length(coef(mcmc_model))  # AIC approximation
bic <- n * log(rss/n) + log(n) * length(coef(mcmc_model))  # BIC approximation

# Summaries for AIC, BIC, and WAIC (WAIC is not applicable here)
aic_bic_waic_summary <- data.frame(
  Metric = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, NA)  # WAIC is not applicable for this model
)

# Print summaries
print("Data Summary:")
print(data_summary)
print("Error Summary:")
print(error_summary)
print("AIC, BIC, WAIC Summary:")
print(aic_bic_waic_summary)

# Create plots
p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

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
print(grid.arrange(p1, p2, p3, p4, ncol = 2))