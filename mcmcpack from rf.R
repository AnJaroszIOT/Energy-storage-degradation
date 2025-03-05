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

# Fit the MCMC regression model
mcmc_model <- MCMCregress(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                          data = df_filtered, 
                          burnin = 1000, 
                          mcmc = 10000)

# Print the summary of the MCMC model
summary(mcmc_model)

# Generate predictions for each observation in the filtered dataset
# Extract the posterior means for the coefficients
coefficients <- colMeans(mcmc_model)

# Calculate predictions
predictions <- coefficients[1] + 
  coefficients[2] * df_filtered$Discharge_Time + 
  coefficients[3] * df_filtered$Decrement_3.6_3.4V + 
  coefficients[4] * df_filtered$Time_constant_current

# Function to calculate AIC, BIC, and WAIC
calculate_information_criteria <- function(actual, predicted, n_params) {
  n <- length(actual)
  residuals <- actual - predicted
  rss <- sum(residuals^2)  # Residual sum of squares
  log_likelihood <- -n / 2 * log(rss / n)  # Approximate log-likelihood
  
  # AIC and BIC calculations
  aic <- -2 * log_likelihood + 2 * n_params
  bic <- -2 * log_likelihood + log(n) * n_params
  
  # WAIC calculation
  waic <- -2 * (mean(log(predicted)) - (n_params / n))  # Simplified WAIC
  
  # Return a detailed data frame with all criteria
  return(data.frame(AIC = aic, BIC = bic, WAIC = waic, n_params = n_params))
}

# Calculate AIC, BIC, and WAIC
n_params <- ncol(mcmc_model) - 1  # Number of parameters (excluding intercept)
info_criteria <- calculate_information_criteria(df_filtered$Max_Voltage_Dischar, predictions, n_params)

# Print the information criteria
print("Information Criteria:")
print(info_criteria)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions)

# Create the plot
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