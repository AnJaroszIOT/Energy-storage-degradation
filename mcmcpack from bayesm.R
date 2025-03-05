# Load necessary libraries
library(MCMCpack)  # For MCMC modeling
library(MASS)      # For mvrnorm function
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(knitr)     # For table formatting

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

# Prepare data for MCMC modeling
y <- df_filtered$Max_Voltage_Dischar
X <- as.matrix(df_filtered[, c("Discharge_Time", "Decrement_3.6_3.4V", "Time_constant_current")])

# Set up the model for MCMC
n <- nrow(X)
p <- ncol(X)

# Run MCMC regression using MCMCpack
mcmc_results <- MCMCregress(y ~ X - 1, 
                            b0 = rep(0, p), 
                            B0 = diag(1, p), 
                            nburn = 200, 
                            niter = 1000)

# Extract posterior means
beta_mean <- colMeans(mcmc_results[, 1:p])  # Extract the coefficients
sigma2_mean <- mean(mcmc_results[, "sigma2"])

# Generate predictions
predictions <- X %*% beta_mean  # Ensure beta_mean is of compatible dimensions


# Function to calculate metrics
calculate_metrics <- function(y, predictions, n, p) {
  # Calculate log-likelihood
  log_likelihood <- sum(dnorm(y, mean = predictions, sd = sqrt(sigma2_mean), log = TRUE))
  
  # Calculate likelihood
  likelihood <- exp(log_likelihood)
  
  # Calculate AIC and BIC
  k <- p + 1  # Number of parameters (including sigma^2)
  AIC <- -2 * log_likelihood + 2 * k
  BIC <- -2 * log_likelihood + log(n) * k
  
  # Calculate errors
  errors <- predictions - y
  mean_error <- mean(errors)
  sd_error <- sd(errors)
  
  # Calculate WAIC
  pointwise_log_likelihood <- dnorm(y, mean = predictions, sd = sqrt(sigma2_mean), log = TRUE)
  waic <- -2 * (sum(pointwise_log_likelihood) - sum(exp(pointwise_log_likelihood)))
  
  # Create a summary table
  metrics_table <- data.frame(
    Metric = c("WAIC", "AIC", "BIC", "Mean Error", "SD Error", "Log-Likelihood", "Likelihood"),
    Value = c(waic, AIC, BIC, mean_error, sd_error, log_likelihood, likelihood)
  )
  
  return(metrics_table)
}

#Calculate intercepts for each plot
intercept1 <- beta_mean[1] # Intercept for Actual vs Predicted 
intercept2 <- coef(lm(Max_Voltage_Dischar ~ Decrement_3.6_3.4V, data = df_filtered))[1] # Intercept for plot 2 
intercept3 <- coef(lm(Max_Voltage_Dischar ~ Discharge_Time, data = df_filtered))[1] # Intercept for plot 3 
intercept4 <- coef(lm(Max_Voltage_Dischar ~ Time_constant_current, data = df_filtered))[1] # Intercept for plot 4

#Print intercepts
cat("\nIntercepts for the plots:\n") 
cat("Intercept for Actual vs Predicted:", intercept1, "\n") 
cat("Intercept for Max Voltage Discharge vs Decrement 3.6-3.4V:", intercept2, "\n") 
cat("Intercept for Max Voltage Discharge vs Discharge Time:", intercept3, "\n") 
cat("Intercept for Max Voltage Discharge vs Time Constant Current:", intercept4, "\n")

# Calculate metrics for the first plot (Actual vs Predicted)
metrics_plot1 <- calculate_metrics(df_filtered$Max_Voltage_Dischar, predictions, n, p)

# Print metrics for the first plot
cat("Metrics for Actual vs Predicted Values:\n")
print(knitr::kable(metrics_plot1))

# Create plot 1: Actual vs Predicted
p1 <- ggplot(data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions), 
             aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", 
       x = "Actual Max Voltage Discharge", 
       y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10)) 

# Calculate quantities for plot 1
voltage_quantities_plot1 <- quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
quantities_table_plot1 <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
  Value = voltage_quantities_plot1
)

# Additional plot 2: Max Voltage Discharge vs Decrement 3.6-3.4V
p2 <- ggplot(df_filtered, aes(x = Decrement_3.6_3.4V, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Decrement 3.6-3.4V", x = "Decrement 3.6-3.4V", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate quantities for plot 2
decrement_quantities_plot2 <- quantile(df_filtered$Decrement_3.6_3.4V, probs = c(0, 0.25, 0.5, 0.75, 1))
quantities_table_plot2 <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
  Value = decrement_quantities_plot2
)

# Calculate metrics for the second plot
metrics_plot2 <- calculate_metrics(df_filtered$Max_Voltage_Dischar, predictions, n, p)

# Print metrics for the second plot
cat("\nMetrics for Max Voltage Discharge vs Decrement 3.6-3.4V:\n")
print(knitr::kable(metrics_plot2))

# Additional plot 3: Max Voltage Discharge vs Discharge Time
p3 <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Discharge Time", x = "Discharge Time", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate quantities for plot 3
discharge_time_quantities_plot3 <- quantile(df_filtered$Discharge_Time, probs = c(0, 0.25, 0.5, 0.75, 1))
quantities_table_plot3 <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
  Value = discharge_time_quantities_plot3
)

# Calculate metrics for the third plot
metrics_plot3 <- calculate_metrics(df_filtered$Max_Voltage_Dischar, predictions, n, p)

# Print metrics for the third plot
cat("\nMetrics for Max Voltage Discharge vs Discharge Time:\n")
print(knitr::kable(metrics_plot3))

# Additional plot 4: Max Voltage Discharge vs Time Constant Current
p4 <- ggplot(df_filtered, aes(x = Time_constant_current, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Time Constant Current", x = "Time Constant Current", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate quantities for plot 4
time_constant_quantities_plot4 <- quantile(df_filtered$Time_constant_current, probs = c(0, 0.25, 0.5, 0.75, 1))
quantities_table_plot4 <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
  Value = time_constant_quantities_plot4
)

# Calculate metrics for the fourth plot
metrics_plot4 <- calculate_metrics(df_filtered$Max_Voltage_Dischar, predictions, n, p)

# Print metrics for the fourth plot
cat("\nMetrics for Max Voltage Discharge vs Time Constant Current:\n")
print(knitr::kable(metrics_plot4))

# Arrange all plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Print quantities tables for each plot
cat("\nQuantities for Plot 1 (Actual vs Predicted):\n")
print(knitr::kable(quantities_table_plot1))

cat("\nQuantities for Plot 2 (Max Voltage Discharge vs Decrement 3.6-3.4V):\n")
print(knitr::kable(quantities_table_plot2))

cat("\nQuantities for Plot 3 (Max Voltage Discharge vs Discharge Time):\n")
print(knitr::kable(quantities_table_plot3))

cat("\nQuantities for Plot 4 (Max Voltage Discharge vs Time Constant Current):\n")
print(knitr::kable(quantities_table_plot4))