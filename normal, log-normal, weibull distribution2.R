# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(fitdistrplus)  # For fitting distributions

# Function to calculate WAIC
calculate_waic <- function(log_likelihood, n, k) {
  # Calculate WAIC
  waic <- -2 * (log_likelihood - k)
  return(waic)
}

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

# Fit Normal distribution
fit_normal <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
log_likelihood_normal <- logLik(fit_normal)
n_normal <- nrow(df_filtered)
k_normal <- length(fit_normal$estimate)

# Transform the data for Log-Normal fitting (e.g., scaling down)
scaled_data <- df_filtered$Max_Voltage_Dischar / 10  # Scale down the data

# Fit Log-Normal distribution on the scaled data
fit_lognormal <- fitdist(scaled_data, "lnorm")
log_likelihood_lognormal <- logLik(fit_lognormal)
n_lognormal <- nrow(df_filtered)
k_lognormal <- length(fit_lognormal$estimate)

# Fit Weibull distribution
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
log_likelihood_weibull <- logLik(fit_weibull)
n_weibull <- nrow(df_filtered)
k_weibull <- length(fit_weibull$estimate)

# Create a summary table for the models
summary_table <- data.frame(
  Model = c("Normal", "Log-Normal", "Weibull"),
  AIC = c(AIC(fit_normal), AIC(fit_lognormal), AIC(fit_weibull)),
  BIC = c(BIC(fit_normal), BIC(fit_lognormal), BIC(fit_weibull)),
  WAIC = c(calculate_waic(log_likelihood_normal, n_normal, k_normal),
           calculate_waic(log_likelihood_lognormal, n_lognormal, k_lognormal),
           calculate_waic(log_likelihood_weibull, n_weibull, k_weibull)),
  SD_Error = c(sqrt(fit_normal$sd[1]^2), 
               sqrt(fit_lognormal$sd[1]^2), 
               sqrt(fit_weibull$sd[1]^2)),
  LogLikelihood = c(log_likelihood_normal, log_likelihood_lognormal, log_likelihood_weibull)
)

# Display the summary table
print(summary_table)

# Calculate quantiles for each distribution
quantiles_normal <- qnorm(c(0.25, 0.5, 0.75), mean = fit_normal$estimate[1], sd = fit_normal$estimate[2])
quantiles_lognormal <- qlnorm(c(0.25, 0.5, 0.75), meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]) * 10  # Scale back to original
quantiles_weibull <- qweibull(c(0.25, 0.5, 0.75), shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2])

# Create a separate quantile table
quantile_table <- data.frame(
  Distribution = c("Normal", "Log-Normal", "Weibull"),
  Quantile_25 = c(quantiles_normal[1], quantiles_lognormal[1], quantiles_weibull[1]),
  Quantile_50 = c(quantiles_normal[2], quantiles_lognormal[2], quantiles_weibull[2]),
  Quantile_75 = c(quantiles_normal[3], quantiles_lognormal[3], quantiles_weibull[3])
)

# Display the quantile table
print(quantile_table)

# Create plots for each distribution with thicker lines
plot_normal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dnorm, args = list(mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]), color = "blue", size = 1.5) +  # Thicker line
  labs(title = "Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_lognormal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dlnorm, args = list(meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), color = "red", size = 1.5) +  # Thicker line
  labs(title = "Log-Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_weibull <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dweibull, args = list(shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), color = "green", size = 1.5) +  # Thicker line
  labs(title = "Weibull Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

# Arrange plots in a grid
grid.arrange(plot_normal, plot_lognormal, plot_weibull, nrow = 3)