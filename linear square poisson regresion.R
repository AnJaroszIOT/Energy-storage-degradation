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

# Fit Log-Normal distribution
fit_lognormal <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")
log_likelihood_lognormal <- logLik(fit_lognormal)
n_lognormal <- nrow(df_filtered)
k_lognormal <- length(fit_lognormal$estimate)

# Fit Weibull distribution
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
log_likelihood_weibull <- logLik(fit_weibull)
n_weibull <- nrow(df_filtered)
k_weibull <- length(fit_weibull$estimate)

# Function to create a summary table for each model
create_model_summary <- function(model, model_name) {
  if (model_name == "Normal") {
    quantiles <- qnorm(c(0.25, 0.5, 0.75), mean = model$estimate[1], sd = model$estimate[2])
    sd_error <- model$sd[1]
  } else if (model_name == "Log-Normal") {
    quantiles <- qlnorm(c(0.25, 0.5, 0.75), meanlog = model$estimate[1], sdlog = model$estimate[2])
    sd_error <- model$sd[1]
  } else if (model_name == "Weibull") {
    quantiles <- qweibull(c(0.25, 0.5, 0.75), shape = model$estimate[1], scale = model$estimate[2])
    sd_error <- model$sd[1]
  }
  
  summary_df <- data.frame(
    Model = model_name,
    Quantile_25 = quantiles[1],
    Quantile_50 = quantiles[2],
    Quantile_75 = quantiles[3],
    SD_Error = sd_error,
    LogLikelihood = as.numeric(logLik(model)),
    AIC = AIC(model),
    BIC = BIC(model),
    WAIC = calculate_waic(as.numeric(logLik(model)), nrow(df_filtered), length(model$estimate))
  )
  
  return(summary_df)
}

# Create summary tables for each model
summary_normal <- create_model_summary(fit_normal, "Normal")
summary_lognormal <- create_model_summary(fit_lognormal, "Log-Normal")
summary_weibull <- create_model_summary(fit_weibull, "Weibull")

# Combine all summaries into one data frame
final_summary <- rbind(summary_normal, summary_lognormal, summary_weibull)

# Display the summary tables for each model
print("Summary Table for Normal Distribution:")
print(summary_normal)

print("Summary Table for Log-Normal Distribution:")
print(summary_lognormal)

print("Summary Table for Weibull Distribution:")
print(summary_weibull)

# Display combined summary
print("Combined Summary Table:")
print(final_summary)

# Create plots for each distribution with thicker lines
plot_normal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dnorm, args = list(mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_lognormal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dlnorm, args = list(meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Log-Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_weibull <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dweibull, args = list(shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Weibull Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

# Arrange plots in a grid
grid.arrange(plot_normal, plot_lognormal, plot_weibull, nrow = 3, ncol = 1)