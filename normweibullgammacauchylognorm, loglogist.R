# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(fitdistrplus)  # For fitting distributions
library(tidyr)  # For pivot_longer function

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

# Fit distributions
fit_normal <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
fit_gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
fit_cauchy <- fitdist(df_filtered$Max_Voltage_Dischar, "cauchy")
fit_lognormal <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")
fit_loglogistic <- fitdist(df_filtered$Max_Voltage_Dischar, "logis")

# Function to calculate AIC, BIC, and WAIC
calculate_model_metrics <- function(model) {
  # Extract log-likelihood
  log_likelihood <- logLik(model)
  
  # Calculate AIC and BIC
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  
  # Calculate WAIC
  waic_value <- calculate_waic(log_likelihood, length(df_filtered$Max_Voltage_Dischar), length(coef(model)))
  
  # Extract model parameters
  if (class(model) == "fitdistr") {
    params <- data.frame(
      Estimate = model$estimate,
      StdError = sqrt(diag(model$vcov)),
      row.names = names(model$estimate)
    )
    intercept <- params[1, "Estimate"]
    sd_error <- params[2, "StdError"]
  } else {
    intercept <- NA
    sd_error <- NA
  }
  
  # Create a summary data frame
  summary_df <- data.frame(
    Model = class(model)[1],
    AIC = aic_value,
    BIC = bic_value,
    WAIC = waic_value,
    LogLikelihood = as.numeric(log_likelihood),
    Intercept = intercept,
    SD_Error = sd_error,
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

# Calculate metrics for each model
model_summaries <- bind_rows(
  calculate_model_metrics(fit_normal),
  calculate_model_metrics(fit_weibull),
  calculate_model_metrics(fit_gamma),
  calculate_model_metrics(fit_cauchy),
  calculate_model_metrics(fit_lognormal),
  calculate_model_metrics(fit_loglogistic)
  )

# Display the summary table
print(model_summaries)

# Create a point plot for AIC, BIC, WAIC, SD Error, and Log-Likelihood
point_plot <- model_summaries %>%
  pivot_longer(cols = c(AIC, BIC, WAIC, SD_Error, LogLikelihood), 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = Model, y = Value, color = Metric, label = round(Value, 2))) +
  geom_point(size = 4) +
  geom_text(vjust = -0.5, size = 4) +
  labs(title = "Model Comparison Metrics", x = "Model", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("AIC" = "red", "BIC" = "green", "WAIC" = "blue", "SD_Error" = "purple", "LogLikelihood" = "orange"))

# Display the point plot
print(point_plot)

# Create plots for each distribution
plot_normal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dnorm, args = list(mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_weibull <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dweibull, args = list(shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Weibull Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_gamma <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Gamma Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_cauchy <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dcauchy, args = list(location = fit_cauchy$estimate[1], scale = fit_cauchy$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Cauchy Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

plot_lognormal <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = dlnorm, args = list(meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Log-Normal Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

# Create a plot for the logarithmic distribution
plot_loglogistic <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  stat_function(fun = function(x) dlnorm(x, meanlog = fit_loglogistic$estimate[1], sdlog = fit_lognormal$estimate[2]), color = "blue", size = 1.5) +
  labs(title = "Loglogistic Distribution Fit", x = "Max Voltage Discharge (V)", y = "Density")

# Arrange plots in a grid with 2 columns and 3 rows
print(grid.arrange(plot_normal, plot_weibull, 
                   plot_gamma, plot_cauchy, 
                   plot_lognormal, plot_loglogistic, 
                   nrow = 3, ncol = 2))