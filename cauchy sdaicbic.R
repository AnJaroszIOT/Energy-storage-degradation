# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(gridExtra)  # For arranging plots
library(fitdistrplus)  # For fitting distributions

# Function to calculate WAIC
calculate_waic <- function(log_likelihood) {
  elpd_waic <- sum(log_likelihood)  # Expected log pointwise predictive density
  p_waic <- var(log_likelihood)  # Effective number of parameters
  waic <- -2 * (elpd_waic - p_waic)  # WAIC calculation
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
fit_cauchy <- fitdistr(df_filtered$Max_Voltage_Dischar, "cauchy")
fit_exponential <- fitdistr(df_filtered$Max_Voltage_Dischar, "exponential")
fit_gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")

# Function to create a summary table for each model
create_model_summary <- function(model, model_name) {
  log_likelihood <- logLik(model)
  quantiles <- NULL
  sd_error <- sd(df_filtered$Max_Voltage_Dischar)
  error <- sd_error / sqrt(length(df_filtered$Max_Voltage_Dischar))  # Standard Error of the mean
  aic <- AIC(model)
  bic <- BIC(model)
  waic <- calculate_waic(logLik(model))  # Calculate WAIC
  
  if (model_name == "Normal") {
    quantiles <- quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 0.95, 1))
  } else if (model_name == "Cauchy") {
    quantiles <- qcauchy(c(0, 0.25, 0.5, 0.75, 0.95, 1), location = model$estimate[1], scale = model$estimate[2])
  } else if (model_name == "Exponential") {
    quantiles <- qexp(c(0, 0.25, 0.5, 0.75, 0.95, 1), rate = 1/mean(df_filtered$Max_Voltage_Dischar))
  } else if (model_name == "Gamma") {
    quantiles <- qgamma(c(0, 0.25, 0.5, 0.75, 0.95, 1), shape = model$estimate[1], rate = model$estimate[2])
  } else if (model_name == "Weibull") {
    quantiles <- qweibull(c(0, 0.25, 0.5, 0.75, 0.95, 1), shape = model$estimate[1], scale = model$estimate[2])
  }
  
  summary_df <- data.frame(
    Model = model_name,
    Min = quantiles[1],
    Q3 = quantiles[4],
    Median = quantiles[3],
    Q5 = quantiles[5],
    Max = quantiles[6],
    SD_Error = sd_error,
    Error = error,
    AIC = aic,
    BIC = bic,
    WAIC = waic
  )
  
  return(summary_df)
}

# Create summary tables for each model
summary_normal <- create_model_summary(fit_normal, "Normal")
summary_cauchy <- create_model_summary(fit_cauchy, "Cauchy")
summary_exponential <- create_model_summary(fit_exponential, "Exponential")
summary_gamma <- create_model_summary(fit_gamma, "Gamma")
summary_weibull <- create_model_summary(fit_weibull, "Weibull")

# Combine all summaries into one data frame
final_summary <- rbind(summary_normal, summary_cauchy, summary_exponential, summary_gamma, summary_weibull)

# Create a summary table for AIC, BIC, and WAIC
criteria_summary <- data.frame(
  Metric = c("AIC", "BIC", "WAIC"),
  Normal = c(summary_normal$AIC, summary_normal$BIC, summary_normal$WAIC),
  Cauchy = c(summary_cauchy$AIC, summary_cauchy$BIC, summary_cauchy$WAIC),
  Exponential = c(summary_exponential$AIC, summary_exponential$BIC, summary_exponential$WAIC),
  Gamma = c(summary_gamma$AIC, summary_gamma$BIC, summary_gamma$WAIC),
  Weibull = c(summary_weibull$AIC, summary_weibull$BIC, summary_weibull$WAIC)
)

# Print the criteria summary table
print(criteria_summary)

# Create individual plots for each metric
plot_sd_error <- ggplot(final_summary, aes(x = Model, y = SD_Error)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(SD_Error, 2)), vjust = -1) +  # Add labels
  labs(title = "Standard Deviation Error for Each Model", y = "SD Error") +
  theme_minimal()

plot_error <- ggplot(final_summary, aes(x = Model, y = Error)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(Error, 2)), vjust = -1) +  # Add labels
  labs(title = "Standard Error for Each Model", y = "Standard Error") +
  theme_minimal()

plot_aic <- ggplot(final_summary, aes(x = Model, y = AIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(AIC, 2)), vjust = -1) +  # Add labels
  labs(title = "AIC for Each Model", y = "AIC") +
  ylim(-36000, 240000) +  # Set y-axis limits
  theme_minimal()

plot_bic <- ggplot(final_summary, aes(x = Model, y = BIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(BIC, 2)), vjust = -1) +  # Add labels
  labs(title = "BIC for Each Model", y = "BIC") +
  ylim(-36000, 240000) +  # Set y-axis limits
  theme_minimal()

plot_waic <- ggplot(final_summary, aes(x = Model, y = WAIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(WAIC, 2)), vjust = -1) +  # Add labels
  labs(title = "WAIC for Each Model", y = "WAIC") +
  ylim(-36000, 240000) +  # Set y-axis limits
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot_sd_error, plot_error, plot_aic, plot_bic, plot_waic, nrow = 5)

