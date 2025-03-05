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

# Fit Log-Normal distribution
fit_lognormal <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")

# Fit Weibull distribution
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")

# Function to create a summary table for each model
create_model_summary <- function(model, model_name) {
  if (model_name == "Normal") {
    quantiles <- quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 0.95, 1))
    sd_error <- sd(df_filtered$Max_Voltage_Dischar)
    error <- sd_error / sqrt(length(df_filtered$Max_Voltage_Dischar))  # Standard Error of the mean
    aic <- AIC(fit_normal)
    bic <- BIC(fit_normal)
    waic <- calculate_waic(logLik(fit_normal), length(df_filtered$Max_Voltage_Dischar), length(coef(fit_normal)))
  } else if (model_name == "Log-Normal") {
    quantiles <- qlnorm(c(0, 0.25, 0.5, 0.75, 0.95, 1), meanlog = model$estimate[1], sdlog = model$estimate[2])
    sd_error <- sd(df_filtered$Max_Voltage_Dischar)
    error <- sd_error / sqrt(length(df_filtered$Max_Voltage_Dischar))
    aic <- AIC(fit_lognormal)
    bic <- BIC(fit_lognormal)
    waic <- calculate_waic(logLik(fit_lognormal), length(df_filtered$Max_Voltage_Dischar), length(coef(fit_lognormal)))
  } else if (model_name == "Weibull") {
    quantiles <- qweibull(c(0, 0.25, 0.5, 0.75, 0.95, 1), shape = model$estimate[1], scale = model$estimate[2])
    sd_error <- sd(df_filtered$Max_Voltage_Dischar)
    error <- sd_error / sqrt(length(df_filtered$Max_Voltage_Dischar))
    aic <- AIC(fit_weibull)
    bic <- BIC(fit_weibull)
    waic <- calculate_waic(logLik(fit_weibull), length(df_filtered$Max_Voltage_Dischar), length(coef(fit_weibull)))
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
summary_lognormal <- create_model_summary(fit_lognormal, "Log-Normal")
summary_weibull <- create_model_summary(fit_weibull, "Weibull")

# Combine all summaries into one data frame
final_summary <- rbind(summary_normal, summary_lognormal, summary_weibull)

# Create individual plots for the second set (SD_Error, Error, AIC, BIC, WAIC)
plot_sd_error <- ggplot(final_summary, aes(x = Model, y = SD_Error)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(SD_Error, 2)), vjust = -1) +  # Add labels
  labs(title = "Standard Deviation Error for Each Model", y = "SD Error") +
  ylim(0, 0.25) +
  theme_minimal()

plot_error <- ggplot(final_summary, aes(x = Model, y = Error)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(Error, 2)), vjust = -1) +  # Add labels
  labs(title = "Standard Error for Each Model", y = "Standard Error") +
  ylim(-0.2, 0.3) +
  theme_minimal()

plot_aic <- ggplot(final_summary, aes(x = Model, y = AIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(AIC, 2)), vjust = -1) +  # Add labels
  labs(title = "AIC for Each Model", y = "AIC") +
  ylim(-36000, -31000) +
  theme_minimal()

plot_bic <- ggplot(final_summary, aes(x = Model, y = BIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(BIC, 2)), vjust = -1) +  # Add labels
  labs(title = "BIC for Each Model", y = "BIC") +
  ylim(-36000, -31000) +
  theme_minimal()

plot_waic <- ggplot(final_summary, aes(x = Model, y = WAIC)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  geom_text(aes(label = round(WAIC, 2)), vjust = -1) +  # Add labels
  ylim(-36000, -31000) +
  labs(title = "WAIC for Each Model", y = "WAIC") +
  theme_minimal()

# Arrange the second set of plots in a grid
grid.arrange(plot_sd_error, plot_error, plot_aic, plot_bic, plot_waic, nrow = 5)