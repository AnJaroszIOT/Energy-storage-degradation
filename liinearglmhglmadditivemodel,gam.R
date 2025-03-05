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
Normal <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
Weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
Gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
Cauchy <- fitdist(df_filtered$Max_Voltage_Dischar, "cauchy")
Lognormal <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")
Logarit <- fitdist(log(df_filtered$Max_Voltage_Dischar), "norm")

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
  if (class(model) %in% c("fitdistr", "fitdist")) {
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
    Model = deparse(substitute(model)),
    AIC = aic_value,
    BIC = bic_value,
    WAIC = waic_value,
    LogLikelihood = as.numeric(log_likelihood),
    Intercept = intercept,
    SD_Error = sd_error,
    Error = sd_error,  # Assuming Error is the same as SD_Error for this context
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
  calculate_model_metrics(fit_logarithmic)  # Include logarithmic model
)

# Display the summary table
print(model_summaries)

# Create a point plot for each metric in separate rows
metrics_to_plot <- c("AIC", "BIC", "WAIC", "LogLikelihood", "SD_Error", "Error")

# Create a list to hold plots
plots <- lapply(metrics_to_plot, function(metric) {
  ggplot(model_summaries, aes_string(x = "Model", y = metric)) +
    geom_point(size = 4, color = "blue") +
    geom_text(aes(label = round(get(metric), 2)), vjust = -0.5, size = 4) +
    labs(title = paste(metric, "for Each Model"), x = "Model", y = metric) +
    theme_minimal()
})

# Arrange plots in a grid with 1 column and 6 rows
print(grid.arrange(grobs = plots, nrow = 6))