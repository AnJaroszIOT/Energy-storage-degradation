


# Load necessary libraries
library(mgcv)  # For Generalized Additive Mixed Models
library(knitr)  # For table formatting
library(loo)  # For WAIC calculation
library(readxl)  # For reading Excel files
library(dplyr)  # For data manipulation

# Set CPU and elapsed time limits to Inf
options(timeout = 600)  # Set timeout to 10 minutes
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

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

# Check if a grouping variable exists; if not, create a dummy variable
if (!"Group" %in% names(df_filtered)) {
  df_filtered$Group <- as.factor(sample(1:3, nrow(df_filtered), replace = TRUE))  # Create a dummy grouping variable
}

# Fit the GAMM model
gamm_model <- gamm(Max_Voltage_Dischar ~ s(Discharge_Time) + s(Decrement_3.6_3.4V) + s(Time_constant_current), 
                   random = list(Group = ~1), data = df_filtered)

# Print model summary
print(summary(gamm_model$gam))

# Calculate AIC, BIC, and WAIC
aic_value <- AIC(gamm_model$gam)
bic_value <- BIC(gamm_model$gam)

# Check if log-likelihood is valid
log_lik <- logLik(gamm_model$gam)
if (!is.null(log_lik)) {
  waic_value <- tryCatch({
    waic(log_lik)
  }, error = function(e) {
    NA  # Return NA if WAIC cannot be calculated
  })
} else {
  waic_value <- NA  # Set WAIC to NA if log-likelihood is not valid
}

# Create a table of criteria
criteria_table <- data.frame(
  Model = "GAMM",
  AIC = aic_value,
  BIC = bic_value,
  WAIC = ifelse(is.list(waic_value) && "waic" %in% names(waic_value), waic_value$waic, waic_value)  # Correct WAIC extraction
)

# Print criteria table
print(knitr::kable(criteria_table, format = "markdown"))