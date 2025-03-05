# Load necessary libraries
library(ggplot2)
library(lme4)  # For mixed models
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

# Ensure 'Group' variable is defined; you may need to create it based on your data
# Uncomment and modify the line below if 'Group' is in your dataset
# df$Group <- as.factor(data[[9]])  # Assuming the group variable is in the 9th column

# If 'Group' does not exist, create a dummy variable for testing
# Example: Create a dummy group variable based on the number of rows
df$Group <- as.factor(rep(1:2, length.out = nrow(df)))  # Example of creating a dummy group variable

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Fit the GLMM model
glmm_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current + (1 | Group), 
                   data = df_filtered)

# Print model summary
print(summary(glmm_model))

# Calculate AIC, BIC, and WAIC
aic_value <- AIC(glmm_model)
bic_value <- BIC(glmm_model)
log_lik <- logLik(glmm_model)
waic_value <- tryCatch({
  waic(log_lik)
}, error = function(e) {
  NA  # Return NA if WAIC cannot be calculated
})

# Create a table of criteria
criteria_table <- data.frame(
  Model = "GLMM",
  AIC = aic_value,
  BIC = bic_value,
  WAIC = ifelse(is.list(waic_value) && "waic" %in% names(waic_value), waic_value$waic, waic_value)  # Correct WAIC extraction
)

# Print criteria table
print(knitr::kable(criteria_table, format = "markdown"))