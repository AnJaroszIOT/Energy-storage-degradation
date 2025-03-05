# Load necessary libraries
library(ggplot2)
library(lme4)  # For mixed models
library(knitr)  # For table formatting
library(loo)  # For WAIC calculation

# Fit the HGLMM model
hglmm_model <- glmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current + (1 | Group), 
                     data = df_filtered, family = gaussian(link = "identity"))

# Print model summary
print(summary(hglmm_model))

# Calculate AIC, BIC, and WAIC
aic_value <- AIC(hglmm_model)
bic_value <- BIC(hglmm_model)
log_lik <- logLik(hglmm_model)
waic_value <- tryCatch({
  waic(log_lik)
}, error = function(e) {
  NA  # Return NA if WAIC cannot be calculated
})

# Create a table of criteria
criteria_table <- data.frame(
  Model = "HGLMM",
  AIC = aic_value,
  BIC = bic_value,
  WAIC = ifelse(is.list(waic_value) && "waic" %in% names(waic_value), waic_value$waic, waic_value)  # Correct WAIC extraction
)

# Print criteria table
print(knitr::kable(criteria_table, format = "markdown"))