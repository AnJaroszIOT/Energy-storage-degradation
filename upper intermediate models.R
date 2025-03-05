# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(lme4)  # For mixed models
library(mgcv)  # For Generalized Additive Models
library(randomForest)  # For Random Forest models

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

# Rescale predictor variables to address different scales
df_filtered <- df_filtered %>%
  mutate(across(c(Discharge_Time, Decrement_3.6_3.4V, Time_constant_current), 
                ~ scale(.) %>% as.vector()))

# Function to fit GLMM and check for singularity
fit_glmm <- function(data) {
  model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current + (1 | Group), 
                data = data)
  if (isSingular(model)) {
    warning("GLMM fit is singular. Returning NA.")
    return(NA)  # Return NA instead of fitting without random effects
  }
  return(model)
}

# Fit GLMM
glmm_model <- fit_glmm(df_filtered)

# Function to fit HGLMM and check for singularity
fit_hglmm <- function(data) {
  model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current + (1 | Group), 
                data = data)
  if (isSingular(model)) {
    warning("HGLMM fit is singular. Returning NA.")
    return(NA)  # Return NA instead of fitting without random effects
  }
  return(model)
}

# Fit HGLMM
hglmm_model <- fit_hglmm(df_filtered)

# Generalized Additive Mixed Model (GAMM)
gamm_model <- gamm(Max_Voltage_Dischar ~ s(Discharge_Time) + s(Decrement_3.6_3.4V) + Time_constant_current, 
                   random = list(Group = ~1), data = df_filtered)

# Random Forest Model
rf_model <- randomForest(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, 
                         data = df_filtered, importance = TRUE)

# Summary statistics for predictors
summary_stats <- df_filtered %>%
  summarise(
    Discharge_Time = list(summary(Discharge_Time)),
    Decrement_3.6_3.4V = list(summary(Decrement_3.6_3.4V)),
    Time_constant_current = list(summary(Time_constant_current))
  )

# Extracting summary statistics
summary_table <- data.frame(
  Parameter = c("Min", "1Q", "Median", "3Q", "Max"),
  Discharge_Time = unlist(lapply(summary_stats$Discharge_Time, function(x) x[c(1, 2, 3, 5, 6)])),
  Decrement_3.6_3.4V = unlist(lapply(summary_stats$Decrement_3.6_3.4V, function(x) x[c(1, 2, 3, 5, 6)])),
  Time_constant_current = unlist(lapply(summary_stats$Time_constant_current, function(x) x[c(1, 2, 3, 5, 6)]))
)

# Random effects for GLMM and HGLMM
random_effects_glmm <- if (inherits(glmm_model, "lmerMod")) ranef(glmm_model) else NA
random_effects_hglmm <- if (inherits(hglmm_model, "lmerMod")) ranef(hglmm_model) else NA

# Extract random effects
random_effects_table <- data.frame(
  Model = c("GLMM", "HGLMM"),
  Random_Effects = c(
    if (!is.na(random_effects_glmm)) paste("Group (Intercept):", random_effects_glmm$Group[,1], collapse = ", ") else NA,
    if (!is.na(random_effects_hglmm)) paste("Group (Intercept):", random_effects_hglmm$Group[,1], collapse = ", ") else NA
  )
)

# Correlation of Fixed Effects
correlation_fixed_effects_glmm <- if (inherits(glmm_model, "lmerMod")) cor(summary(glmm_model)$coefficients) else NA
correlation_fixed_effects_hglmm <- if (inherits(hglmm_model, "lmerMod")) cor(summary(hglmm_model)$coefficients) else NA

# Extract correlation of fixed effects
correlation_table <- data.frame(
  Model = c("GLMM", "HGLMM"),
  Correlation = c(
    if (!is.na(correlation_fixed_effects_glmm) && is.matrix(correlation_fixed_effects_glmm)) {
      paste(correlation_fixed_effects_glmm[upper.tri(correlation_fixed_effects_glmm)], collapse = ", ")
    } else {
      NA
    },
    if (!is.na(correlation_fixed_effects_hglmm) && is.matrix(correlation_fixed_effects_hglmm)) {
      paste(correlation_fixed_effects_hglmm[upper.tri(correlation_fixed_effects_hglmm)], collapse = ", ")
    } else {
      NA
    }
  )
)

# Parametric coefficients for GLMM, HGLMM, and GAMM
parametric_coefficients_glmm <- if (!is.na(glmm_model)) summary(glmm_model)$coefficients else NA
parametric_coefficients_hglmm <- if (!is.na(hglmm_model)) summary(hglmm_model)$coefficients else NA
parametric_coefficients_gamm <- summary(gamm_model$gam)$p.coeff

# Create a table for parametric coefficients
parametric_table <- data.frame(
  Model = c("GLMM", "HGLMM", "GAMM"),
  Coefficients = c(
    if (!is.na(parametric_coefficients_glmm)) paste(rownames(parametric_coefficients_glmm), collapse = ", ") else NA,
    if (!is.na(parametric_coefficients_hglmm)) paste(rownames(parametric_coefficients_hglmm), collapse = ", ") else NA,
    paste(rownames(parametric_coefficients_gamm), collapse = ", ")
  )
)

# Approximate significance of smooth terms for GAMM
smooth_terms_gamm <- summary(gamm_model$gam)$s.table

# Create a table for smooth terms significance
smooth_terms_table <- data.frame(
  Term = rownames(smooth_terms_gamm),
  Approx_Significance = smooth_terms_gamm[, "p-value"]
)

# Combine all tables into one summary table
final_summary_table <- list(
  Summary_Statistics = summary_table,
  Random_Effects = random_effects_table,
  Correlation_Fixed_Effects = correlation_table,
  Parametric_Coefficients = parametric_table,
  Smooth_Terms_Significance = smooth_terms_table
)

# Print the final summary tables
print(final_summary_table$Summary_Statistics)
print(final_summary_table$Random_Effects)
print(final_summary_table$Correlation_Fixed_Effects)
print(final_summary_table$Parametric_Coefficients)
print(final_summary_table$Smooth_Terms_Significance)