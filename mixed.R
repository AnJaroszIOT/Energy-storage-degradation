# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(lme4)  # For mixed models
library(nlme)  # For linear mixed models
library(glmmTMB)  # For advanced mixed models
library(randomForest)  # For random forest modeling
library(GLMMadaptive)  # For adaptive mixed models

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
    warning("GLMM fit is singular. Fitting without random effects.")
    model <- lm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, data = data)
  }
  return(model)
}

# Fit GLMM
glmm_model <- fit_glmm(df_filtered)

# Function to fit HGLMM and check for singularity
fit_hglmm <- function(data) {
  model <- glmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current + (1 | Group), 
                 data = data, family = gaussian(link = "identity"))
  if (isSingular(model)) {
    warning("HGLMM fit is singular. Fitting without random effects.")
    model <- lm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, data = data)
  }
  return(model)
}

# Fit HGLMM
hglmm_model <- fit_hglmm(df_filtered)

# Fit Linear Mixed Model using nlme
linear_mixed_model_nlme <- nlme::lme(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, 
                                     random = ~ 1 | Group, data = df_filtered)

# Fit Adaptive Mixed Model using GLMMadaptive
adaptive_mixed_model <- mixed_model(
  fixed = Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
  random = ~ 1 | Group,
  family = gaussian(),  # Specify the family
  data = df_filtered
)

# Fit Random Forest Model
rf_model <- randomForest(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, data = df_filtered)

# Create plots for GLMM
plot_glmm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point() +
  geom_line(aes(y = predict(glmm_model)), color = "darkblue") +
  labs(title = "GLMM Predictions", x = "Discharge Time", y = "Max Voltage Discharge")

# Create plots for HGLMM
plot_hglmm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point() +
  geom_line(aes(y = predict(hglmm_model)), color = "blue") +
  labs(title = "HGLMM Predictions", x = "Discharge Time", y = "Max Voltage Discharge")

# Create plots for Linear Mixed Model using nlme
plot_linear_mixed_nlme <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point() +
  geom_line(aes(y = predict(linear_mixed_model_nlme)), color = "green") +
  labs(title = "Linear Mixed Model Predictions (nlme)", x = "Discharge Time", y = "Max Voltage Discharge")

# Create plots for Adaptive Mixed Model
pred_adaptive <- predict(adaptive_mixed_model)
plot_adaptive_mixed <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point() +
  geom_line(aes(y = pred_adaptive), color = "orange") +
  labs(title = "Adaptive Mixed Model Predictions", x = "Discharge Time", y = "Max Voltage Discharge")

# Create plots for Random Forest Model
pred_rf <- predict(rf_model)
plot_rf <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point() +
  geom_line(aes(y = pred_rf), color = "red") +
  labs(title = "Random Forest Predictions", x = "Discharge Time", y = "Max Voltage Discharge")

# Arrange plots in a grid
grid.arrange(plot_glmm, plot_hglmm, plot_linear_mixed_nlme, plot_adaptive_mixed, plot_rf, ncol = 2)