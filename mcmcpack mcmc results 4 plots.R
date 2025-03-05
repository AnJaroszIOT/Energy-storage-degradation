# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(broom)
library(broom.mixed)
library(gridExtra)
library(cowplot)  # For using plot_grid
library(lme4)
library(mgcv)
library(car)

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

# Rescale variables to improve convergence
df_filtered <- df_filtered %>%
  mutate(across(c(Discharge_Time, Decrement_3.6_3.4V, Time_constant_current), 
                ~ scale(.) %>% as.vector()))

# Check for multicollinearity
vif_values <- vif(lm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current, data = df_filtered))
print(vif_values)

# Fit mixed models with control parameters to help with convergence
control_params <- lmerControl(opt = "optimx", 
                              optimizer = "nlminb", 
                              optCtrl = list(maxit = 1000))

# Linear Mixed Model
lmm_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Discharge_Time), 
                  data = df_filtered, control = control_params)

# Generalized Linear Mixed Model (assuming a Gamma distribution)
glmm_model <- glmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Discharge_Time), 
                    data = df_filtered, family = Gamma(link = "log"), 
                    control = glmerControl(optimizer = "optimx", 
                                           optCtrl = list(maxit = 1000)))

# Hierarchical Generalized Linear Mixed Model (HGLMM)
hglmm_model <- glmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Discharge_Time) + (1 | Decrement_3.6_3.4V), 
                     data = df_filtered, family = Gamma(link = "log"), 
                     control = glmerControl(optimizer = "optimx", 
                                            optCtrl = list(maxit = 1000)))

# Function to calculate WAIC
calculate_waic <- function(model) {
  log_likelihood <- logLik(model)
  n <- length(residuals(model))
  k <- length(fixef(model))  # Use fixef for mixed models
  waic <- -2 * (log_likelihood - k)
  return(waic)
}

# Create a summary table for the models
model_summaries <- data.frame(
  Model = c("Linear Mixed", "Gamma GLMM", "HGLMM"),
  AIC = c(AIC(lmm_model), AIC(glmm_model), AIC(hglmm_model)),
  BIC = c(BIC(lmm_model), BIC(glmm_model), BIC(hglmm_model)),
  WAIC = c(calculate_waic(lmm_model), calculate_waic(glmm_model), calculate_waic(hglmm_model))
)

# Display the model summaries
print(model_summaries)

# Create a coefficients table
coefficients_table <- bind_rows(
  tidy(lmm_model) %>% mutate(Model = "Linear Mixed"),
  tidy(glmm_model, effects = "fixed") %>% mutate(Model = "Gamma GLMM"),
  tidy(hglmm_model, effects = "fixed") %>% mutate(Model = "HGLMM")
)

# Display the coefficients table
print(coefficients_table)

# Calculate summary statistics for predictions
summary_stats <- function(model, model_name) {
  predictions <- predict(model, type = "response")  # Use type = "response" for GLMM
  stats <- data.frame(
    Model = model_name,
    Min = min(predictions),
    Q1 = quantile(predictions, 0.25),
    Median = median(predictions),
    Q3 = quantile(predictions, 0.75),
    Max = max(predictions)
  )
  return(stats)
}

# Calculate summary statistics for each model
summary_statistics <- bind_rows(
  summary_stats(lmm_model, "Linear Mixed"),
  summary_stats(glmm_model, "Gamma GLMM"),
  summary_stats(hglmm_model, "HGLMM")
)

# Display the summary statistics
print(summary_statistics)

# Create plots for each model

# Linear Mixed Model Plot
df_filtered$Predicted_LMM <- predict(lmm_model)
plot_lmm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_LMM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Linear Mixed Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Gamma GLMM Plot
df_filtered$Predicted_GLMM <- predict(glmm_model, type = "response")
plot_glmm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_GLMM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Gamma Generalized Linear Mixed Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# HGLMM Plot
df_filtered$Predicted_HGLMM <- predict(hglmm_model, type = "response")
plot_hglmm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_HGLMM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Hierarchical Generalized Linear Mixed Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Arrange plots in a grid using cowplot
plot_grid(plot_lmm, plot_glmm, plot_hglmm, nrow = 3)