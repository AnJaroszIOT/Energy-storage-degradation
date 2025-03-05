# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(broom.mixed)  # For tidying mixed models
library(gridExtra)  # For arranging plots
library(lme4)  # For hierarchical models
library(mgcv)  # For Generalized Additive Models

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

# Fit models
linear_model <- lm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, data = df_filtered)

# GLM Model
glm_model <- glm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, 
                       data = df_filtered, family = Gamma(link = "log"))

# Hierarchical Generalized Linear Model
glmer_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Discharge_Time), data = df_filtered)

# Additive Model
additive_model <- lm(Max_Voltage_Dischar ~ poly(Discharge_Time, 2) + poly(Decrement_3.6_3.4V, 2), data = df_filtered)

# Generalized Additive Model
gam_model <- gam(Max_Voltage_Dischar ~ s(Discharge_Time) + s(Decrement_3.6_3.4V), data = df_filtered)

# Hierarchical Generalized Advanced Model
hglm_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (Discharge_Time | Decrement_3.6_3.4V), data = df_filtered)

# Function to calculate WAIC
calculate_waic <- function(model) {
  log_likelihood <- logLik(model)
  n <- length(residuals(model))
  k <- length(coef(model))
  waic <- -2 * (log_likelihood - k)
  return(waic)
}

# Create a summary table for the models
model_summaries <- data.frame(
  Model = c("Linear", "Gamma GLM", "Hierarchical", "Additive", "GAM", "Hierarchical Advanced"),
  AIC = c(AIC(linear_model), AIC(gamma_glm_model), AIC(glmer_model), AIC(additive_model), AIC(gam_model), AIC(hglm_model)),
  BIC = c(BIC(linear_model), BIC(gamma_glm_model), BIC(glmer_model), BIC(additive_model), BIC(gam_model), BIC(hglm_model)),
  WAIC = c(calculate_waic(linear_model), calculate_waic(gamma_glm_model), calculate_waic(glmer_model), calculate_waic(additive_model), calculate_waic(gam_model), calculate_waic(hglm_model))
)

# Display the model summaries
print(model_summaries)

# Create a coefficients table
coefficients_table <- bind_rows(
  tidy(linear_model) %>% mutate(Model = "Linear"),
  tidy(gamma_glm_model) %>% mutate(Model = "Gamma GLM"),
  tidy(glmer_model, effects = "fixed") %>% mutate(Model = "Hierarchical"),
  tidy(additive_model) %>% mutate(Model = "Additive"),
  tidy(gam_model) %>% mutate(Model = "GAM"),
  tidy(hglm_model, effects = "fixed") %>% mutate(Model = "Hierarchical Advanced")
)

# Display the coefficients table
print(coefficients_table)

# Calculate summary statistics for predictions
summary_stats <- function(model, model_name) {
  predictions <- predict(model, type = "response")  # Use type = "response" for GLM
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
  summary_stats(linear_model, "Linear"),
  summary_stats(gamma_glm_model, "Gamma GLM"),
  summary_stats(glmer_model, "Hierarchical"),
  summary_stats(additive_model, "Additive"),
  summary_stats(gam_model, "GAM"),
  summary_stats(hglm_model, "Hierarchical Advanced")
)

# Display the summary statistics
print(summary_statistics)

# Create plots for each model

# Linear Model Plot
plot_linear <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred") +
  labs(title = "Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Gamma GLM Plot
df_filtered$Predicted_GammaGLM <- predict(gamma_glm_model, type = "response")
plot_glm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_GammaGLM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Gamma Generalized Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# GLMER Model Plot
df_filtered$Predicted_GLMM <- predict(glmer_model)
plot_glmer <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_GLMM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Hierarchical Generalized Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Arrange plots in a grid with appropriate rows
grid.arrange(plot_linear, plot_glm, plot_glmer, nrow = 3)
