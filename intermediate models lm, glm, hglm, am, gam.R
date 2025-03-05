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

# Fit a Linear Model
linear_model <- lm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, data = df_filtered)

# Fit a Generalized Linear Model (GLM)
glm_model <- glm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, data = df_filtered, family = gaussian())

# Fit a Hierarchical Linear Model
glmer_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Discharge_Time), data = df_filtered)

# Fit an Additive Model
additive_model <- lm(Max_Voltage_Dischar ~ poly(Discharge_Time, 2) + poly(Decrement_3.6_3.4V, 2), data = df_filtered)

# Fit a Generalized Additive Model (GAM)
gam_model <- gam(Max_Voltage_Dischar ~ s(Discharge_Time) + s(Decrement_3.6_3.4V), data = df_filtered)

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
  Model = c("Linear", "GLM", "Hierarchical", "Additive", "GAM"),
  AIC = c(AIC(linear_model), AIC(glm_model), AIC(glmer_model), AIC(additive_model), AIC(gam_model)),
  BIC = c(BIC(linear_model), BIC(glm_model), BIC(glmer_model), BIC(additive_model), BIC(gam_model)),
  WAIC = c(calculate_waic(linear_model), calculate_waic(glm_model), calculate_waic(glmer_model), calculate_waic(additive_model), calculate_waic(gam_model))
)

# Display the model summaries
print(model_summaries)

# Create a coefficients table
coefficients_table <- bind_rows(
  tidy(linear_model) %>% mutate(Model = "Linear"),
  tidy(glm_model) %>% mutate(Model = "GLM"),
  tidy(glmer_model, effects = "fixed") %>% mutate(Model = "Hierarchical"),
  tidy(additive_model) %>% mutate(Model = "Additive"),
  tidy(gam_model) %>% mutate(Model = "GAM")
)

# Display the coefficients table
print(coefficients_table)

# Create plots for each model

# Linear Model Plot
plot_linear <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred") +
  labs(title = "Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# GLM Plot
plot_glm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = gaussian()), color = "darkred") +
  labs(title = "Generalized Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Hierarchical Model Plot
df_filtered$Predicted_GLMM <- predict(glmer_model)
plot_glmer <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_GLMM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Hierarchical Generalized Linear Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Additive Model Plot
df_filtered$Predicted_Additive <- predict(additive_model)
plot_additive <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_Additive)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Additive Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# GAM Plot
df_filtered$Predicted_GAM <- predict(gam_model)
plot_gam <- ggplot(df_filtered, aes(x = Discharge_Time, y = Predicted_GAM)) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "blue") +
  geom_line(color = "darkred") +
  labs(title = "Generalized Additive Model", x = "Discharge Time", y = "Max Volt Dischar") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10))  # Adjust y-axis title size

# Arrange plots in a grid with five rows
grid.arrange(plot_linear, plot_glm, plot_glmer, plot_additive, plot_gam, nrow = 5)