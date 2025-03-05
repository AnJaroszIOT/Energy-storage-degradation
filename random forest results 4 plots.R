# Load necessary libraries
library(randomForest)  # For Random Forest
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

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
df_filtered <- df %>% 
  filter(Max_Voltage_Dischar >= 3.5, 
         Discharge_Time > 0, 
         Decrement_3.6_3.4V > 0, 
         Time_constant_current > 0)

# Fit the Random Forest model
rf_model <- randomForest(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                         data = df_filtered,
                         ntree = 500,  # Number of trees
                         mtry = 2,     # Number of variables randomly sampled as candidates at each split
                         importance = TRUE)

# Generate predictions
predictions <- predict(rf_model, newdata = df_filtered)

# Calculate summary statistics for Max Voltage Discharge
data_summary <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Calculate error summary
errors <- predictions - df_filtered$Max_Voltage_Dischar
error_summary <- data.frame(
  Metric = c("Mean Error", "Standard Deviation of Error"),
  Value = c(mean(errors), sd(errors))
)

# Calculate AIC and BIC (using residuals)
n <- nrow(df_filtered)
rss <- sum((errors)^2)  # Residual sum of squares
aic <- n * log(rss/n) + 2 * length(rf_model$predicted)  # AIC approximation
bic <- n * log(rss/n) + log(n) * length(rf_model$predicted)  # BIC approximation

# Summaries for AIC, BIC, and WAIC (WAIC is not applicable here)
aic_bic_waic_summary <- data.frame(
  Metric = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, NA)  # WAIC is not applicable for Random Forest
)

# Print summaries
print("Data Summary:")
print(data_summary)

print("Error Summary:")
print(error_summary)

print("AIC, BIC, WAIC Summary:")
print(aic_bic_waic_summary)

# Summaries for plots p1, p2, p3, and p4
summary_p1 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(predictions, probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p2 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Decrement_3.6_3.4V, probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p3 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Discharge_Time, probs = c(0, 0.25, 0.5, 0.75, 1))
)

summary_p4 <- data.frame(
  Metric = c("Min", "Q1", "Median", "Q3", "Max"),
  Value = quantile(df_filtered$Time_constant_current, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# Print summaries for plots
print("Summary for Plot 1 (Predictions):")
print(summary_p1)

print("Summary for Plot 2 (Decrement vs Max Voltage Discharge):")
print(summary_p2)

print("Summary for Plot 3 (Discharge Time vs Max Voltage Discharge):")
print(summary_p3)

print("Summary for Plot 4 (Time Constant Current vs Max Voltage Discharge):")
print(summary_p4)

# Create a plot of actual vs predicted values
plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions)

p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Additional plots with regression lines
p2 <- ggplot(df_filtered, aes(x = Decrement_3.6_3.4V, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Decrement 3.6-3.4V", x = "Decrement 3.6-3.4V", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p3 <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Discharge Time", x = "Discharge Time", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p4 <- ggplot(df_filtered, aes(x = Time_constant_current, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge \n vs Time Constant Current", x = "Time Constant Current", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Arrange all plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)