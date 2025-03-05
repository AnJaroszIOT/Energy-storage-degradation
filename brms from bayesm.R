# Load necessary libraries
library(brms)
library(MASS)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(knitr)

# Set seed for reproducibility
set.seed(123)

# Try reading the Excel file with error handling
data <- tryCatch({
  read_excel("Dataset.xlsx")
}, error = function(e) {
  message("Error reading the Excel file: ", e)
  return(NULL)
})

# Check if data was read successfully
if (is.null(data)) {
  stop("Data could not be loaded. Please check the file path and format.")
}

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

# Filter the dataframe
df_filtered <- df %>% 
  filter(Max_Voltage_Dischar >= 3.5, 
         Discharge_Time > 0, 
         Decrement_3.6_3.4V > 0, 
         Time_constant_current > 0)

# Fit the Bayesian regression model using brms
model <- brm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
             data = df_filtered,
             family = gaussian(),
             iter = 2000,
             warmup = 500,
             chains = 4,
             seed = 123)

# Summary of the model
summary(model)

# Generate predictions
predictions <- predict(model, newdata = df_filtered)

# Add predictions to the dataframe for visualization
df_filtered$Predicted_Voltage <- predictions[, "Estimate"]

# Plot the predictions against the actual values
ggplot(df_filtered, aes(x = Max_Voltage_Dischar, y = Predicted_Voltage)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Predicted vs Actual Max Voltage Discharge",
       x = "Actual Max Voltage Discharge",
       y = "Predicted Max Voltage Discharge") +
  theme_minimal()# Load necessary libraries
library(brms)
library(MASS)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(knitr)

# Set seed for reproducibility
set.seed(123)

# Try reading the Excel file with error handling
data <- tryCatch({
  read_excel("Dataset.xlsx")
}, error = function(e) {
  message("Error reading the Excel file: ", e)
  return(NULL)
})

# Check if data was read successfully
if (is.null(data)) {
  stop("Data could not be loaded. Please check the file path and format.")
}

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

# Filter the dataframe
df_filtered <- df %>% 
  filter(Max_Voltage_Dischar >= 3.5, 
         Discharge_Time > 0, 
         Decrement_3.6_3.4V > 0, 
         Time_constant_current > 0)

# Fit the Bayesian regression model using brms
model <- brm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
             data = df_filtered,
             family = gaussian(),
             iter = 2000,
             warmup = 500,
             chains = 4,
             seed = 123)

# Summary of the model
summary(model)

# Generate predictionsz
predictions <- predict(model, newdata = df_filtered)

# Add predictions to the dataframe for visualization
df_filtered$Predicted_Voltage <- predictions[, "Estimate"]

# Plot the predictions against the actual values
ggplot(df_filtered, aes(x = Max_Voltage_Dischar, y = Predicted_Voltage)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Predicted vs Actual Max Voltage Discharge",
       x = "Actual Max Voltage Discharge",
       y = "Predicted Max Voltage Discharge") +
  theme_minimal()