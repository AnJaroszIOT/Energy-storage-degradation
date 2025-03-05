library(rstanarm)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(knitr)

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

# Fit the Bayesian model
bayes_model <- stan_glm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
                        data = df_filtered, family = gaussian())

# Generate predictions
predictions <- predict(bayes_model, df_filtered)

# Calculate information criteria
n_params <- length(bayes_model$coefficients)
info_criteria <- calculate_information_criteria(df_filtered$Max_Voltage_Dischar, predictions, n_params)

# Print the information criteria
print("Information Criteria:")
print(info_criteria)

# Calculate summary statistics
min_value <- min(df_filtered$Max_Voltage_Dischar)
q3_value <- quantile(df_filtered$Max_Voltage_Dischar, 0.75)
median_value <- median(df_filtered$Max_Voltage_Dischar)
q5_value <- quantile(df_filtered$Max_Voltage_Dischar, 0.95)
max_value <- max(df_filtered$Max_Voltage_Dischar)
log_likelihood <- as.numeric(info_criteria$log_likelihood)
likelihood <- as.numeric(info_criteria$likelihood)
std_error <- sqrt(as.numeric(info_criteria$mse))
error <- std_error / mean(df_filtered$Max_Voltage_Dischar)

# Create a table of summary statistics
summary_stats <- data.frame(
  Quantity = c("Min", "3rd Quartile", "Median", "5th Quartile", "Max", "Log-likelihood", "Likelihood", "Standard Error", "Error"),
  Value = c(min_value, q3_value, median_value, q5_value, max_value, log_likelihood, likelihood, std_error, error)
)

# Print the summary statistics table
kable(summary_stats, digits = 3)

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
