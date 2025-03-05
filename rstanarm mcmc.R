# Load necessary libraries
library(rstanarm)
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
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Fit the MCMC model using rstanarm with adjusted parameters
result <- stan_glm(
  Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + Time_constant_current,
  data = df_filtered,
  family = gaussian(),
  prior = normal(0, 3),
  prior_intercept = normal(0, 3),
  chains = 4,
  iter = 4000,      # Further increased iterations
  warmup = 200,    # Further increased warmup
  thin = 5,
  control = list(adapt_delta = 0.99)  # Further adjusted adapt_delta
)

# Function to calculate AIC, BIC, WAIC, SD error, likelihood, and summary statistics
calculate_metrics <- function(model, data, predictions) {
  log_likelihood <- logLik(model)
  n <- nrow(data)
  k <- length(coef(model))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # WAIC (using the log-likelihood)
  waic <- -2 * sum(log(predictions)) + 2 * k
  
  # Standard error of predictions
  sd_error <- sd(data$Max_Voltage_Dischar - predictions)
  
  # Likelihood
  likelihood <- exp(log_likelihood)
  
  # Summary statistics
  summary_stats <- data.frame(
    Metric = c("Min", "1Q", "Median", "3Q", "Max"),
    Value = c(min(data$Max_Voltage_Dischar), 
              quantile(data$Max_Voltage_Dischar, 0.25), 
              median(data$Max_Voltage_Dischar), 
              quantile(data$Max_Voltage_Dischar, 0.75), 
              max(data$Max_Voltage_Dischar))
  )
  
  return(list(AIC = aic, BIC = bic, WAIC = waic, SD_Error = sd_error, Likelihood = likelihood, Summary = summary_stats))
}

# Generate predictions
predictions <- predict(result, newdata = df_filtered)

# Calculate overall metrics
metrics <- calculate_metrics(result, df_filtered, predictions)

# Create plots
p1 <- ggplot(data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p2 <- ggplot(df_filtered, aes(x = Decrement_3.6_3.4V, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge vs Decrement 3.6-3.4V", x = "Decrement 3.6-3.4V", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p3 <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge vs Discharge Time", x = "Discharge Time", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

p4 <- ggplot(df_filtered, aes(x = Time_constant_current, y = Max_Voltage_Dischar)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Max Voltage Discharge vs Time Constant Current", x = "Time Constant Current", y = "Max Voltage Discharge") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

# Calculate metrics for each plot
metrics1 <- calculate_metrics(result, df_filtered, predictions)
metrics2 <- calculate_metrics(result, df_filtered, predict(result, newdata = df_filtered %>% select(Decrement_3.6_3.4V)))
metrics3 <- calculate_metrics(result, df_filtered, predict(result, newdata = df_filtered %>% select(Discharge_Time)))
metrics4 <- calculate_metrics(result, df_filtered, predict(result, newdata = df_filtered %>% select(Time_constant_current)))

# Create tables for each metrics
table1 <- data.frame(Metric = c("AIC", "BIC", "WAIC", "Standard Error", "Likelihood"), Value = unlist(metrics1[c("AIC", "BIC", "WAIC", "SD_Error", "Likelihood")]))
table2 <- data.frame(Metric = c("AIC", "BIC", "WAIC", "Standard Error", "Likelihood"), Value = unlist(metrics2[c("AIC", "BIC", "WAIC", "SD_Error", "Likelihood")]))
table3 <- data.frame(Metric = c("AIC", "BIC", "WAIC", "Standard Error", "Likelihood"), Value = unlist(metrics3[c("AIC", "BIC", "WAIC", "SD_Error", "Likelihood")]))
table4 <- data.frame(Metric = c("AIC", "BIC", "WAIC", "Standard Error", "Likelihood"), Value = unlist(metrics4[c("AIC", "BIC", "WAIC", "SD_Error", "Likelihood")]))

# Arrange all plots and their corresponding tables in a grid
grid.arrange(p1, tableGrob(table1), p2, tableGrob(table2), p3, tableGrob(table3), p4, tableGrob(table4), ncol = 2)