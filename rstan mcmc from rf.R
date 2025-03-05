# Load necessary libraries
library(rstan)
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

# Scale the predictors
df_filtered <- df_filtered %>%
  mutate(across(c(Discharge_Time, Decrement_3.6_3.4V, Time_constant_current), scale))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df_filtered),
  x1 = df_filtered$Discharge_Time,
  x2 = df_filtered$Decrement_3.6_3.4V,
  x3 = df_filtered$Time_constant_current,
  y = df_filtered$Max_Voltage_Dischar
)

# Stan model code with priors
stan_model_code <- "
data {
  int<lower=0> N; // number of observations
  vector[N] x1; // Discharge Time
  vector[N] x2; // Decrement 3.6-3.4V
  vector[N] x3; // Time Constant Current
  vector[N] y; // Max Voltage Discharge
}
parameters {
  real alpha; // intercept
  vector[3] beta; // coefficients for predictors
  real<lower=0> sigma; // error term
}
model {
  // Priors
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  
  // Likelihood
  y ~ normal(alpha + beta[1] * x1 + beta[2] * x2 + beta[3] * x3, sigma);
}
"

# Fit the model using Stan with increased iterations and max_treedepth
stan_fit <- stan(model_code = stan_model_code, data = stan_data, 
                 iter = 4000, chains = 4, control = list(max_treedepth = 15))

# Print the summary of the model to check for issues
print(stan_fit)

# Check for divergent transitions
if (any(stan_fit@sim$diagnostics[, "divergent__"] > 0)) {
  warning("There are divergent transitions in the model fitting.")
}

# Extract the posterior samples
posterior_samples <- extract(stan_fit)

# Check if posterior_samples$beta is available
if (!is.null(posterior_samples$beta) && nrow(posterior_samples$beta) > 0) {
  # Create predictions based on the posterior samples
  n_samples <- nrow(posterior_samples$beta)
  predictions <- matrix(0, nrow = nrow(df_filtered), ncol = n_samples)
  
  for (i in 1:n_samples) {
    predictions[, i] <- posterior_samples$alpha[i] + 
      posterior_samples$beta[i, 1] * df_filtered$Discharge_Time + 
      posterior_samples$beta[i, 2] * df_filtered$Decrement_3.6_3.4V + 
      posterior_samples$beta[i, 3] * df_filtered$Time_constant_current
  }
  
  # Take the mean of the predictions across the posterior samples
  predictions <- rowMeans(predictions)
  
  # Create a plot of actual vs predicted values
  plot_data <- data.frame(Actual = df_filtered$Max_Voltage_Dischar, Predicted = predictions)
  
  p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "darkred", se = FALSE) +
    labs(title = "Actual vs Predicted Values", x = "Actual Max Voltage Discharge", y = "Predicted Max Voltage Discharge") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))
  
  # Additional plots for predictors
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
  
  # Arrange all plots in a grid
  grid.arrange(p1, p2, p3, p4, ncol = 2)