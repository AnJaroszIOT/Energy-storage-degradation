# Load necessary libraries
library(readxl)  # Load the readxl package to use read_excel()
library(rstan)   # Load the rstan package for Bayesian modeling
library(bayesplot)  # Load bayesplot for MCMC diagnostics and visualization
library(ggplot2)    # Load ggplot2 for additional plotting capabilities

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
  Charging_time = data[[8]]
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df[df$Max_Voltage_Dischar >= 3.5, ]

# Define the Stan model
stan_model_code <- "
data {
  int<lower=0> N;  // number of observations
  vector[N] Max_Voltage_Dischar;  // predictor
  vector[N] Time_constant_current;  // predictor
  vector[N] Charging_time;  // response
}
parameters {
  real beta0;  // intercept
  real beta1;  // coefficient for Max_Voltage_Dischar
  real beta2;  // coefficient for Time_constant_current
  real<lower=0> sigma;  // standard deviation
}
model {
  Charging_time ~ normal(beta0 + beta1 * Max_Voltage_Dischar + beta2 * Time_constant_current, sigma);
}
"

# Prepare data for Stan
stan_data <- list(
  N = nrow(df_filtered),
  Max_Voltage_Dischar = df_filtered$Max_Voltage_Dischar,
  Time_constant_current = df_filtered$Time_constant_current,
  Charging_time = df_filtered$Charging_time
)

# Fit the model using Stan
stan_fit <- stan(model_code = stan_model_code, data = stan_data, iter = 2000, chains = 4)

# Print the summary of the model
print(stan_fit)

# Visualize the results using bayesplot
# Extract posterior samples
posterior_samples <- extract(stan_fit)

# Plot trace plots for parameters
mcmc_trace(posterior_samples, pars = c("beta0", "beta1", "beta2", "sigma"))

# Plot density of parameters
mcmc_dens(posterior_samples, pars = c("beta0", "beta1", "beta2", "sigma"))

# Calculate and print quantiles for the parameters
quantiles <- apply(posterior_samples$beta0, 2, quantile, probs = c(0.025, 0.5, 0.975))
print(quantiles)

quantiles <- apply(posterior_samples$beta1, 2, quantile, probs = c(0.025, 0.5, 0.975))
print(quantiles)

quantiles <- apply(posterior_samples$beta2, 2, quantile, probs = c(0.025, 0.5, 0.975))
print(quantiles)

quantiles <- apply(posterior_samples$sigma, 2, quantile, probs = c(0.025, 0.5, 0.975))
print(quantiles)