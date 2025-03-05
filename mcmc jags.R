# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(lme4)  # For mixed models
library(mgcv)  # For Generalized Additive Models
library(knitr)  # For creating tables
library(randomForest)  # For Random Forest models
library(rstan)  # For Stan
library(MCMCpack)  # For MCMC methods
library(jagsUI)  # For JAGS
library(brms)  # For Bayesian regression using Stan

# Set CPU and elapsed time limits to Inf
options(timeout = 600)  # Set timeout to 10 minutes
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

# Read the Excel file
data <- read_excel("Dataset.xlsx")

# Create the dataframe with the specified columns
df <- data.frame(
  Discharge_Time = data[[2]],
  Decrement_3_6_3_4V = data[[3]],  # Changed variable name to avoid periods
  Max_Voltage_Dischar = data[[4]],
  Min_Voltage_Charg = data[[5]],
  Time_at_4_15V = data[[6]],
  Time_constant_current = data[[7]],
  Charging_time = data[[8]] * 10  # Reduce charging time by a factor of 10
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Check if a grouping variable exists; if not, create a dummy variable
if (!"Group" %in% names(df_filtered)) {
  df_filtered$Group <- as.factor(sample(1:3, nrow(df_filtered), replace = TRUE))  # Create a dummy grouping variable
}

# Prepare data for Stan
stan_data <- list(
  N = nrow(df_filtered),
  Discharge_Time = df_filtered$Discharge_Time,
  Decrement_3_6_3_4V = df_filtered$Decrement_3_6_3_4V,  # Updated variable name
  Time_constant_current = df_filtered$Time_constant_current,
  Max_Voltage_Dischar = df_filtered$Max_Voltage_Dischar
)

# Stan model code
stan_model_code <- "
data {
  int<lower=0> N;
  vector[N] Discharge_Time;
  vector[N] Decrement_3_6_3_4V;  // Updated variable name
  vector[N] Time_constant_current;
  vector[N] Max_Voltage_Dischar;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real beta3;
  real<lower=0> sigma;
}
model {
  Max_Voltage_Dischar ~ normal(alpha + beta1 * Discharge_Time + beta2 * Decrement_3_6_3_4V + beta3 * Time_constant_current, sigma);
}
"

# Fit Stan model
stan_fit <- stan(model_code = stan_model_code, data = stan_data, iter = 2000, chains = 4)
print(stan_fit)

# MCMCpack model
mcmc_model <- MCMCregress(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3_6_3_4V + Time_constant_current, data = df_filtered)
summary(mcmc_model)

# JAGS model
jags_model_code <- "
model {
  for (i in 1:N) {
    Max_Voltage_Dischar[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta1 * Discharge_Time[i] + beta2 * Decrement_3_6_3_4V[i]  // Updated variable name
      + beta3 * Time_constant_current[i];
  }
  alpha ~ dnorm(0, 0.001);
  beta1 ~ dnorm(0, 0.001);
  beta2 ~ dnorm(0, 0.001);
  beta3 ~ dnorm(0, 0.001);
  tau ~ dgamma(0.001, 0.001);
}
"

# Prepare data for JAGS
jags_data <- list(
  N = nrow(df_filtered),
  Discharge_Time = df_filtered$Discharge_Time,
  Decrement_3_6_3_4V = df_filtered$Decrement_3_6_3_4V,  # Updated variable name
  Time_constant_current = df_filtered$Time_constant_current,
  Max_Voltage_Dischar = df_filtered$Max_Voltage_Dischar
)

# Fit JAGS model
jags_fit <- jags(jags_data, inits = NULL, parameters.to.save = c("alpha", "beta1", "beta2", "beta3"), model.file = textConnection(jags_model_code), n.chains = 3, n.iter = 2000)
print(jags_fit)

# brms model
brms_model <- brm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3_6_3_4V + Time_constant_current, data = df_filtered, family = gaussian(), iter = 2000, chains = 4)
summary(brms_model)

# Create a table for model summaries
model_summaries <- data.frame(
  Model = c("Stan", "MCMCpack", "JAGS", "brms"),
  Summary = c(summary(stan_fit)$summary[1:4, ], summary(mcmc_model), summary(jags_fit), summary(brms_model)$fixed)
)

# Print the model summaries table
kable(model_summaries, caption = "MCMC Model Summaries")

# Create plots for the models (example for Stan)
stan_samples <- extract(stan_fit)
plot(stan_samples$alpha, main = "Posterior Distribution of Alpha (Stan)", xlab = "Alpha", ylab = "Density")