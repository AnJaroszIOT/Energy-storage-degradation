# Load necessary libraries
install.packages("runjags")
library(readxl)  # Load the readxl package to use read_excel()
library(runjags)  # Load the runjags package for Bayesian modeling with JAGS
library(coda)     # Load coda for MCMC diagnostics

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

# Prepare data for JAGS
jags_data <- list(
  N = nrow(df_filtered),
  Charging_time = df_filtered$Charging_time,
  Max_Voltage_Dischar = df_filtered$Max_Voltage_Dischar,
  Time_constant_current = df_filtered$Time_constant_current
)

# JAGS model specification
jags_model <- "
model {
  for (i in 1:N) {
    Charging_time[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1 * Max_Voltage_Dischar[i] + beta2 * Time_constant_current[i]
  }
  
  # Priors
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)  # Precision
  sigma <- 1 / sqrt(tau)  # Standard deviation
}
"

# Initial values for the parameters
inits <- function() {
  list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), tau = rgamma(1, 0.001, 0.001))
}

# Number of chains and iterations
num_chains <- 4
num_iterations <- 2000
burnin <- 1000  # Number of burn-in iterations

# Run the JAGS model using runjags
runjags_fit <- run.jags(
  model = jags_model,
  data = jags_data,
  inits = inits,
  n.chains = num_chains,
  n.burnin = burnin,
  n.iter = num_iterations,
  n.thin = 1,
  method = "parallel"  # Use parallel processing for faster execution
)

# Summary of the model
summary_runjags <- summary(runjags_fit)
print(summary_runjags)

# MCMC diagnostics
mcmc_chain <- as.mcmc(runjags_fit)
plot(mcmc_chain)

# Optional: Check convergence diagnostics
gelman_diag <- gelman.diag(mcmc_chain)
print(gelman_diag)