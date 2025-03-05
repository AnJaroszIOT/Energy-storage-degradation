# Load necessary libraries
library(rstan)
library(loo)

# Set options for Stan
options(mc.cores = parallel::detectCores())

# Example data (replace with your actual dataset)
data <- list(
  N = 100,            # Number of observations
  x = rnorm(100),     # Predictor variable
  y = rnorm(100)      # Response variable
)

# Stan model definition as a string
stan_model_code <- "
data {
  int<lower=0> N;         // number of observations
  vector[N] x;            // predictor
  vector[N] y;            // response
}
parameters {
  real Intercept;         // intercept
  real beta;              // slope
  real<lower=0> sigma;    // error term
}
model {
  y ~ normal(Intercept + beta * x, sigma); // likelihood
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_model_code)

# Fit the model with specified control parameters
fit <- sampling(stan_model, data = data, 
                iter = 2000, chains = 4,
                control = list(max_treedepth = 15, adapt_delta = 0.95, save_pars = save_pars(all = TRUE)))

# Extracting the results
waic_result <- loo(fit, moment_match = TRUE)

# Displaying the WAIC results
print(waic_result)

# Accessing looic correctly
looic_value <- waic_result$estimates["looic", "estimate"]
cat("Looic Value:", looic_value, "\n")

# Check for problematic observations
print(waic_result$diagnostics)