

# Load necessary libraries
library(readxl)       # Load the readxl package to use read_excel()
library(bayesiantools) # Load the bayesiantools package for Bayesian analysis
library(dplyr)        # Load dplyr for data manipulation

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
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Define the model formula
model_formula <- Charging_time ~ Max_Voltage_Dischar + Time_constant_current

# Fit Bayesian MCMC model using bayesiantools
# Here we use the bayesian_model function to fit the model
mcmc_model <- bayesian_model(
  formula = model_formula,
  data = df_filtered,
  family = "gaussian",  # Specify the family for the model
  prior = list(
    b = normal(0, 5),  # Prior for the coefficients
    Intercept = normal(0, 5),  # Prior for the intercept
    sigma = exponential(1)  # Prior for the error term
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4,  # Number of chains
  cores = 4,  # Number of cores for parallel processing
  control = list(adapt_delta = 0.95)  # Control parameters for better convergence
)

# Summary of the model
summary(mcmc_model)

# Posterior predictive checks
pp_check(mcmc_model)

# You can also plot the results
plot(mcmc_model)