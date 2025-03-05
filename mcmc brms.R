# Load necessary libraries
library(readxl)  # Load the readxl package to use read_excel()
library(brms)    # Load the brms package for Bayesian regression modeling

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

# Fit Bayesian MCMC model using brms with verbose output
mcmc_model <- brm(
  formula = Charging_time ~ Max_Voltage_Dischar + Time_constant_current,
  data = df_filtered,
  family = gaussian(),  # Change to appropriate family if needed
  prior = set_prior("normal(0, 5)", class = "b"),  # Prior for the coefficients
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4,  # Number of chains
  cores = 4,  # Number of cores for parallel processing
  control = list(adapt_delta = 0.95),  # Optional: control parameters
  verbose = TRUE  # Enable verbose output
)

# Summary of the model
print(summary(mcmc_model))

# Plot the results
plot(mcmc_model)