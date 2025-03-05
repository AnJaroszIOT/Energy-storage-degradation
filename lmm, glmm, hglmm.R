# Load necessary libraries
library(readxl)  # Load the readxl package to use read_excel()
library(brms)    # Load the brms package for Bayesian regression modeling
library(loo)     # Load the loo package for model comparison

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

# Fit Bayesian MCMC model using brms
mcmc_model <- brm(
  formula = Charging_time ~ Max_Voltage_Dischar + Time_constant_current,
  data = df_filtered,
  family = gaussian(),  # Change to appropriate family if needed
  prior = set_prior("normal(0, 5)", class = "b"),  # Prior for the coefficients
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations (first half of total iterations)
  chains = 4,  # Number of chains
  cores = 4  # Number of cores for parallel processing
)

# Calculate AIC, BIC, and WAIC
model_aic <- AIC(mcmc_model)
model_bic <- BIC(mcmc_model)
model_waic <- waic(mcmc_model)

# Create a summary table
model_summary <- data.frame(
  Criterion = c("AIC", "BIC", "WAIC"),
  Value = c(model_aic, model_bic, model_waic$waic)
)

# Print the summary table
print(model_summary)