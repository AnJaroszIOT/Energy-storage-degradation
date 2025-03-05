


# Load necessary libraries
library(readxl)  # Load the readxl package to use read_excel()
library(rstanarm)  # Load the rstanarm package for Bayesian regression modeling
library(bayesplot)  # Load the bayesplot package for plotting
library(ggplot2)  # Load ggplot2 for additional plotting capabilities

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

# Fit Bayesian MCMC model using rstanarm
mcmc_model <- stan_glm(
  formula = Charging_time ~ Max_Voltage_Dischar + Time_constant_current,
  data = df_filtered,
  family = gaussian(),  # Change to appropriate family if needed
  prior = normal(0, 5),  # Prior for the coefficients
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations (first half of total iterations)
  chains = 4,  # Number of chains
  cores = 4  # Number of cores for parallel processing
)

# Summary of the model
print(summary(mcmc_model))

# Plot the results
plot(mcmc_model)

# Extract posterior samples
posterior_samples <- as.data.frame(mcmc_model)

# Create distribution plots for all parameters
color_scheme_set("brightblue")  # Set color scheme for plots

# Plotting posterior distributions for all parameters
mcmc_areas(posterior_samples, 
           pars = c("(Intercept)", "Max_Voltage_Dischar", "Time_constant_current"),
           prob = 0.95) + 
  labs(y = "Sigma") +  # Set the y-axis title to "Sigma"
  theme_minimal() +  # Optional: use a minimal theme for better aesthetics
  theme(axis.title.y = element_text(size = 16, face = "bold"),  # Customize y-axis title size and style
        plot.title = element_text(size = 20, face = "bold")) +  # Customize plot title size and style
  ggtitle("**Posterior Distributions of Model Parameters**")  # Set the title

# Alternatively, use mcmc_trace to visualize the chains
mcmc_trace(mcmc_model, facet_args = list(ncol = 2), 
           color = c("black", "darkblue", "blue", "lightblue")) +  # Set colors for each chain
  labs(y = "Sigma") +  # Set the y-axis title to "Sigma"
  theme_minimal() +
  theme(axis.title.y = element_text(size = 16, face = "bold"),  # Customize y-axis title size and style
        plot.title = element_text(size = 20, face = "bold")) +  # Customize plot title size and style
  ggtitle("**Trace Plots for Model Parameters**")  # Set the title