# Load necessary libraries
library(readxl)  # Load the readxl package to use read_excel()
library(MCMCpack)  # Load the MCMCpack package for Bayesian modeling
library(coda)  # Load coda for MCMC diagnostics

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

# Number of chains and iterations
num_chains <- 4
num_iterations <- 2000
burnin <- 1000  # Number of burn-in iterations

# Initialize a list to store MCMC results for each chain
mcmc_results <- vector("list", num_chains)

# Fit Bayesian MCMC model using MCMCpack for each chain and store results
for (chain in 1:num_chains) {
  start_time <- Sys.time()  # Start time for the chain
  
  # Warm-up phase
  warmup_start <- Sys.time()
  mcmc_results[[chain]] <- MCMCregress(
    Charging_time ~ Max_Voltage_Dischar + Time_constant_current,
    data = df_filtered,
    b0 = c(0, 0, 0),  # Prior means
    B0 = diag(1000, 3),  # Prior variances
    mcmc = num_iterations,  # Total number of iterations
    burnin = burnin  # Number of burn-in iterations
  )
  warmup_end <- Sys.time()  # End time for warm-up
  
  # Sampling phase
  sampling_start <- Sys.time()
  # The sampling is already included in the MCMCregress call, so we just need to calculate the time
  sampling_end <- Sys.time()  # End time for sampling
  
  end_time <- Sys.time()  # End time for the chain
  elapsed_time <- end_time - start_time  # Calculate total elapsed time
  
  # Calculate elapsed times
  warmup_time <- warmup_end - warmup_start
  sampling_time <- sampling_end - sampling_start
  
  # Print elapsed times for the current chain
  cat(sprintf("Chain %d:\n", chain))
  cat(sprintf("  Elapsed Time (Warm-up): %.6f seconds\n", as.numeric(warmup_time)))
  cat(sprintf("  Elapsed Time (Sampling): %.6f seconds\n", as.numeric(sampling_time)))
  cat(sprintf("  Elapsed Time (Total): %.6f seconds\n\n", as.numeric(elapsed_time)))
}

# Combine results from all chains
combined_mcmc <- do.call(rbind, mcmc_results)

# Summary of the combined model
summary_combined <- summary(combined_mcmc)
print(summary_combined)

# Optional: Visualize results
mcmc_chain <- mcmc(combined_mcmc)
plot(mcmc_chain)