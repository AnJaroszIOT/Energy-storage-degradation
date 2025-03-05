# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate data for Normal Distribution
n <- 1000  # Number of samples
normal_data <- rnorm(n, mean = 2.5, sd = 1)  # Mean = 2.5, SD = 1

# Generate data for Log-Normal Distribution
log_normal_data <- rlnorm(n, meanlog = 0.5, sdlog = 0.2)  # Log-normal parameters

# Generate data for Weibull Distribution
weibull_shape <- 1.5  # Shape parameter
weibull_scale <- 2.0   # Scale parameter
weibull_data <- rweibull(n, shape = weibull_shape, scale = weibull_scale)

# Cap Weibull data at a maximum of 5 for voltage
weibull_data[weibull_data > 5] <- 5

# Create a data frame for plotting
data <- data.frame(
  Value = c(normal_data, log_normal_data, weibull_data),
  Distribution = factor(rep(c("Normal", "Log-Normal", "Weibull"), each = n))
)

# Plot the distributions
ggplot(data, aes(x = Value, fill = Distribution)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plots of Normal, Log-Normal, and Weibull Distributions",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Normal" = "blue", "Log-Normal" = "lightblue", "Weibull" = "darkblue"))