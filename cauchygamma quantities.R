# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(gridExtra)

# Assuming df_filtered is already defined and contains the relevant data
# Fit distributions with specified model names
Norm <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
Weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")
Gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
Cauchy <- fitdist(df_filtered$Max_Voltage_Dischar, "cauchy")
Lognorm <- fitdist(df_filtered$Max_Voltage_Dischar, "lnorm")
Logarit <- fitdist(log(df_filtered$Max_Voltage_Dischar), "norm")

# Function to calculate summary statistics for each model
calculate_summary_statistics <- function(model) {
  # Extract fitted values
  fitted_values <- model$estimate
  
  # Calculate statistics
  summary_df <- data.frame(
    Statistic = c("Min", "3Q", "Median", "5Q", "Max"),
    Value = c(
      min(fitted_values),
      quantile(fitted_values, 0.75),
      median(fitted_values),
      quantile(fitted_values, 0.95),
      max(fitted_values)
    ),
    Model = deparse(substitute(model))
  )
  
  return(summary_df)
}

# Calculate summary statistics for each model
summary_statistics <- bind_rows(
  calculate_summary_statistics(Norm),
  calculate_summary_statistics(Weibull),
  calculate_summary_statistics(Gamma),
  calculate_summary_statistics(Cauchy),
  calculate_summary_statistics(Lognorm),
  calculate_summary_statistics(Logarit)
)

# Print the summary statistics table
print("Summary Statistics Table:")
print(summary_statistics)

# Create plots for each statistic
plots <- lapply(unique(summary_statistics$Statistic), function(stat) {
  p <- ggplot(subset(summary_statistics, Statistic == stat), aes(x = Model, y = Value)) +
    geom_point(size = 4, color = "blue") +
    geom_line(aes(group = 1), color = "blue") +
    geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +
    labs(title = paste(stat, "for Each Model"), x = "Model", y = stat) +
    ylim(0, 5000) 
    theme_minimal()
  
  return(p)
})

# Arrange plots in a grid with 1 column and 5 rows
grid.arrange(grobs = plots, nrow = 5)