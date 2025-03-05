# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(gridExtra)  # For arranging plots
library(fitdistrplus)  # For fitting distributions

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
  Charging_time = data[[8]] * 10  # Reduce charging time by a factor of 10
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df %>% filter(Max_Voltage_Dischar >= 3.5)

# Fit distributions
fit_normal <- fitdistr(df_filtered$Max_Voltage_Dischar, "normal")
fit_cauchy <- fitdistr(df_filtered$Max_Voltage_Dischar, "cauchy")
fit_exponential <- fitdistr(df_filtered$Max_Voltage_Dischar, "exponential")
fit_gamma <- fitdist(df_filtered$Max_Voltage_Dischar, "gamma")
fit_weibull <- fitdist(df_filtered$Max_Voltage_Dischar, "weibull")

# Function to calculate summary statistics
calculate_summary_stats <- function(model, model_name) {
  if (model_name == "Normal") {
    quantiles <- quantile(df_filtered$Max_Voltage_Dischar, probs = c(0, 0.25, 0.5, 0.75, 1))
  } else if (model_name == "Cauchy") {
    quantiles <- qcauchy(c(0, 0.25, 0.5, 0.75, 1), location = model$estimate[1], scale = model$estimate[2])
  } else if (model_name == "Exponential") {
    quantiles <- qexp(c(0, 0.25, 0.5, 0.75, 1), rate = 1/mean(df_filtered$Max_Voltage_Dischar))
  } else if (model_name == "Gamma") {
    quantiles <- qgamma(c(0, 0.25, 0.5, 0.75, 1), shape = model$estimate[1], rate = model$estimate[2])
  } else if (model_name == "Weibull") {
    quantiles <- qweibull(c(0, 0.25, 0.5, 0.75, 1), shape = model$estimate[1], scale = model$estimate[2])
  }
  
  return(quantiles)
}

# Calculate summary statistics for Distributions
summary_normal <- calculate_summary_stats(fit_normal, "Normal")
summary_cauchy <- calculate_summary_stats(fit_cauchy, "Cauchy")
summary_exponential <- calculate_summary_stats(fit_exponential, "Exponential")
summary_gamma <- calculate_summary_stats(fit_gamma, "Gamma")
summary_weibull <- calculate_summary_stats(fit_weibull, "Weibull")

# Combine summaries into a data frame
summary_df <- data.frame(
  Model = rep(c("Normal", "Cauchy", "Exponential", "Gamma", "Weibull"), each = 5),
  Statistic = rep(c("Min", "Q1", "Median", "Q3", "Max"), times = 5),
  Value = c(summary_normal, summary_cauchy, summary_exponential, summary_gamma, summary_weibull)
)

# Create separate plots for each statistic
plot_min <- ggplot(subset(summary_df, Statistic == "Min"), aes(x = Model, y = Value)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  labs(title = "Minimum Values for Distributions", y = "Min Value", x = "Model") +
  theme_minimal()

plot_q3 <- ggplot(subset(summary_df, Statistic == "Q3"), aes(x = Model, y = Value)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  labs(title = "3rd Quartile (Q3) Values for Distributions", y = "Q3 Value", x = "Model") +
  theme_minimal()

plot_median <- ggplot(subset(summary_df, Statistic == "Median"), aes(x = Model, y = Value)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  labs(title = "Median Values for Distributions", y = "Median Value", x = "Model") +
  theme_minimal()

plot_q5 <- ggplot(subset(summary_df, Statistic == "Q3"), aes(x = Model, y = Value)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  labs(title = "5th Quartile (Q5) Values for Distributions", y = "Q5 Value", x = "Model") +
  theme_minimal()

plot_max <- ggplot(subset(summary_df, Statistic == "Max"), aes(x = Model, y = Value)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  labs(title = "Maximum Values for Distributions", y = "Max Value", x = "Model") +
  theme_minimal()

# Arrange the plots in a grid with 5 rows
grid.arrange(plot_min, plot_q3, plot_median, plot_q5, plot_max, nrow = 5)