# Load necessary libraries
library(mgcv)  # For fitting GAMMs
library(ggplot2)  # For plotting
library(readxl)  # For reading Excel files
library(gridExtra)  # For arranging plots

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
  Charging_time = data[[8]],
  Group = as.factor(data[[9]])  # Ensure Group is treated as a factor
)

# Filter the dataframe to include only rows where Max Voltage Discharge is >= 3.5V
df_filtered <- df[df$Max_Voltage_Dischar >= 3.5, ]

# Rescale the predictor variables
df_filtered$Max_Voltage_Dischar <- scale(df_filtered$Max_Voltage_Dischar)
df_filtered$Decrement_3.6_3.4V <- scale(df_filtered$Decrement_3.6_3.4V)

# Fit Generalized Additive Mixed Model (GAMM)
gamm_model <- gamm(Charging_time ~ s(Max_Voltage_Dischar) + s(Decrement_3.6_3.4V), 
                   random = list(Group = ~1), 
                   data = df_filtered)

# Display the summary of the GAMM
cat("Summary of Generalized Additive Mixed Model (GAMM):\n")
print(summary(gamm_model$lme))  # Display summary of the linear mixed model part

# Create a plot for the fitted GAMM
plot_gamm <- ggplot(df_filtered, aes(x = Max_Voltage_Dischar, y = Charging_time)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x) + s(Decrement_3.6_3.4V), se = TRUE, color = "darkblue") +
  labs(title = "Generalized Additive Mixed Model", 
       x = "Max Voltage Discharge", 
       y = "Charging Time") +
  theme(plot.title = element_text(size = 16, face = "bold"),  # Increased title size
        axis.title = element_text(size = 12),  # Axis titles size
        axis.text = element_text(size = 10))  # Axis text size

# Display the plot
print(plot_gamm)

# Extract fitted values and residuals for further analysis
fitted_values <- gamm_model$gam$fitted.values
residuals <- gamm_model$gam$residuals

# Create a summary table for model diagnostics
model_diagnostics <- data.frame(
  Metric = c("AIC", "BIC", "Log-Likelihood", "Residual SD"),
  Value = c(AIC(gamm_model$lme), BIC(gamm_model$lme), logLik(gamm_model$lme), sd(residuals))
)

# Print the summary table
cat("\nModel Diagnostics:\n")
print(model_diagnostics)