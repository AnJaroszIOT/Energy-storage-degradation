# Load necessary libraries
library(ggplot2)
library(readxl)  # Load the readxl package to use read_excel()
library(dplyr)  # For data manipulation
library(broom)  # For tidying model outputs
library(gridExtra)  # For arranging plots
library(mgcv)  # For Generalized Additive Models
library(rpart)  # For decision trees
library(lme4)  # For Hierarchical Generalized Linear Models

# Set CPU and elapsed time limits to Inf
options(timeout = 600)  # Set timeout to 10 minutes
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

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

# Check if a grouping variable exists; if not, create a dummy variable
if (!"Group" %in% names(df_filtered)) {
  df_filtered$Group <- as.factor(sample(1:3, nrow(df_filtered), replace = TRUE))  # Create a dummy grouping variable
}

# Linear Adaptive Model using Recursive Least Squares (RLS)
adaptive_model <- function(data, alpha = 0.99) {
  n <- nrow(data)
  p <- 3  # Number of parameters (intercept and two slopes)
  
  # Initialize parameters
  theta <- matrix(0, nrow = p, ncol = 1)  # Parameter vector
  P <- diag(1 / (1 - alpha), p)  # Covariance matrix
  predictions <- numeric(n)  # Store predictions
  
  for (i in 1:n) {
    x <- c(1, data$Discharge_Time[i], data$Decrement_3.6_3.4V[i])  # Input vector
    y <- data$Max_Voltage_Dischar[i]  # Output value
    
    # Prediction
    predictions[i] <- t(theta) %*% x
    
    # Update step
    y_hat <- predictions[i]
    error <- y - y_hat
    K <- P %*% x / (alpha + t(x) %*% P %*% x)  # Kalman gain
    theta <- theta + K * error  # Update parameters
    P <- (P - K %*% t(x) %*% P) / alpha  # Update covariance matrix
  }
  
  return(predictions)
}

# Fit the adaptive model
adaptive_predictions <- adaptive_model(df_filtered)

# Create plots for Adaptive Model
plot_adaptive <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "darkgreen") +
  geom_line(aes(y = adaptive_predictions), color = "red") +
  labs(title = "Linear Adaptive Model (RLS)", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Generalized Linear Model using 'glm' function
glm_model <- glm(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, 
                 data = df_filtered, family = gaussian())
summary(glm_model)

# Hierarchical Generalized Linear Model using 'lmer' function
hglm_model <- lmer(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V + (1 | Group), 
                   data = df_filtered)
summary(hglm_model)

# Linear Model using Nonlinear Least Squares (nls)
linear_model <- nls(Max_Voltage_Dischar ~ a + b * Discharge_Time + c * Decrement_3.6_3.4V, 
                    data = df_filtered, start = list(a = 0, b = 1, c = 1))
summary(linear_model)

# Generalized Additive Model (GAM)
gam_model <- gam(Max_Voltage_Dischar ~ s(Discharge_Time) + s(Decrement_3.6_3.4V, k = 5), data = df_filtered)
summary(gam_model)

# Decision Tree Model
tree_model <- rpart(Max_Voltage_Dischar ~ Discharge_Time + Decrement_3.6_3.4V, data = df_filtered)
summary(tree_model)

# Create plots for GLM
plot_glm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "darkgreen") +
  geom_line(aes(y = predict(glm_model)), color = "darkred") +
  labs(title = "Generalized Linear Model (glm)", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Create plots for HGLM
plot_hglm <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "darkgreen") +
  geom_line(aes(y = predict(hglm_model)), color = "darkred") +
  labs(title = "Hierarchical GLM", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Create plots for Linear Model
plot_linear <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "darkgreen") +
  geom_line(aes(y = predict(linear_model)), color = "darkred") +
  labs(title = "Linear Model (nls)", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Create plots for GAM
plot_gam <- ggplot(df_filtered, aes(x = Discharge_Time, y = predict(gam_model, newdata = df_filtered))) +
  geom_point(aes(y = Max_Voltage_Dischar), color = "darkgreen") +
  geom_line(color = "darkred") +
  labs(title = "Generalized Additive Model", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Create plots for Decision Tree
plot_tree <- ggplot(df_filtered, aes(x = Discharge_Time, y = Max_Voltage_Dischar)) +
  geom_point(color = "darkgreen") +
  geom_line(aes(y = predict(tree_model, newdata = df_filtered)), color = "darkred") +
  labs(title = "Decision Tree Model", x = "Discharge Time", y = "Max Voltage \n Discharge")

# Arrange plots in a grid with six rows
grid.arrange(plot_adaptive, plot_glm, plot_hglm, plot_linear, plot_gam, plot_tree, nrow = 3, ncol = 2)  # Arrange all six plots

# Print model summaries
cat("Adaptive Model Summary:\n")
print(adaptive_predictions)
cat("\nGeneralized Linear Model Summary (glm):\n")
print(summary(glm_model))
cat("\nHierarchical GLM Summary (lmer):\n")
print(summary(hglm_model))
cat("\nLinear Model Summary (nls):\n")
print(summary(linear_model))
cat("\nGAM Model Summary:\n")
print(summary(gam_model))
cat("\nDecision Tree Model Summary:\n")
print(summary(tree_model))