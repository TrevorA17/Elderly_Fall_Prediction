# Load dataset
elderly_fall_data <- read.csv("data/elderly.csv", colClasses = c(
  Distance = "numeric",
  Pressure = "factor",
  HRV = "numeric",
  Sugar_level = "numeric",
  SpO2 = "numeric",
  Accelerometer = "factor",
  Decision = "factor"
))

# Display the structure of the dataset
str(elderly_fall_data)

# View the first few rows of the dataset
head(elderly_fall_data)

# View the dataset in a separate viewer window
View(elderly_fall_data)

# Calculate counts and percentages for Pressure levels
pressure_freq <- table(elderly_fall_data$Pressure)
pressure_percent <- prop.table(pressure_freq) * 100

# Calculate counts and percentages for Accelerometer readings
accelerometer_freq <- table(elderly_fall_data$Accelerometer)
accelerometer_percent <- prop.table(accelerometer_freq) * 100

# Calculate counts and percentages for Decision outcomes
decision_freq <- table(elderly_fall_data$Decision)
decision_percent <- prop.table(decision_freq) * 100

# Display the frequency tables
cat("Frequency of Pressure levels:\n")
print(pressure_freq)
cat("\nPercentage of Pressure levels:\n")
print(pressure_percent)

cat("\nFrequency of Accelerometer readings:\n")
print(accelerometer_freq)
cat("\nPercentage of Accelerometer readings:\n")
print(accelerometer_percent)

cat("\nFrequency of Decision outcomes:\n")
print(decision_freq)
cat("\nPercentage of Decision outcomes:\n")
print(decision_percent)

# Calculate measures of central tendency for Distance
distance_mean <- mean(elderly_fall_data$Distance)
distance_median <- median(elderly_fall_data$Distance)
distance_mode <- as.numeric(names(sort(table(elderly_fall_data$Distance), decreasing = TRUE)[1]))

# Calculate measures of central tendency for HRV
hrv_mean <- mean(elderly_fall_data$HRV)
hrv_median <- median(elderly_fall_data$HRV)
hrv_mode <- as.numeric(names(sort(table(elderly_fall_data$HRV), decreasing = TRUE)[1]))

# Display the measures of central tendency
cat("Measures of Central Tendency for Distance:\n")
cat("Mean:", distance_mean, "\n")
cat("Median:", distance_median, "\n")
cat("Mode:", distance_mode, "\n\n")

cat("Measures of Central Tendency for HRV:\n")
cat("Mean:", hrv_mean, "\n")
cat("Median:", hrv_median, "\n")
cat("Mode:", hrv_mode, "\n\n")

# Calculate Measures of Distribution for Distance
distance_range <- range(elderly_fall_data$Distance)
distance_variance <- var(elderly_fall_data$Distance)
distance_sd <- sd(elderly_fall_data$Distance)
distance_quantiles <- quantile(elderly_fall_data$Distance, probs = c(0.25, 0.5, 0.75))

# Calculate Measures of Distribution for HRV
hrv_range <- range(elderly_fall_data$HRV)
hrv_variance <- var(elderly_fall_data$HRV)
hrv_sd <- sd(elderly_fall_data$HRV)
hrv_quantiles <- quantile(elderly_fall_data$HRV, probs = c(0.25, 0.5, 0.75))

# Display the Measures of Distribution
cat("Measures of Distribution for Distance:\n")
cat("Range:", distance_range, "\n")
cat("Variance:", distance_variance, "\n")
cat("Standard Deviation:", distance_sd, "\n")
cat("Quantiles (25th, 50th, 75th):", distance_quantiles, "\n\n")

cat("Measures of Distribution for HRV:\n")
cat("Range:", hrv_range, "\n")
cat("Variance:", hrv_variance, "\n")
cat("Standard Deviation:", hrv_sd, "\n")
cat("Quantiles (25th, 50th, 75th):", hrv_quantiles, "\n\n")

# Calculate correlation coefficients for numerical variables
correlation_matrix <- cor(elderly_fall_data[c("Distance", "HRV", "Sugar_level", "SpO2")])

# Display correlation matrix
cat("Correlation Matrix for Numerical Variables:\n")
print(correlation_matrix)

# Calculate contingency table for Pressure and Decision
pressure_decision_table <- table(elderly_fall_data$Pressure, elderly_fall_data$Decision)

# Display contingency table
cat("\nContingency Table for Pressure and Decision:\n")
print(pressure_decision_table)

# Calculate contingency table for Accelerometer and Decision
accelerometer_decision_table <- table(elderly_fall_data$Accelerometer, elderly_fall_data$Decision)

# Display contingency table
cat("\nContingency Table for Accelerometer and Decision:\n")
print(accelerometer_decision_table)

# Load the required library for ANOVA
library(stats)

# Perform ANOVA for Distance across different levels of Pressure
anova_result <- aov(Distance ~ Pressure, data = elderly_fall_data)

# Summary of ANOVA results
summary(anova_result)

# Load the required library for plotting
library(ggplot2)

# Create histograms for numerical variables
distance_plot <- ggplot(elderly_fall_data, aes(x = Distance)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Distance", x = "Distance", y = "Frequency")

hrv_plot <- ggplot(elderly_fall_data, aes(x = HRV)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of HRV", x = "HRV", y = "Frequency")

sugar_level_plot <- ggplot(elderly_fall_data, aes(x = Sugar_level)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(title = "Histogram of Sugar Level", x = "Sugar Level", y = "Frequency")

spo2_plot <- ggplot(elderly_fall_data, aes(x = SpO2)) +
  geom_histogram(binwidth = 5, fill = "lightyellow", color = "black") +
  labs(title = "Histogram of SpO2", x = "SpO2", y = "Frequency")

# Load the required library for arranging plots
library(gridExtra)

# Combine plots into a single display
grid.arrange(distance_plot, hrv_plot, sugar_level_plot, spo2_plot, ncol = 2)

# Create box plot for Distance across different Pressure levels
distance_pressure_plot <- ggplot(elderly_fall_data, aes(x = Pressure, y = Distance, fill = Pressure)) +
  geom_boxplot() +
  labs(title = "Box Plot of Distance by Pressure Levels", x = "Pressure", y = "Distance")

# Create box plot for HRV across different Pressure levels
hrv_pressure_plot <- ggplot(elderly_fall_data, aes(x = Pressure, y = HRV, fill = Pressure)) +
  geom_boxplot() +
  labs(title = "Box Plot of HRV by Pressure Levels", x = "Pressure", y = "HRV")

# Create violin plot for Sugar Level across different Pressure levels
sugar_pressure_plot <- ggplot(elderly_fall_data, aes(x = Pressure, y = Sugar_level, fill = Pressure)) +
  geom_violin() +
  labs(title = "Violin Plot of Sugar Level by Pressure Levels", x = "Pressure", y = "Sugar Level")

# Create violin plot for SpO2 across different Pressure levels
spo2_pressure_plot <- ggplot(elderly_fall_data, aes(x = Pressure, y = SpO2, fill = Pressure)) +
  geom_violin() +
  labs(title = "Violin Plot of SpO2 by Pressure Levels", x = "Pressure", y = "SpO2")

# Combine plots into a single display
grid.arrange(distance_pressure_plot, hrv_pressure_plot, sugar_pressure_plot, spo2_pressure_plot, ncol = 2)

