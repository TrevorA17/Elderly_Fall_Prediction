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

# Calculate measures of central tendency for Sugar Levels
sugar_mean <- mean(elderly_fall_data$Sugar_levels)
sugar_median <- median(elderly_fall_data$Sugar_levels)
sugar_mode <- as.numeric(names(sort(table(elderly_fall_data$Sugar_levels), decreasing = TRUE)[1]))

# Calculate measures of central tendency for SpO2 levels
spo2_mean <- mean(elderly_fall_data$SpO2_levels)
spo2_median <- median(elderly_fall_data$SpO2_levels)
spo2_mode <- as.numeric(names(sort(table(elderly_fall_data$SpO2_levels), decreasing = TRUE)[1]))

# Display the measures of central tendency
cat("Measures of Central Tendency for Distance:\n")
cat("Mean:", distance_mean, "\n")
cat("Median:", distance_median, "\n")
cat("Mode:", distance_mode, "\n\n")

cat("Measures of Central Tendency for HRV:\n")
cat("Mean:", hrv_mean, "\n")
cat("Median:", hrv_median, "\n")
cat("Mode:", hrv_mode, "\n\n")

cat("Measures of Central Tendency for Sugar Levels:\n")
cat("Mean:", sugar_mean, "\n")
cat("Median:", sugar_median, "\n")
cat("Mode:", sugar_mode, "\n\n")

cat("Measures of Central Tendency for SpO2 levels:\n")
cat("Mean:", spo2_mean, "\n")
cat("Median:", spo2_median, "\n")
cat("Mode:", spo2_mode, "\n")

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

# Calculate Measures of Distribution for Sugar Levels
sugar_range <- range(elderly_fall_data$Sugar_levels)
sugar_variance <- var(elderly_fall_data$Sugar_levels)
sugar_sd <- sd(elderly_fall_data$Sugar_levels)
sugar_quantiles <- quantile(elderly_fall_data$Sugar_levels, probs = c(0.25, 0.5, 0.75))

# Calculate Measures of Distribution for SpO2 levels
spo2_range <- range(elderly_fall_data$SpO2_levels)
spo2_variance <- var(elderly_fall_data$SpO2_levels)
spo2_sd <- sd(elderly_fall_data$SpO2_levels)
spo2_quantiles <- quantile(elderly_fall_data$SpO2_levels, probs = c(0.25, 0.5, 0.75))

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

cat("Measures of Distribution for Sugar Levels:\n")
cat("Range:", sugar_range, "\n")
cat("Variance:", sugar_variance, "\n")
cat("Standard Deviation:", sugar_sd, "\n")
cat("Quantiles (25th, 50th, 75th):", sugar_quantiles, "\n\n")

cat("Measures of Distribution for SpO2 levels:\n")
cat("Range:", spo2_range, "\n")
cat("Variance:", spo2_variance, "\n")
cat("Standard Deviation:", spo2_sd, "\n")
cat("Quantiles (25th, 50th, 75th):", spo2_quantiles, "\n")

# Calculate correlation coefficients for numerical variables
correlation_matrix <- cor(elderly_fall_data[c("Distance", "HRV", "Sugar_levels", "SpO2_levels")])

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


