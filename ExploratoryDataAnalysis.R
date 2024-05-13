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
