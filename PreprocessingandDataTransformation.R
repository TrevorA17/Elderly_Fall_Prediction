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

# Count missing values in each column
missing_values <- colSums(is.na(elderly_fall_data))

# Display columns with missing values
print("Columns with Missing Values:")
print(names(missing_values[missing_values > 0]))

# Display total number of missing values
print("Total Number of Missing Values:")
print(sum(missing_values))
