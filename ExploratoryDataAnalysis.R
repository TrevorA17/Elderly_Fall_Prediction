# Load dataset
elderly_fall_data <- read.csv("data/elderly.csv", colClasses = c(
  Distance = "numeric",
  Pressure = "integer",
  HRV = "numeric",
  Sugar_level = "numeric",
  SpO2 = "numeric",
  Accelerometer = "integer",
  Decision = "integer"
))

# Display the structure of the dataset
str(elderly_fall_data)

# View the first few rows of the dataset
head(elderly_fall_data)

# View the dataset in a separate viewer window
View(elderly_fall_data)
