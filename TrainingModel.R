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

# Load the required library for data splitting
library(caret)

# Set seed for reproducibility
set.seed(123)

# Specify the percentage of data to be allocated for testing (e.g., 80% training, 20% testing)
split_percentage <- 0.8

# Split the dataset into training and testing sets
training_index <- createDataPartition(elderly_fall_data$Decision, p = split_percentage, list = FALSE)
training_data <- elderly_fall_data[training_index, ]
testing_data <- elderly_fall_data[-training_index, ]

# Print the dimensions of the training and testing sets
print("Dimensions of the training set:")
print(dim(training_data))
print("Dimensions of the testing set:")
print(dim(testing_data))
