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

# Define the number of bootstrap samples
num_bootstraps <- 1000

# Create an empty vector to store bootstrap statistics
bootstrap_statistics <- numeric(num_bootstraps)

# Perform bootstrapping
for (i in 1:num_bootstraps) {
  # Resample the dataset with replacement
  bootstrap_sample <- elderly_fall_data[sample(nrow(elderly_fall_data), replace = TRUE), ]
  
  # Calculate the statistic of interest (e.g., mean, median, etc.)
  # For example, let's calculate the mean of the Distance variable
  bootstrap_statistics[i] <- mean(bootstrap_sample$Distance)
}

# Compute the confidence interval
confidence_interval <- quantile(bootstrap_statistics, c(0.025, 0.975))

# Print the confidence interval
print(confidence_interval)

# Train a Random Forest Regression model
rf_model <- train(Decision ~ ., data = elderly_fall_data, method = "rf", trControl = train_control)

# Print the trained Random Forest model
print(rf_model)

# Load the required library for modeling
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a Gradient Boosting Regression model
gbm_model <- train(Decision ~ ., data = elderly_fall_data, method = "gbm", trControl = train_control)

# Print the trained Gradient Boosting model
print(gbm_model)

# Load the required library for modeling
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a Support Vector Regression (SVR) model
svr_model <- train(Decision ~ ., data = elderly_fall_data, method = "svmRadial", trControl = train_control)

# Print the trained SVR model
print(svr_model)
