# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Saving the Random Forest model for the elderly dataset
saveRDS(models$RF, file = "./models/elderly_rf_model.rds")

# Load the saved model
loaded_elderly_rf_model <- readRDS("./models/elderly_rf_model.rds")

# Prepare new data for prediction
new_elderly_data <- data.frame(
  Distance = 25.54,
  Pressure = factor(1, levels = c(0, 1, 2)),  # Assuming factor levels are 0, 1, and 2
  HRV = 101.396,
  Sugar_level = 61.08,
  SpO2 = 87.77,
  Accelerometer = factor(1, levels = c(0, 1)),  # Assuming factor levels are 0 and 1
  Decision = factor(0, levels = c(0, 1, 2))  # Assuming factor levels are 0, 1, and 2
)

# Use the loaded model to make predictions for elderly data
predictions_elderly_loaded_model <- predict(loaded_elderly_rf_model, newdata = new_elderly_data)

# Print predictions
print(predictions_elderly_loaded_model)
