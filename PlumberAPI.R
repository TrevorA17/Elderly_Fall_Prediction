# Load the saved Random Forest model for elderly falls
loaded_elderly_rf_model <- readRDS("./models/elderly_rf_model.rds")

#* @apiTitle Elderly Fall Prediction Model API
#* @apiDescription Used to predict elderly falls.

#* @param Distance Numeric: Distance value
#* @param Pressure Factor: Pressure level (0, 1, 2)
#* @param HRV Numeric: HRV value
#* @param Sugar_level Numeric: Sugar level value
#* @param SpO2 Numeric: SpO2 value
#* @param Accelerometer Factor: Accelerometer reading (0, 1)

#* @post /predict_elderly_fall

predict_elderly_fall <- function(Distance, Pressure, HRV, Sugar_level, SpO2, Accelerometer) {
  
  # Create a data frame using the arguments
  new_elderly_data <- data.frame(
    Distance = as.numeric(Distance),
    Pressure = as.factor(Pressure),
    HRV = as.numeric(HRV),
    Sugar_level = as.numeric(Sugar_level),
    SpO2 = as.numeric(SpO2),
    Accelerometer = as.factor(Accelerometer)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_elderly_rf_model, newdata = new_elderly_data)
  
  # Return the prediction
  return(prediction)
}
