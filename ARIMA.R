library(tidyverse)
library(forecast)
library(fpp2)
library(dplyr)
library(ggplot2)
library(caret)

data <- read.csv("https://raw.githubusercontent.com/data-cracker/datasets/main/MasterDataSet.csv")
glimpse(data)
summary(data)

data$total_ghg <- rowSums(data[, 25:34])
# Get the column names from the 2nd to the last column
column_names <- colnames(data)[2:ncol(data)]
# Print the column names
print(column_names)

forecast_list <- list()
for (feature in column_names) {
  ts_data <- ts(data[[feature]], start = min(data$Year), frequency = 1)
  
  # Fit an ARIMA model to the training data
  fit <- auto.arima(ts_data)
  
  # Make forecasts on the test data
  forecast_values <- forecast(fit, h = 10)
  
  forecast_list[[feature]] <- forecast_values$mean
}
forecast_features <- data.frame(forecast_list)
forecast_features

predicted_df <- NULL
year_column <- 2021:2030
predicted_df <- cbind(Year = year_column, forecast_features)
predicted_df$Type <- "Predicted"
print(predicted_df)

historical_df <- data
historical_df$Type = "Historical"

arima_data <- rbind(historical_df, predicted_df)
arima_data$total_ghg <- rowSums(arima_data[, 25:34]) 
arima_data$total_gdp <- rowSums(arima_data[, 2:7])
arima_data$ghg_intensity = arima_data$total_ghg / arima_data$total_gdp
write.csv(arima_data, file = "arima_data.csv", row.names = FALSE)

