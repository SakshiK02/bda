#QUESTION 1
# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the data from Excel
dataa <- Book1

# Prepare the time series data
years <- dataa$t
obs<- dataa$observation
ts_data <- ts(obs, start = min(years), frequency = 1)

# Simple Exponential Smoothing
alpha <- 0.3
ses_model <- ses(ts_data, alpha = alpha)
ses_model

# Holt’s Exponential Smoothing
beta <- 0.2
holt_model <- holt(ts_data, alpha = alpha, beta = beta)
holt_model

# Plot the original data and both smoothed models on the same graph
plot(ts_data, type = "o", col = "blue", xlab = "Year", ylab = "Observation", main = "Time Series with Smoothing")
lines(ses_model$fitted, col = "red", lty = 1)  # SES smoothed values
lines(holt_model$fitted, col = "green", lty = 2)  # Holt's smoothed values

# Add a legend
legend("topleft", legend = c("Original Data", "SES Smoothed", "Holt's Smoothed"),
       col = c("blue", "red", "green"), lty = 1:2, pch = 1)


#QUESTION 2
# Load necessary libraries
install.packages("ggplot2")
library(ggplot2)  # For enhanced plotting

# Prepare the data
years <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 368.3, 369.47, 371.03, 373.61, 357.61)
ts_co2 <- ts(co2_concentration, start = min(years), frequency = 1)

# a) Time Series Plot
plot(ts_co2, type = "o", col = "blue", xlab = "Year", ylab = "Average CO2 Concentration", main = "CO2 Concentration Over Time")

# Calculate the 3-year moving average manually
moving_avg_manual <- function(data, window_size) {
  n <- length(data)
  ma <- rep(NA, n)  # Initialize with NA
  for (i in seq(window_size, n)) {
    ma[i] <- mean(data[(i-window_size+1):i])
  }
  return(ma)
}

# Calculate the 3-year moving average
window_size <- 3
ma_co2 <- moving_avg_manual(co2_concentration, window_size)

# Forecast for 2004 using the last moving average value
forecast_2004 <- tail(ma_co2, 1)

# Display the forecast
cat("Forecasted CO2 Concentration for 2004 using 3-Year Moving Average: ", forecast_2004, "\n")

#QUESTION 3
# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the AirPassengers data
data("AirPassengers")

# Plot the original data
plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Year")

# Apply Holt-Winters Multiplicative Model
holt_winters_model <- HoltWinters(AirPassengers, gamma = TRUE, seasonal = "multiplicative")

# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)
forecasted_values

# Plot the forecasts
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of Passengers", xlab = "Year")
