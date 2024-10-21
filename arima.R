###ARIMA(1,1,1)###

# Load necessary libraries
library(forecast)
library(tseries)

# (1) Set up the data and create a time series object
sales <- c(1000, 1050, 1100, 1150, 1200, 1250, 
           1300, 1350, 1400, 1450, 1500, 1550)

# Create a time series object with monthly frequency starting from January
sales_ts <- ts(sales, start = c(1, 1), frequency = 12)

# Plot the original time series
plot(sales_ts, type = "o", col = "blue", lwd = 2, 
     main = "Monthly Sales Time Series", xlab = "Month", ylab = "Sales")

# (2) Fit an ARIMA(1,1,1) model to the data
arima_model <- arima(sales_ts, order = c(1, 1, 1))
arima_model <- arima(sales_ts, order = c(0, 1, 0))

print(summary(arima_model))  # Display model summary

# (3) Check the residual diagnostics of the fitted model
checkresiduals(arima_model)

# (4) Forecast the next 12 values using the fitted model
forecast_values <- forecast(arima_model, h = 12)

# Print the forecasted values
print(forecast_values)

# (5) Plot the original time series, fitted values, and forecasted values
plot(forecast_values, main = "Sales Forecast for Next 12 Months", 
     xlab = "Month", ylab = "Sales", col = "darkgreen", lwd = 2)

# Overlay the original time series
lines(sales_ts, col = "blue", lwd = 2)

# Add a legend to the plot
legend("topleft", legend = c("Original Data", "Forecasted Data"), 
       col = c("blue", "darkgreen"), lty = 1, lwd = 2)





###ARIMA airline###

# Load necessary libraries
library(forecast)
library(tseries)

# (1) Monthly airline passenger data from 1949 to 1959
years <- rep(1949:1959, each = 12)
months <- rep(month.abb, times = 11)
passengers <- c(3.5, 3.6, 3.7, 3.8, 4, 4.1, 4.4, 4.5, 4.2, 4, 3.9, 3.8,
                3.7, 3.8, 4, 4.2, 4.5, 4.6, 4.9, 5, 5, 4.7, 4.3, 4.2,
                4, 4.2, 4.4, 4.6, 4.9, 5, 5.3, 5.5, 5.2, 5, 4.8, 4.7,
                4.8, 5, 5.2, 5.4, 5.7, 5.9, 6.2, 6.5, 6.1, 5.8, 5.6,
                5.5, 5.7, 5.9, 6.1, 6.3, 6.6, 6.8, 7.1, 7.4, 7, 6.7,
                6.5, 6.3, 6.5, 6.7, 6.9, 7.2, 7.5, 7.8, 8.1, 8.4, 8,
                7.7, 7.5, 7.4, 7.6, 7.8, 8, 8.3, 8.6, 8.9, 9.2, 9.5,
                9.1, 8.8, 8.6, 8.5, 8.7, 8.9, 9.1, 9.4, 9.7, 10, 10.3,
                10.6, 10.2, 9.9, 9.7, 9.6, 9.8, 10, 10.2, 10.5, 10.8,
                11.1, 11.4, 11.7, 11.3, 11, 10.8, 10.7, 10.9, 11.1,
                11.3, 11.6, 11.9, 12.2, 12.5, 12.8, 12.4, 12.1, 11.9,
                11.8, 12, 12.2, 12.4, 12.7, 13, 13.3, 13.6, 13.9,
                13.5, 13.2, 13, 12.9)

# Create a time series object
passengers_ts <- ts(passengers, start = c(1949, 1), frequency = 12)

# (1) Plot the time series
plot(passengers_ts, type = "o", col = "blue", main = "Monthly Airline Passengers (1949-1959)", 
     xlab = "Year", ylab = "Passengers (Thousands)")

# Check for stationarity using the Augmented Dickey-Fuller test
adf_test <- adf.test(passengers_ts)
print(adf_test)

# (2) Fit ARIMA(1,1,1) model to the data
arima_model <- arima(passengers_ts, order = c(1, 1, 1))

# Print model summary
summary(arima_model)

# (3) Check residual diagnostics
checkresiduals(arima_model)

# (4) Forecast the next 12 months
forecast_values <- forecast(arima_model, h = 12)

# Print the forecasted values
print(forecast_values)

# (5) Plot the original time series, fitted values, and forecasted values
plot(forecast_values, main = "Passenger Forecast for 1960", 
     xlab = "Year", ylab = "Passengers (Thousands)")
lines(passengers_ts, col = "blue")  # Overlay the original time series
