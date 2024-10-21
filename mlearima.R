# Load necessary libraries
library(forecast)
library(tseries)
#Estimate the parameters of an ARIMA model

# 1. Define the time series data
sales <- c(800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300)
months <- seq(as.Date("2023-01-01"), by = "month", length.out = length(sales))

# 2. Create a time series object
sales_ts <- ts(sales, start = c(2023, 1), frequency = 12)

# 3. Plot the time series data
plot(sales_ts, main = "Monthly Sales Data", ylab = "Sales", xlab = "Month", col = "blue")

# 4. Estimate ARIMA model parameters using auto.arima
arima_fit <- auto.arima(sales_ts)

# 5. Print the ARIMA model summary
summary(arima_fit)

# 6. Plot the fitted ARIMA model diagnostics
tsdiag(arima_fit)

# 7. Forecast the next 5 months and plot the forecast
forecast_values <- forecast(arima_fit, h = 5)
plot(forecast_values, main = "ARIMA Model Forecast", col = "red")


#determine the optimal values of p, d, and q for an ARIMA(p,d,q) model


# 1. Define the quarterly GDP data
gdp <- c(100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155)
quarters <- seq(as.Date("2010-01-01"), by = "quarter", length.out = length(gdp))

# 2. Create a time series object
gdp_ts <- ts(gdp, start = c(2010, 1), frequency = 4)

# 3. Plot the time series data
plot(gdp_ts, main = "Quarterly GDP Data", ylab = "GDP", xlab = "Quarter", col = "blue")

# 4. Determine the optimal values of p, d, and q using auto.arima
arima_fit <- auto.arima(gdp_ts)

# 5. Print the ARIMA model summary to check the optimal p, d, q values
summary(arima_fit)

# 6. Plot the diagnostics of the ARIMA model
tsdiag(arima_fit)

# 7. Plot the fitted ARIMA model forecast for the next 4 quarters
forecast_values <- forecast(arima_fit, h = 4)
plot(forecast_values, main = "ARIMA Model Forecast", col = "red")



#Estimate the parameters of an ARIMA model using Maximum Likelihood Estimation (MLE)


# Load necessary libraries
library(forecast)
library(tseries)

# 1. Define the monthly temperature data
temperature <- c(20, 22, 25, 28, 30, 32, 35, 38, 40, 42, 45, 48)
months <- seq(as.Date("2023-01-01"), by = "month", length.out = length(temperature))

# 2. Create a time series object
temp_ts <- ts(temperature, start = c(2023, 1), frequency = 12)

# 3. Plot the time series data
plot(temp_ts, main = "Monthly Temperature Data", ylab = "Temperature", xlab = "Month", col = "blue")

# 4. Estimate ARIMA model parameters using MLE (this is the default method in auto.arima)
arima_fit_mle <- auto.arima(temp_ts, method = "ML")

# 5. Print the ARIMA model summary to see the estimated parameters
summary(arima_fit_mle)

# 6. Plot the diagnostics of the fitted ARIMA model
tsdiag(arima_fit_mle)

# 7. Forecast the next 6 months and plot the forecast
forecast_values <- forecast(arima_fit_mle, h = 6)
plot(forecast_values, main = "ARIMA Model Forecast for Temperature", col = "red")

#Compare different ARIMA models


# Load necessary libraries
library(forecast)
library(tseries)

# 1. Define the monthly sales data
sales <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210)
months <- seq(as.Date("2023-01-01"), by = "month", length.out = length(sales))

# 2. Create a time series object
sales_ts <- ts(sales, start = c(2023, 1), frequency = 12)

# 3. Plot the time series data
plot(sales_ts, main = "Monthly Sales Data", ylab = "Sales", xlab = "Month", col = "blue")

# 4. Fit ARIMA(1,1,1) model
arima_111 <- Arima(sales_ts, order = c(1, 1, 1))#(0,1,0)
summary(arima_111)

# 5. Fit ARIMA(2,1,2) model
arima_212 <- Arima(sales_ts, order = c(2, 1, 2))#(0,2,0)
summary(arima_212)

# 6. Compare the models using AIC and BIC
cat("AIC for ARIMA(1,1,1):", AIC(arima_111), "\n")
cat("BIC for ARIMA(1,1,1):", BIC(arima_111), "\n")
cat("AIC for ARIMA(2,1,2):", AIC(arima_212), "\n")
cat("BIC for ARIMA(2,1,2):", BIC(arima_212), "\n")

# 7. Plot the residuals for both models
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
tsdiag(arima_111, main = "Diagnostics for ARIMA(1,1,1)")
tsdiag(arima_212, main = "Diagnostics for ARIMA(2,1,2)")
par(mfrow = c(1, 1))  # Reset plotting grid

# 8. Forecast the next 6 months using both models and compare
forecast_111 <- forecast(arima_111, h = 6)
forecast_212 <- forecast(arima_212, h = 6)

# Plot the forecasts
par(mfrow = c(1, 2))
plot(forecast_111, main = "Forecast for ARIMA(1,1,1)", col = "red")
plot(forecast_212, main = "Forecast for ARIMA(2,1,2)", col = "green")
par(mfrow = c(1, 1))


#Estimate the parameters of an ARIMA model for the following time series data with outliers

# Load necessary libraries
library(forecast)
library(tseries)

# 1. Define the monthly sales data with outliers
sales_with_outliers <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 500)
months <- seq(as.Date("2023-01-01"), by = "month", length.out = length(sales_with_outliers))

# 2. Create a time series object
sales_ts_outliers <- ts(sales_with_outliers, start = c(2023, 1), frequency = 12)

# 3. Plot the time series data
plot(sales_ts_outliers, main = "Monthly Sales Data with Outliers", ylab = "Sales", xlab = "Month", col = "blue")

# 4. Estimate ARIMA model parameters using auto.arima (including the outlier)
arima_fit_outliers <- auto.arima(sales_ts_outliers)

# 5. Print the ARIMA model summary to see the estimated parameters
summary(arima_fit_outliers)

# 6. Plot the diagnostics of the fitted ARIMA model
tsdiag(arima_fit_outliers)

# 7. Forecast the next 6 months and plot the forecast
forecast_values_outliers <- forecast(arima_fit_outliers, h = 6)
plot(forecast_values_outliers, main = "ARIMA Model Forecast with Outliers", col = "red")

