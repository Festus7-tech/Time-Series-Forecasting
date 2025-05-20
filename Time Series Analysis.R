#Analysing the Time series model with inbuilt data
# Load a built-in dataset
data("AirPassengers")
AP <- AirPassengers
#checking the structure if the dataset
str(AP)
#Visualize the time series data to check the seasonality
plot(AP, main="AirPassengers Time Series", ylab="Passengers", xlab="Year",col='orange')
# Decompose time series to trends, seasonal components
decomposed <- decompose(AP)
plot(decomposed) # visiualize decompose values
#Check stationarity using Augmented Dickey-Fuller Tests
library(tseries) # Time series and Computional finance
# Perform the ADF test
adf.test(AP)
# Plot ACF and PACF
acf(AP) #Autocorrelations
pacf(AP) #PArtial-Autocorrelation
#Time Series Forecasting with ARIMA
library(forecast) #for forecasting
# Fit ARIMA model
fit <- auto.arima(AP)

# Forecast future values
forecasted <- forecast(fit, h=12)  # Forecast 12 months ahead

# Plot forecast
plot(forecasted)
#Exponential smoothing-ETS
# Fit ETS model(Error,Trend,Seasonality)
ets_fit <- ets(AP)
# Forecasting the smoothing using 12 periods 
ets_forecast <- forecast(ets_fit, h=12)
plot(ets_forecast)
#Seasonal Decomposition of Time series-STL
#STL decomposition,Seasonal-Trend decomposition using LOESS
stl_fit <- stl(AP, s.window="periodic")
plot(stl_fit)

# Forecasting after STL decomposition
stl_forecast <- forecast(stl_fit, h=12) # using 12 periods 
plot(stl_forecast)
#Evaluating the Forecast accuracy

accuracy(forecasted) # Accuracy of the forecast
# Cross-validation for ARIMA
library(forecast) # performing the time series forecasts
# Define a function for forecasting using ARIMA
arima_forecast <- function(y, h) {
  forecast(auto.arima(y), h=h)
}

# Perform time series cross-validation
cv_errors <- tsCV(AirPassengers, arima_forecast, h=1)
summary(cv_errors) # summary of the errors
# Calculate the Mean Squared Error (MSE)
mse <- mean(cv_errors^2, na.rm=TRUE)
print(mse) # display of the mean squared error
###
#installing the polars library that is not in CRAN
install.packages("tidypolars", repos = "https://community.r-multiverse.org")
library(polars) # data manipulation and data processing
library(tidyverse)# key data transformation functions
library(timetk) # time series analysis and forecasting
#load library for quantitative analysis
library(quantmod) # financial modelling
# Download stock data for a company (e.g., Apple Inc., AAPL)
getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# View the first few rows of the stock data
head(AAPL)
#load data for retrieving,manipulating finacial data
library(tidyquant) # scaling financial data
# Plot the stock prices
chartSeries(AAPL) 
# 20-day and 50-day Simple moving averages on Closing Prices
AAPL$SMA20 <- SMA(Cl(AAPL), n = 20) # 20-day moving average
AAPL$SMA50 <- SMA(Cl(AAPL), n = 50) # 50-day moving average

# Plot the moving averages for Technical Analysis
chartSeries(AAPL, TA = "addSMA(20);addSMA(50)")
#Calculating the Technical Analysis on Closing Prices
# RSI indicator over past 14-periods
AAPL$RSI <- RSI(Cl(AAPL), n = 14) 
# Plot RSI
chartSeries(AAPL, TA = "addRSI()") # using closing prices

# Bollinger Bands
chartSeries(AAPL, TA = "addBBands()")
#Using the Tidyverse approach
# Download and convert stock data to a tibble
aapl_data <- tq_get("AAPL", from = "2020-01-01")

# View the tibble format
head(aapl_data) # first few rows of data

# Plot the closing prices with ggplot2
library(ggplot2) # Graphical representation
#Generate a plot
ggplot(aapl_data, aes(x = date, y = close)) +
  geom_line() +
  labs(title = "AAPL Stock Closing Prices", x = "Date", y = "Closing Price")
#STock Forecasting
# GARCH modeling
library(rugarch) # load the pacakage
# calculating returns
library(PerformanceAnalytics)
#Import GOOGLE data from Yahoo
GOOGLE = getSymbols('GOOG', from='2005-02-07', to='2005-07-08',auto.assign = FALSE)
#Omitting the missing values
GOOGLE = na.omit(GOOGLE)
head(GOOGLE) # first few rows
# Select the relevant close price series
stock_prices = GOOGLE[,4] # Extract the forth column 
head(stock_prices)
#Decomposition, Regulirisation of time-space time series
library(pastecs)
#Descriptive statistics
summary(stock_prices)
# Generate a table with several key statistical measures
stat.desc(stock_prices)
#Visualize time series
chartSeries(GOOGLE, type = "bars", theme="white",main="Google Stock")
# Time series pattern
plot(stock_prices, type='l', col=4, main="Time series Plot of price Google", xlab='Date: from February 7, 2005 to July 23, 2005', ylab='Stock')  
library(ggplot2) #graphical representation
# Plot the stock prices with autoplot
autoplot(stock_prices,main="Google stock")
# Load the package
library(tseries) # Time sseries analysis
#Stationary test of Google stock
adf.test(stock_prices) #with Augmented -Dickey Fuller test
#Identify non-stationary time series
acf(stock_prices) # Autocorrelation function
# Calculate the log returns of the stock prices using 
# the "logarithm method
google_return <- CalculateReturns(stock_prices, method = "log")

# Remove any missing values (NA) from the calculated returns.
google_return <- na.omit(google_return)
#First few rows of data
head(google_return)
#Plot from transformed data
plot(google_return,main='Google return', xlab='Date', ylab='Log(Return)')
# Time Series Differencing
diff_price = diff(stock_prices) #differencing
diff_price = na.omit(diff_price) # omit missing values
plot(diff_price, type="l",main="1st order diff of google stock",ylab="Price Differences",xlab="Days")
