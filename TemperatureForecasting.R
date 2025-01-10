library(astsa)
library(forecast)
library(fpp2)
library(tidyverse)
library(readxl)
library(zoo)

#Data Prep

Weather.data <- read_excel('3_LRArea.xlsx')
Weather.data <- zoo(Weather.data$MaxTemperature, order.by = Weather.data$Date)

temperatureTrain <- window(Weather.data, start = as.Date('1980-1-1'), end = as.Date('1997-12-31'))
temperatureTest <- window(Weather.data, start = as.Date('1998-1-1'), end = as.Date('1999-12-31'))
temperature <- window(Weather.data, start = as.Date('1980-1-1'), end = as.Date('1999-12-31'))

temperatureTS <- ts(temperature, frequency = 365)
temperatureTrainTS <- ts(temperatureTrain, start = c(1980,1), end = c(1997, 365), frequency = 365)
temperatureTestTS <- ts(temperatureTest, start = c(1998, 1), frequency = 365)

plot(temperatureTrainTS, main = "Weather Data", type = "o", pch = 20, ylab = "Temperature")

#Holt-Winters Additive

HWA <- HoltWinters(temperatureTrainTS, seasonal = "additive")
pHWA <- predict(HWA, 730)

act <- coredata(temperatureTest)
pHWAerr <- act - pHWA
(pHWAMAPE <- mean(abs(pHWAerr / act)))
(pHWARMSE <- sqrt(mean(pHWAerr^2)))

plot(temperatureTrainTS, ylab = "Temperature", xlim = c(1980, 2000), main = 'Additive Holt-Winters Forecast')
lines(pHWA, col = 'blue')

#Holt-Winters Multiplicative

HWM <- HoltWinters(temperatureTrainTS, seasonal = "multiplicative")
pHWM <- predict(HWM, 730)

pHWMerr <- act - pHWM
(pHWMMAPE <- mean(abs(pHWMerr / act)))
(pHWMRMSE <- sqrt(mean(pHWMerr^2)))

plot(temperatureTrainTS, ylab = "Temperature", xlim = c(1980, 2000), main = 'Multiplicative Holt-Winters Forecast')
lines(pHWM, col = 'red')


#Additive vs Multiplicative
plot(pHWA, ylab = "Temperature", col = 'blue', xlim = c(1998, 2000), main = 'Holt-Winters Forecasts vs Actual')
lines(pHWM, col = 'red')
lines(temperatureTestTS)

#SARIMA

acf(temperatureTS)
pacf(temperatureTS)
acf(diff(temperatureTS, lag = 365))
pacf(diff(temperatureTS, lag = 365))


#Based on a visual assessment of the ACF and PACF functions
SARIMA <- arima(temperatureTS, order = c(3, 0, 0), seasonal = list(order = c(0, 1, 0)))
SARIMA_forecast <- forecast(SARIMA, h = 730)

SARIMA_error <- accuracy(SARIMA_forecast$mean, temperatureTest)

plot(SARIMA_forecast, ylab = "Temperature", main = 'SARIMA Forecast')

#AutoSARIMA

#This is what auto.arima chose, setting it manually to save runtime
aSARIMA <- arima(temperatureTS, order = c(5, 0, 0), seasonal = list(order = c(0, 1, 0))) 
aSARIMA_forecast <- forecast(aSARIMA, h = 730)

aSARIMA_error <- accuracy(aSARIMA_forecast$mean, temperatureTest)

plot(aSARIMA_forecast, ylab = "Temperature", main = 'Auto SARIMA Forecast')
