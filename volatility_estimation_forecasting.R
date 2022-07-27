# Volatility Trading Analysis in R | Andrew Mack @gingfacekillah

# Using a number of methods from Euan Sinclair's book "Volatility Trading" to estimate historical
# volatility and then using this estimate to forecast volatility looking forward.

# Load Libraries
library(quantmod)
library(TTR)
library(forecast)
library(PerformanceAnalytics)
library(rugarch)
library(Quandl)

# Dataset is a stock ticker OHLC + volume, can add VIX close too if desired
data <- AMD # downloaded from yahoo finance
data <- xts(data[, -1], order.by=as.POSIXct(data$Date))  #xts acceptable date classes include: 'Date', 'POSIXct' & 'timeDate
ohlc <- data[,c("Open","High","Low","Close")]

#### Part A: Historical volatility estimation methods
## 1. Close to Close Estimation
hvcc <- volatility(ohlc,calc='close',n=5,N=252)
plot(hvcc,main='Close to Close Estimation')

## 2. Parkinson Estimation
hvp <- TTR::volatility(ohlc,calc='parkinson',n=5,N=252)
hvpe <- hvcc^2/hvp^2 # Parkinson Efficiency
max(na.exclude(hvpe))
mean(na.exclude(hvpe))
plot(cbind(hvcc,hvp),main='Parkinson Estimation')

## 3. Garman-Klass Estimation
hvgk <- volatility(ohlc,calc='garman.klass',n=5,N=252)
hvgke <- hvcc^2/hvgk^2
max(na.exclude(hvgke))
mean(na.exclude(hvgke))
plot(cbind(hvcc,hvgk),main='Garman-Klass Estimation')

## 4. Rogers-Satchell Estimation
hvrs <- volatility(ohlc,calc='rogers.satchell',n=5,N=252)
hvrse <- hvcc^2/hvrs^2
max(na.exclude(hvrse))
mean(na.exclude(hvrse))
plot(cbind(hvcc,hvrs),main='Rogers-Satchell Estimation')

## 5. Garman-Klass-Yang-Zhang Estimation
hvgkyz <- volatility(ohlc,calc='gk.yz',n=5,N=252)
hvgkyze <- hvcc^2/hvgkyz^2
max(na.exclude(hvgkyze))
mean(na.exclude(hvgkyze))
plot(cbind(hvcc,hvgkyz),main='Garman-Klass-Yang-Zhang Estimation')

## 6. Yang-Zhang Estimation
hvyz <- volatility(ohlc,calc='yang.zhang',n=5,N=252)
hvyze <- hvcc^2/hvyz^2
max(na.exclude(hvyze))
mean(na.exclude(hvyze))
plot(cbind(hvcc,hvyz),main='Yang-Zhang Estimation')

## Historical Volatility Estimation Efficiency Comparison
hve <- cbind(hvpe,hvgke,hvrse,hvgkyze,hvyze)
colnames(hve) <- c('parkinson','garman_klass','rogers_satchell','gk_yang_zhang','yang_zhang')
max_efficiency <- apply(na.exclude(hve),2,max)
mean_efficiency <- apply(na.exclude(hve),2,mean)
max_efficiency
mean_efficiency


## Part B: Historical Volatility Forecasting
hvcct <- hvcc['2021-07-26::2022-05-09'] # training data
hvccf <- hvcc['2022-05-10::2022-07-26'] # testing data

## 1. ARIMA Forecast
harimam <- Arima(ts(hvcct,frequency=5),order=c(0,0,0),seasonal=c(0,1,0),include.constant=T)
harimaf <- Arima(ts(hvccf,frequency=5),model=harimam)$fitted
harimaf <- xts(harimaf,order.by=as.Date(index(hvccf)))
plot(cbind(hvccf,harimaf),main="ARIMA Forecast")
