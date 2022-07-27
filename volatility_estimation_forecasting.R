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


#### Part B: Historical Volatility Forecasting with GARCH models from: https://rpubs.com/silasselfe/garch
data <-data$`Adj Close`
chartSeries(data)

return <- CalculateReturns(data) # Histogram of daily returns
return <- return[-1]
hist(return)

chart.Histogram(return, # Nicer histogram comparing return distributions to a norm.dist
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))

chartSeries(return) # plot returns now that we have them relatively stationary
chart.RollingPerformance(R = return, # Annualized volatility
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "AMD's rolling 1 year volatility")

# GARCH Models
# 1. sGARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
sgarch <- ugarchfit(data = return, spec = s)
sgarch
plot(sgarch, which = 'all') # Detailed results plot
f_sgarch <- ugarchforecast(fitORspec = m, n.ahead = 5) # Forecast
plot(fitted(f_sgarch))

# 2. GARCH with sstd
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
sstd_garch <- ugarchfit(data = return, spec = s)
sstd_garch
plot(sstd_garch, which = 'all') # Detailed results plot
f_sstd_garch <- ugarchforecast(fitORspec = sstd_garch, n.ahead = 5) # Forecast
plot(fitted(f_sstd_garch))

# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
gjr_garch<- ugarchfit(data = return, spec = s)
gjr_garch
plot(gjr_garch, which = 'all') # Detailed results plot
f_gjr_garch <- ugarchforecast(fitORspec = gjr_garch, n.ahead = 5) # Forecast
plot(fitted(f_gjr_garch))

# 4. AR(1) GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
ar_garch <- ugarchfit(data = return, spec = s)
ar_garch
plot(ar_garch, which = 'all') # Detailed results plot
f_ar_garch <- ugarchforecast(fitORspec = ar_garch, n.ahead = 5) # Forecast
plot(fitted(f_ar_garch))

# 5. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm =T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
gjrm_garch <- ugarchfit(data = return, spec = s)
gjrm_garch
plot(gjrm_garch, which = 'all') # Detailed results plot
f_gjrm_garch <- ugarchforecast(fitORspec = gjrm_garch, n.ahead = 5) # Forecast
plot(fitted(f_gjrm_garch))


# GARCH Simulation
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
sfinal <- s
setfixed(sfinal) <- as.list(coef(m))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*30,
                  rseed = 123)
tail(data,1)

p <- 87.54*apply(fitted(sim), 2, 'cumsum') + 87.54
matplot(p, type = "l", lwd = 3)