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

## 2. GARCH Forecast
hgarchs <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model="std")
hgarchm <- ugarchfit(spec=hgarchs,data=hvcct,k=5,out.sample=length(hvccf))
hgarchp <- ugarchforecast(hgarchm,n.ahead=1,n.roll=length(hvccf),out.sample=length(hvccf))
hgarchf <- xts(hgarchp@forecast$sigmaFor[2:(length(hvccf)+1)]*sqrt(252),order.by=as.Date(index(hvccf)))
plot(cbind(hvccf,hgarchf),main="GARCH Forecast")

## 3. ARIMA-GARCH Forecast
harimagarchs <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0),include.mean=T),distribution.model="norm")
harimagarchm <- ugarchfit(spec=harimagarchs,data=na.exclude(diff(hvcc,k=21)),out.sample=length(hvccf))
harimagarchf <- xts(harimagarchm@model$pars[1]+lag(hvccf,k=21),order.by=as.Date(index(hvccf)))
plot(cbind(hvccf,harimagarchf),main="ARIMA-GARCH Forecast")

## Implied Volatility through the VIX
hvixf <- lag(hdata[,5]/100,k=21)['2017-01-03::2018-06-29']
plot(cbind(hvccf,hvixf),main="VIX Implied Volatility")

## Historical and Implied Volatility Forecasting Accuracy
hf <- cbind(hvccf,hsrwf,hmeanf,hsmaf,hemaf,harimaf,hgarchf,harimagarchf,hvixf)
colnames(hf) <- c('hvccf','hsrwf','hmeanf','hsmaf','hemaf','harimaf','hgarchf','harimagarchf','hvixf')
hf <- ts(na.exclude(coredata(hf)))
hvccfts <- hf[,1]
hsrwfts <- hf[,2]
hmeanfts <- hf[,3]
hsmafts <- hf[,4]
hemafts <- hf[,5]
harimafts <- hf[,6]
hgarchfts <- hf[,7]
harimagarchfts <- hf[,8]
hvixfts <- hf[,9]
accuracy(hsrwfts,hvccfts)
accuracy(hmeanfts,hvccfts)
accuracy(hsmafts,hvccfts)
accuracy(hemafts,hvccfts)
accuracy(harimafts,hvccfts)
accuracy(hgarchfts,hvccfts)
accuracy(harimagarchfts,hvccfts)
accuracy(hvixfts,hvccfts)






library(lubridate); require("pbapply") # progress bar!
ticker <- "SPY"
stock <- getSymbols(ticker, auto.assign = FALSE)
tmp <- getQuote(ticker)
stock <- rbind(stock, xts(cbind(tmp$Open, tmp$High, tmp$Low, tmp$Last, tmp$Volume, tmp$Last), order.by = Sys.Date()))
tmp <- Ad(stock)
rets <- ROC(tmp, type = "discrete")
rets[is.na(rets)] <-0
mean(rets)
sd(rets)

stk_ret <- function(STK_PRC, N, MEAN, STDEV)
  {
  delta_t = 1/N # For 1 period
  for (i in seq(N))
  {
    epsilon <- runif(n=1, min = 0, max =1) # random probabilities
    STK_PRC <- STK_PRC * (1 + qnorm(epsilon, MEAN * delta_t, STDEV * sqrt(delta_t)))
  }
  
  STK_PRC
}

last(tmp)
simulations <- 1000
N = 20
STK_PRC <- as.numeric(coredata(tmp[Sys.Date() - days(20)]))
MEAN = mean(rets)
STDEV = sd(rets)
# Run the simulation
stock_prices <- c() # empty vector to store prices
for(i in seq(simulations))
  {
  stock_prices <- c(stock_prices,stk_ret(STK_PRC = STK_PRC, N=N, MEAN=MEAN, STDEV=STDEV))
}

quantile(stock_prices)

# Options expiry
EXPIRY <- tmp[options.expiry(tmp)]
EXPIRY <- EXPIRY["2007::"]
IDX <- index(EXPIRY)
NEXT.EXPIRY <- as.Date("2020-06-19")
IDX <- c(IDX, NEXT.EXPIRY)

MEAN = function(calculateUNTIL){
  tmp <- tmp[paste0("::", calculateUNTIL)]
  tmp <- ROC(tmp, type = "discrete")
  tmp[is.na(tmp)] <- 0
  mean(tmp)
}

STDEV = function(calculateUNTIL){
  tmp <- tmp[paste0("::", calculateUNTIL)]
  tmp <- ROC(tmp, type = "discrete")
  tmp[is.na(tmp)] <- 0
  sd(tmp)
}

means <- do.call(rbind, lapply(as.list(IDX), MEAN))
stdevs <- do.call(rbind, lapply(as.list(IDX), STDEV))
days = as.numeric(diff(IDX))


MONTE.CARLO <- function(sim, iter, LastIter){
  simulations <- sim
  N <- days[iter]
  STK_PRC <- as.numeric(EXPIRY[iter])
  MEAN <- means[iter]
  STDEV <- stdevs[iter]
  
  for(i in seq(simulations))
  {
    stock_prices <- c(stock_prices, stk_ret(STK_PRC = STK_PRC, N=N, MEAN=MEAN, STDEV=STDEV))
  }
  
  START <- as.data.frame(round(STK_PRC,2))
  START.DATE <- index(EXPIRY[iter])
  PROBS <- as.data.frame(t(round(quantile(stock_prices, probs = seq(0,1,0.05)),2)))
  
  if(iter == LastIter){
    
    END <- as.data.frame(NA)
    END.DATE <- as.data.frame(NA)}else{
      END <- as.data.frame(as.numeric(round(EXPIRY[iter+1],2)))
      END.DATE <- index(EXPIRY[iter+1])
    }
    all <- cbind(START, START.DATE, PROBS, END, END.DATE)
    colnames(all) <- c("START.PRC", "START.DATE", "0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%",
                       "40%", "45%", "50%", "55%", "60%", "65%", "70%", "75%", "80%", "85%", "90%", "95%",
                       "100%", "END.PRC", "END.DATE")
    all
  }
  
p <- pblapply(as.list(1:length(days)), function(x){
  MONTE.CARLO(sim=10000, iter = x, LastIter = length(days))
})
p <- do.call(rbind,p)

plot(p$END.PRC, type="l")
lines(p$`0%`, col='red')
lines(p$`100%`,col='green')

# number of months
nMo <- nrow(p)

# numbers of times it closes above 100%
sum(as.numeric(na.omit(ifelse(p$END.PRC > p$`100%`,1,0))))/nMo

# numbers of times it closes below 0%
sum(as.numeric(na.omit(ifelse(p$END.PRC < p$`0%`,1,0))))/nMo

write.csv(p,"SPY.csv")

