# Clenow's Momentum ranking methodology from the book "Stocks on the Move"

### -- Libraries
library(zoo)
library(quantmod)
library(TTR)
library(tidyverse)

### -- Select tickers
ticker <- c("AMD", "NVDA", "AMZN")

### -- Create empty dataframe to store results
clenow_scanner <- data.frame()

### -- Function to get data and perform ranking
for (ticker in ticker){
### -- Get data with quantmod
getTickerData <- function(x){
    getSymbols(x, src = "yahoo", from = Sys.Date() - 400, to = Sys.Date(), auto.assign = FALSE)}
stock_data <- map(ticker, getTickerData) %>% map(Ad) %>% reduce(merge.xts)
stock_data<- as.data.frame(stock_data)
names(stock_data) <- ticker

### -- Calculate momentum rank
stock_data <- stock_data %>%
    as.data.frame() %>%
    mutate(ln_price = log(stock_data)) %>%
    mutate(slope_90 = rollapply(ln_price, width = 90,
                         FUN = function(x) lm(x ~ seq_along(x))$coefficients[2],
                         align = "right", fill = NA)) %>%
    mutate(annualized_slope = ((exp(slope_90)^250))-1)%>%
    mutate(rolling_rsq = rollapply(annualized_slope,
                                   width = 90, FUN = function(x) summary(lm(x ~ seq_along(x)))$r.squared,
                                   align = "right", fill = NA))%>%
    mutate(adjusted_slope = rolling_rsq * annualized_slope)%>%
    select(adjusted_slope) %>%
    slice_tail(n = 1)
x <- as.data.frame(stock_data$adjusted_slope)
colnames(x) <- "adjusted_slope"
row.names(x) <- ticker

clenow_scanner <- bind_rows(x, clenow_scanner)
}
print(clenow_scanner)
