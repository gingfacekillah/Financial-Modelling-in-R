### A stock scanner roughly based on the work of Kristjan Kullamägi

library(tidyverse)
library(quantmod)
library(TTR)
library(zoo)

# Read in vector of tickers
# weekly_tickers <- read.csv("weeklyTickers.csv") # Load your tickers as a vector
weekly_tickers <- c("AAPL", "AMD", "NVDA", "AMZN") # Example

# Get today's date and calculate the start date (400 days back)
end_date <- Sys.Date()
start_date <- end_date - 400

# Function to get OHLC data for a single ticker
getSymbols(weekly_tickers, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
list_data <- mget(weekly_tickers)

# Keep tickers and list_data, remove everything else
objects_to_keep <- c("weekly_tickers", "list_data")
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)
rm(all_objects)
rm(objects_to_remove)

# ADR20 - The average daily range for the past 20 days
calculate_ADR <- function(ticker_data) {
    # Extract high and low prices
    highs <- ticker_data[, 2]
    lows <- ticker_data[, 3]

    # Calculate the ratio of high to low prices
    ratio_high_low <- highs / lows

    # Calculate the 20-period SMA of the ratio, then convert to ADR%
    adr_20 <- 100 * (SMA(ratio_high_low, n = 20) - 1)

    return(tail(adr_20, 1))
}
adr_values_list <- sapply(list_data, calculate_ADR)
adr <- data.frame(adr_values_list)
adr$ticker <- rownames(adr)
adr <- adr %>% rename(ADR_20 = adr_values_list)

# ACF - autocorrelation of returns
calc_ACF <- function(ticker_data, window_size) {
    # Extract the 6th column (Adjusted Close prices)
    adj_close_prices <- ticker_data[, 6]

    # Calculate daily log returns on the Adjusted Close prices
    prices_log <- dailyReturn(adj_close_prices, type = "log")

    # Calculate ACF on the log returns
    acf_result <- acf(prices_log, lag.max = window_size, plot = FALSE)

    # Return the ACF values
    return(acf_result$acf)
}
acf_last_values <- sapply(list_data, function(x) tail(calc_ACF(x, window_size = 252), 1))
acf <- data.frame(acf_last_values)
acf$ticker <- rownames(acf)
acf <- acf %>% rename(ACF_252 = acf_last_values)

# Clenow Momentum
clenow_momentum <- function(ticker_data) {
    # Extract the Adjusted Close prices (column 6)
    adjusted_close <- ticker_data[, 6]

    # Calculate the natural logarithm of the adjusted close prices
    ln_price <- log(adjusted_close)

    # Calculate the 100-day slope of the log prices
    slope_100 <- rollapply(ln_price, width = 100,
                          FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
                          align = "right", fill = NA)

    # Calculate the annualized slope
    annualized_slope <- (exp(slope_100) ^ 250) - 1

    # Calculate the rolling R-squared over the annualized slope
    rolling_rsq <- rollapply(annualized_slope, width = 100,
                             FUN = function(x) summary(lm(x ~ seq_along(x)))$r.squared,
                             align = "right", fill = NA)

    # Calculate the adjusted slope
    adjusted_slope <- rolling_rsq * annualized_slope

    # Extract the last value of the adjusted slope
    last_adjusted_slope <- tail(adjusted_slope, 1)

    # Return the last adjusted slope with the ticker label
    return(last_adjusted_slope)
}
clenow <- sapply(list_data, clenow_momentum)
clenow <- data.frame(clenow)
clenow$ticker <- rownames(clenow)

# Dollar Volume (Volume * Price)
volume_price_last <- function(ticker_data) {
    last_volume <- tail(ticker_data[, 5], 1)
    last_price <- tail(ticker_data[, 6], 1)
    return(last_volume * last_price)
}
volumePrice <- sapply(list_data, volume_price_last)
volumePrice <- data.frame(volumePrice)
volumePrice$ticker <- rownames(volumePrice)
volumePrice <- volumePrice %>% rename(dollarVolume = volumePrice)

# Last Price
price_last <- function(ticker_data) {
    price <- tail(ticker_data[, 6], 1)
    return(price)
}
priceLast <- sapply(list_data, price_last)
priceLast <- data.frame(priceLast)
priceLast$ticker <- rownames(priceLast)

# 1M returns
return_1M <- function(ticker_data){
    end_price <- as.numeric(tail(ticker_data[, 6], n = 1))
    min_price <- min(as.numeric(tail(ticker_data[, 6], n = 22)))
    # Calculate return
    output <- (end_price / min_price) - 1

    return(output)
}
return1M <- sapply(list_data, return_1M)
return1M <- data.frame(return1M)
return1M$ticker <- rownames(return1M)

# 3M returns
return_3M <- function(ticker_data){
    end_price <- as.numeric(tail(ticker_data[, 6], n = 1))
    min_price <- min(as.numeric(tail(ticker_data[, 6], n = 67)))
    # Calculate return
    output <- (end_price / min_price) - 1

    return(output)
}
return3M <- sapply(list_data, return_3M)
return3M <- data.frame(return3M)
return3M$ticker <- rownames(return3M)

# 6M returns
return_6M <- function(ticker_data){
    end_price <- as.numeric(tail(ticker_data[, 6], n = 1))
    min_price <- min(as.numeric(tail(ticker_data[, 6], n = 126)))
    # Calculate return
    output <- (end_price / min_price) - 1

    return(output)
}
return6M <- sapply(list_data, return_6M)
return6M <- data.frame(return6M)
return6M$ticker <- rownames(return6M)

# Join all dataframes together
dataframes_list <- list(adr, acf, clenow, volumePrice, priceLast, return1M, return3M, return6M)
joined_df <- reduce(dataframes_list, left_join, by = "ticker")
joined_df <- joined_df %>% mutate(dollarVolume = dollarVolume / 1e6)
print(joined_df)

# Qullamaggie Scanner
top_decile_1M <- quantile(joined_df$return1M, 0.9, na.rm = TRUE)
top_decile_3M <- quantile(joined_df$return3M, 0.9, na.rm = TRUE)
top_decile_6M <- quantile(joined_df$return6M, 0.9, na.rm = TRUE)

qullamaggie_scan <- joined_df %>%
    filter(dollarVolume > 5, ADR_20 > 5) %>% # More than 5 million in dollar volume, more than 5% ADR20
    filter(return1M >= top_decile_1M | return3M >= top_decile_3M | return6M >= top_decile_6M) # Looking for 1/3/6 month gainers

