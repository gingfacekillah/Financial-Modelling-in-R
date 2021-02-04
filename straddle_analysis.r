#yahooQF() Great list of possible data pulls!
#getQuote(tickers)

## ** Work in progress ** ##
##########################################################################
#### 1. Get a list of all optionable stock tickers from BarCharts.com ####
require("httr");require("dplyr");require("purrr")

# Attach page URL for BarChart.com
pg <- html_session("https://www.barchart.com/options/stocks-by-sector?page=1")

# Save URL cookies
cookies <- pg$response$cookies

# Use a named character vector for unquote splicing with !!!
token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                           cookies$name)))
# Scrape ticker names with RVest
pg <- 
  pg %>% rvest:::request_GET(
    paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?lists=",
           "stocks.optionable.by_sector.all.us&",
           "fields=symbol%2CsymbolName%2ClastPrice%2CpriceChange%2CpercentChange",
           "%2ChighPrice%2ClowPrice%2Cvolume%2CtradeTime%2CsymbolCode%2CsymbolType",
           "%2ChasOptions&orderBy=symbol&orderDir=asc&meta=field.shortName",
           "%2Cfield.type%2Cfield.description&hasOptions=true&page=1",
           "&limit=1000000&raw=1"),
    config = httr::add_headers(`x-xsrf-token` = token)
  )

# Raw data
data_raw <- httr::content(pg$response)

# Convert raw data into a data frame
data <- 
  purrr::map_dfr(
    data_raw$data,
    function(x){
      as.data.frame(x$raw)
    }
  )
# Fix time structure 
data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")

# Extract ticker symbols
tickers <- data$symbol
tickers <- tickers[1:100]

# Remove data no longer needed to conserve computer memory
# rm(data)
# rm(pg)
# rm(data_raw)

########################################################################################
#### 2. Get historical close to close volatility for all tickers for last 12 months ####
require("quantmod");require("RQuantLib");require("lubridate"); require("BatchGetSymbols")
require("TTR"); require("pbapply"); require("data.table"); require("tidyquant")

# Get last 12 month OHLC data for each ticker
historicalData <- BatchGetSymbols::BatchGetSymbols(tickers,
                                 first.date = Sys.Date() -365,
                                 last.date = Sys.Date(),
                                 thresh.bad.data = 0.75,
                                 bench.ticker = "^GSPC",
                                 freq.data = 'daily',
                                 type.return = 'arit')
# Ticker output summary
historicalData$df.control

# Extract price data
df <- as.data.frame(historicalData$df.tickers)
#rm(historicalData)

# Modify column names for TTR package use
names(df)[1:6] <- c("price.Open", "price.High", "price.Low",
                    "price.Close", "Volume", "price.Adjusted")

# Extract closing price for each ticker, convert to time series data
price_Close <- pblapply(as.list(unique(df$ticker)), function(x){
  tmp = subset(df, df$ticker == x)
  id = tmp[,c("ref.date", "price.Close")]
  id = xts(id[,2], order.by = as.Date(id$ref.date, format = "%Y-%m-%d"))
  names(id) = x
  id
})

# Merge data and exclude NA's
price_Close <- do.call(merge,price_Close)
price_Close <- as.data.frame(price_Close)
price_Close <- price_Close[-c(252),] %>%
  select_if(~ !any(is.na(.)))

# Function to calculate historical annualized volatility for each ticker
hist_vol <- function(x){
  hvol <- volatility(x,calc='close',n=nrow(price_Close),N=nrow(price_Close))
  hvol
}

# Loop for all tickers & delete NA rows
historical_vol <- na.omit(sapply(price_Close, hist_vol))

# Transform from rows to columns
final_vol<- unlist(historical_vol[1,])
final_vol <- as.data.frame(final_vol)
#View(final_vol)
x = rownames(final_vol) # look into this, "final_vol" was previously used, maybe "price_Close"
############################################################################################
#### 3. Calculate implied vol for 30 DTE call and put ATM and average to get average IV ####
require("RQuantLib"); require("quantmod"); require("lubridate"); require("jsonlite"); require("svMisc")

# Create dummy variables for final dataframe
#final_vol$ticker <- rownames(final_vol)
#rownames(final_vol) <- NULL
final_vol$underlying_last <- rep(1, nrow(final_vol))
final_vol$call_strike <- rep(1, nrow(final_vol))
final_vol$put_strike <- rep(1, nrow(final_vol))
final_vol$dividend_yield <- rep(1, nrow(final_vol))
final_vol$risk_freeRate <- rep(1, nrow(final_vol))
final_vol$dte <- rep(1, nrow(final_vol))
final_vol$call_price <- rep(1, nrow(final_vol))
final_vol$put_price <- rep(1, nrow(final_vol))
final_vol$call_implied <- rep(1, nrow(final_vol))
final_vol$put_implied <- rep(1, nrow(final_vol))
final_vol$implied_avg <- rep(1, nrow(final_vol))
final_vol$log_diff <- rep(1, nrow(final_vol))

# Underlying last price
underlying_last <- (price_Close[nrow(price_Close),]) # this number needs to adjust to changing row #
final_vol$underlying_last <- unlist(underlying_last[1,])

# Days to expiration
expiration = as.Date("2021-02-05") # collect the 30 DTE date here, parsed from options chain
TODAY = Sys.Date()
days2exp = as.numeric(expiration - TODAY)
exp = yearFraction(startDates = TODAY, endDates = expiration, dayCounters = 1)
final_vol$dte <- exp

# Risk free rate
getSymbols.FRED("DGS10", env = .GlobalEnv)
treas = na.locf(DGS10)/100/360
final_vol$risk_freeRate = sum(rep(last(treas), days2exp))

# Collect dividend yield data
opt_divYield <- function(x){
  q = yahooQF(c("Dividend Yield"))
  div = getQuote(x, what = q)
  div$'Dividend Yield'
}

# Dividend yield - NA's replaced with zero
divY <- pblapply(x, opt_divYield)
DivYield <- unlist(sapply(divY, function(x) x[1]))
DivYield[is.na(DivYield)] = 0

# Update final_vol data frame with pulled dividend yield values
for (i in (x)){
  final_vol[x, 'dividend_yield'] <- DivYield
}

# Get options chain data for all tickers
price = getQuote(x)
chains = pblapply(x, getOptionChain)

# Call ATM strikes
calls = pblapply(chains, function(x) x$calls)
call_strike_list <- Map(function(cl, p) cl[which.min(abs(p - cl$Strike)), ], calls, price$Last)

# Put ATM strikes
puts = pblapply(chains, function(x) x$puts)
put_strike_list <- Map(function(cl, p) cl[which.min(abs(p - cl$Strike)), ], puts, price$Last)

# Collect call data
call_strike_list[sapply(call_strike_list, function(x) length(x)==0L)] <- NA
call_strikes <- unlist(sapply(call_strike_list, function(x) x[1]))
call_price <- unlist(sapply(call_strike_list, function(x) x[2]))
call_implied_vol <- unlist(sapply(call_strike_list, function(x) x[9]))

# Collect put data
put_strike_list[sapply(put_strike_list, function(x) length(x)==0L)] <- NA
put_strikes <- unlist(sapply(put_strike_list, function(x) x[1]))
put_price <- unlist(sapply(put_strike_list, function(x) x[2]))
put_implied_vol <- unlist(sapply(put_strike_list, function(x) x[9]))

## Add call data to final_vol data frame ##
# Call strikes
for (i in (x)){
  final_vol[x, 'call_strike'] <- call_strikes
}
# Call prices
for (i in (x)){
  final_vol[x, 'call_price'] <- call_price
}

# Call implied volatility
for (i in (x)){
  final_vol[x, 'call_implied'] <- call_implied_vol
}

## Add put data to final_vol data frame ##
# Put strikes
for (i in (x)){
  final_vol[x, 'put_strike'] <- call_strikes
}
# Put prices
for (i in (x)){
  final_vol[x, 'put_price'] <- put_price
}

# Put implied volatility
for (i in (x)){
  final_vol[x, 'put_implied'] <- put_implied_vol
}

# Remove NA rows from final_vol data frame
final_vol <- drop_na(final_vol)


# # Call implied volatility # generalize this and sapply through each row
# final_vol$call_implied <- as.numeric(
#   AmericanOptionImpliedVolatility(type = "call",
#                                   underlying = final_vol$underlying_last,
#                                   strike = final_vol$strike,
#                                   dividendYield = final_vol$dividend_yield,
#                                   riskFreeRate = final_vol$risk_freeRate,
#                                   maturity = final_vol$dte,
#                                   volatility = 0.5,
#                                   value = final_vol$call_price))
# 
# # Put implied volatility # generalize this and sapply through each row
# final_vol$put_implied <- as.numeric(
#   AmericanOptionImpliedVolatility(type = "put",
#                                   underlying = underlying,
#                                   strike = strike,
#                                   dividendYield = divY,
#                                   riskFreeRate = rF,
#                                   maturity = exp,
#                                   volatility = vol,
#                                   value = 3.92))

# Average call & put implied volatility
final_vol$implied_avg = round(((final_vol$call_implied + final_vol$put_implied) /2),4)

######################################################
#### 4. Compare log differences between HV and IV ####

# Calculate log difference between historical and implied volatility
final_vol$log_diff <- round((log(final_vol$final_vol) - log(final_vol$implied_avg)),3)

# Rank & sort tickers into a list of highest and lowest log differences
final_vol <- final_vol[order(-final_vol$log_diff),]
View(final_vol)

