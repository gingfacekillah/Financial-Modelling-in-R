# SP500 Sector Direction Analysis   | Andrew Mack @Gingfacekillah
# Libraries
library(rvest) # for scraping
library(tidyverse) # blanket import for core tidyverse packages
library(tidyquant) # tidy financial analysis 
library(janitor) # tidy data cleaning functions
library(quantmod) # just in case
library(ggplot2) # Plots
library(sjmisc) # rotate dataframes easily
library(matrixStats) # row medians and other functions
library(pbapply) # Progress bar
library(readr) # Export .csv
library(derivmkts) # option greeks

#### 1. Get sector symbols
tickers = c("SPY",  # s&p 500 index
            "XLC",  # communication services
            "XLY",  # consumer discretionary
            "XLP",  # consumer staples
            "XLE",  # energy
            "XLF",  # financials
            "XLV",  # healthcare
            "XLI",  # industrials
            "XLB",  # materials
            "XLRE", # real estate
            "XLK",  # technology
            "XLU",  # utilities
            "QQQ")  # nasdaq 100 index


#### 2. Setting number of months to scrape OHLC data
today <- Sys.Date()# Save current system date to a variable
date = today %m+% months(-12)# Subtract 12 months from the current date
print(date)
start_date <- as.Date(date)
end_date <- as.Date(today)

#### 3. Get OHLC data for each ticker in the ticker vector
getTickerData <- function(x){getSymbols(x,
                                        src = "yahoo",
                                        from = date,
                                        to = today,
                                        auto.assign = FALSE)}
prices_adj <- map(tickers, getTickerData) %>% map(Ad) %>% reduce(merge.xts) # Iterating through every ticker
names(prices_adj) <- tickers


#### 4. Calculate Historical Annualized Volatility ####
hist_vol <- function(x){
  hvol <- volatility(x,calc='close',n=7,N=nrow(prices_adj))
  hvol
}
historical_vol <- na.omit(sapply(prices_adj, hist_vol)) # Loop for all tickers & delete NA rows
historical_vol <- as.data.frame(historical_vol) # Make into a dataframe
historical_vol2 <- historical_vol %>% rotate_df() # Rotate new dataframe
historical_vol2$mean_vol <- rowMeans(historical_vol2) # Calculate row averages
historical_vol2 <- tibble::rownames_to_column(historical_vol2, "ticker") # Make tickers a separate column
vol_chart <- historical_vol2 %>% select(ticker,mean_vol) # New dataframe with tickers & mean volatility

# Get risk-free rate
# For a weekly forecast, risk-free rate is effectively zero but could also get it here
#getSymbols.FRED("DGS10", env=.GlobalEnv)
#DGS10 <- last(DGS10)/100/360
#days2exp <- 5

#### 5. Get implied volatility for each ticker 
# Create dummy variables for final dataframe
otickers <- vol_chart$ticker
master_volChart <- as.data.frame(otickers)
master_volChart <- rename(master_volChart, ticker = otickers)
master_volChart$underlying <- rep(1, nrow(master_volChart))
master_volChart$type <- rep("C", nrow(master_volChart))
master_volChart$call_strike <- rep(1, nrow(master_volChart))
master_volChart$call_dte <- rep(1, nrow(master_volChart))
master_volChart$call_risk_free <- rep(0, nrow(master_volChart)) # if actually calculating, use as.numeric((1+DGS10)^(days2exp)-1)
master_volChart$call_div_yield <- rep(0, nrow(master_volChart))
master_volChart$call_volume <- rep(1, nrow(master_volChart))
master_volChart$call_open_interest <- rep(1, nrow(master_volChart))
master_volChart$call_bid <- rep(1, nrow(master_volChart))
master_volChart$call_ask <- rep(1, nrow(master_volChart))
master_volChart$call_mid<- rep(1, nrow(master_volChart))
master_volChart$call_spread <- rep(1, nrow(master_volChart))
master_volChart$call_implied_vol <- rep(1, nrow(master_volChart))
master_volChart$call_mean_vol <- rep(1, nrow(master_volChart))
master_volChart$call_vol_premium <- rep(1, nrow(master_volChart))

# Get options chain data for all tickers
price = getQuote(otickers)
foo <- function(x){tryCatch(getOptionChain(x), error = function(e) e)} # supply expiration for non-weeklies
chains = pblapply(otickers, foo)

# Collect call strike closest to ATM
calls = pblapply(chains, function(x) x$calls)
call_strike_list <- Map(function(cl, p) cl[which.min(abs(p - cl$Strike)), ], calls, price$Last)

# Collect call data
call_strike_list[sapply(call_strike_list, function(x) length(x)==0L)] <- NA
call_strike <- unlist(sapply(call_strike_list, function(x) x[5]))
dte <- unlist(sapply(call_strike_list, function(x) x[4]))

# Get dividend yield for BSM calculation later
opt_divYield <- function(x){
  q = yahooQF(c("Dividend Yield"))
  div = getQuote(x, what = q)
  div$'Dividend Yield'
}

# Dividend yield - NA's replaced with zero
divY <- pblapply(otickers, opt_divYield)
DivYield <- unlist(sapply(divY, function(x) x[1]))
DivYield[is.na(DivYield)] = 0
divY <- unlist(divY)

# Get what we need from the call data list
volume <- unlist(sapply(call_strike_list, function(x) x[11]))
open_interest <- unlist(sapply(call_strike_list, function(x) x[12]))
call_bid <- unlist(sapply(call_strike_list, function(x) x[9]))
call_ask <- unlist(sapply(call_strike_list, function(x) x[10]))
#implied_vol <- unlist(sapply(call_strike_list, function(x) x[14])) # Calculate IV yourself. Yahoo finance IV data is garbage.

master_volChart$underlying <- price$Last
master_volChart$call_strike <- call_strike
master_volChart$call_dte <- (as.numeric(as.Date.POSIXct(dte)-today))/365
master_volChart$call_div_yield <- divY
master_volChart$call_volume <- volume
master_volChart$call_open_interest <- open_interest
master_volChart$call_bid <- call_bid
master_volChart$call_ask <- call_ask
master_volChart$call_mid <- (master_volChart$call_bid + master_volChart$call_ask)/2
master_volChart$call_spread <- abs(master_volChart$call_bid-master_volChart$call_ask)
master_volChart$call_mean_vol <- vol_chart$mean_vol

# Percentage function for vol_premium_pct column
percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

master_volChart$call_vol_premium_pct <- percent(master_volChart$call_vol_premium)
master_volChart <- drop_na(master_volChart) # Remove NA's

# Black-Scholes for calls
Black_Scholes <- function(S,K,Time,r,sigma,rho, type = "C") {
  d_1 <- (log(S/K) + (r- rho + (sigma^2/2))*Time) / (sigma*sqrt(Time))
  d_2 <- d_1 - (sigma*sqrt(Time))
  if(type == "P"){
    price <- (K*exp(-r*Time)*pnorm(-d_2,0,1)) - (S*exp(-rho*Time)*pnorm(-d_1,0,1)) 
  }
  if(type == "C") {
    price <- (S*exp(-rho*Time)*pnorm(d_1,0,1)) - (K*exp(-r*Time)*pnorm(d_2))
  }
  return(price)
}

# Implied volatility using Black-Scholes
impl_volatility <- function(S, K, Time, r, rho, price) {
  root <- function(sigma) {
    Black_Scholes(S,K,Time,r,sigma,rho) - price
  }
  
  uniroot(root, lower = -1000, upper = 1000, extendInt = "yes")$root
}

# Iterate IV estimation through master_volChart
result <- numeric() # Empty vector to save each iteration
iv_calc <- for (row in 1:nrow(master_volChart)){
  result <- c(result, impl_volatility(S=master_volChart[row, "underlying"],
                                      K=master_volChart[row, "call_strike"],
                                      Time = master_volChart[row, "call_dte"],
                                      r = master_volChart[row, "call_risk_free"],
                                      rho = master_volChart[row, "call_div_yield"],
                                      price = master_volChart[row, "call_mid"]))
  
  print(result)
}

# Take the final IV estimate iteration for each ticker in the list and store as a vector
iv_final <- sapply(result,tail,1)
iv_final <- unlist(iv_final)

# Add calculated IV into master_volChart
master_volChart$call_implied_vol <- iv_final
master_volChart$call_vol_premium <- (master_volChart$call_implied_vol - master_volChart$call_mean_vol)
master_volChart$call_vol_premium_pct <- percent(master_volChart$call_vol_premium)
master_volChart$call_vrp_edge <- (-master_volChart$call_vol_premium*100)

# Mean & median VRP
mean(master_volChart$call_vol_premium)
volMed <- median(master_volChart$call_vol_premium)
volMed
call_median_vrp <- volMed
call_avg_vrp <- mean(master_volChart$call_vol_premium)
master_volChart$call_relative_value <- percent(volMed-master_volChart$call_vol_premium)

# Calculate the options greeks for calls
S=master_volChart$underlying
X=master_volChart$call_strike
v = master_volChart$call_implied_vol
r = master_volChart$call_risk_free
Time = master_volChart$call_dte
b = master_volChart$call_div_yield
master_volChart$d1 <- (log(S/X) + (r - b + (v^2)/2)*Time ) / (v * sqrt(Time))
master_volChart$d2 <- master_volChart$d1 - v*(sqrt(Time))
master_volChart$call_delta <- exp( (b-r)*Time ) * pnorm(master_volChart$d1)
master_volChart$call_theta <- (-exp(-b*Time) * (S*dnorm(master_volChart$d1)*v)/(2*sqrt(Time)) -
                                 r * X * exp(-r*Time) * pnorm(master_volChart$d2))/1000 
master_volChart$call_gamma <- exp(-b*Time) * ( dnorm(master_volChart$d1)/( S*v*sqrt(Time)) )
master_volChart$call_vega  <- S * exp(-b*Time) * dnorm(master_volChart$d1) * sqrt(Time)/100
master_volChart$call_vanna <- -exp(-b*Time) * dnorm(master_volChart$d1) * (master_volChart$d2/v)
master_volChart$call_volga <- master_volChart$call_vega * ((master_volChart$d1*master_volChart$d2) / v)

# Calculate expected value
master_volChart$call_fair_price  <- ((master_volChart$call_vega*master_volChart$call_vrp_edge)+master_volChart$call_mid)
master_volChart$call_exp_value <- ((master_volChart$call_vega*master_volChart$call_vrp_edge)/master_volChart$call_mid)
# Save puts data
calls_master <- master_volChart

# Repeat process for puts
master_volChart <- as.data.frame(otickers)
master_volChart <- rename(master_volChart, ticker = otickers)
master_volChart$underlying <- rep(1, nrow(master_volChart))
master_volChart$type <- rep("P", nrow(master_volChart))
master_volChart$put_strike <- rep(1, nrow(master_volChart))
master_volChart$put_dte <- rep(0, nrow(master_volChart))
master_volChart$put_risk_free <- rep(0, nrow(master_volChart))
master_volChart$put_div_yield <- rep(0, nrow(master_volChart))
master_volChart$put_volume <- rep(1, nrow(master_volChart))
master_volChart$put_open_interest <- rep(1, nrow(master_volChart))
master_volChart$put_bid <- rep(1, nrow(master_volChart))
master_volChart$put_ask <- rep(1, nrow(master_volChart))
master_volChart$put_mid<- rep(1, nrow(master_volChart))
master_volChart$put_spread <- rep(1, nrow(master_volChart))
master_volChart$put_implied_vol <- rep(1, nrow(master_volChart))
master_volChart$put_mean_vol <- rep(1, nrow(master_volChart))
master_volChart$put_vol_premium <- rep(1, nrow(master_volChart))

# Collect put strike closest to ATM
puts = pblapply(chains, function(x) x$puts)
put_strike_list <- Map(function(cl, p) cl[which.min(abs(p - cl$Strike)), ], puts, price$Last)

# Collect put data
put_strike_list[sapply(put_strike_list, function(x) length(x)==0L)] <- NA
put_strike <- unlist(sapply(put_strike_list, function(x) x[5]))
dte <- unlist(sapply(put_strike_list, function(x) x[4]))

# Get what we need from the put data list
volume <- unlist(sapply(put_strike_list, function(x) x[11]))
open_interest <- unlist(sapply(put_strike_list, function(x) x[12]))
put_bid <- unlist(sapply(put_strike_list, function(x) x[9]))
put_ask <- unlist(sapply(put_strike_list, function(x) x[10]))
#implied_vol <- unlist(sapply(call_strike_list, function(x) x[14])) # Yahoo finace IV data is garbage

master_volChart$underlying <- price$Last
master_volChart$put_strike <- put_strike
master_volChart$put_dte <- (as.numeric(as.Date.POSIXct(dte)-today))/365
master_volChart$put_div_yield <- divY
master_volChart$put_volume <- volume
master_volChart$put_open_interest <- open_interest
master_volChart$put_bid <- put_bid
master_volChart$put_ask <- put_ask
master_volChart$put_mid <- (master_volChart$put_bid + master_volChart$put_ask)/2
master_volChart$put_spread <- abs(master_volChart$put_bid-master_volChart$put_ask)
master_volChart$put_mean_vol <- vol_chart$mean_vol

# Percentage function for vol_premium_pct column
percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
master_volChart$put_vol_premium_pct <- percent(master_volChart$put_vol_premium)
master_volChart <- drop_na(master_volChart)

# Black-Scholes for puts
Black_Scholes <- function(S,K,Time,r,sigma,rho, type = "P") {
  d_1 <- (log(S/K) + (r- rho + (sigma^2/2))*Time) / (sigma*sqrt(Time))
  d_2 <- d_1 - (sigma*sqrt(Time))
  if(type == "P"){
    price <- (K*exp(-r*Time)*pnorm(-d_2,0,1)) - (S*exp(-rho*Time)*pnorm(-d_1,0,1)) 
  }
  if(type == "C") {
    price <- (S*exp(-rho*Time)*pnorm(d_1,0,1)) - (K*exp(-r*Time)*pnorm(d_2))
  }
  return(price)
}

# Implied volatility using Black-Scholes
impl_volatility <- function(S, K, Time, r, rho, price) {
  root <- function(sigma) {
    Black_Scholes(S,K,Time,r,sigma,rho) - price
  }
  
  uniroot(root, lower = -1000, upper = 1000, extendInt = "yes")$root
}

# Iterate IV estimation through master_volChart
result <- numeric() # Empty vector to save each iteration
iv_calc <- for (row in 1:nrow(master_volChart)){
  result <- c(result, impl_volatility(S=master_volChart[row, "underlying"],
                                      K=master_volChart[row, "put_strike"],
                                      Time = master_volChart[row, "put_dte"],
                                      r = master_volChart[row, "put_risk_free"],
                                      rho = master_volChart[row, "put_div_yield"],
                                      price = master_volChart[row, "put_mid"]))
  
  print(result)
}
# Take the final IV estimate iteration for each ticker in the list and store as a vector
iv_final <- sapply(result,tail,1)
iv_final <- unlist(iv_final)

# Add calculated IV into master_volChart
master_volChart$put_implied_vol <- iv_final
master_volChart$put_vol_premium <- (master_volChart$put_implied_vol - master_volChart$put_mean_vol)
master_volChart$put_vol_premium_pct <- percent(master_volChart$put_vol_premium)
master_volChart$put_vrp_edge <- (-master_volChart$put_vol_premium*100)

# Mean & median VRP
mean(master_volChart$put_vol_premium)
volMed <- median(master_volChart$put_vol_premium)
volMed
put_avg_vrp <- mean(master_volChart$put_vol_premium)
put_median_vrp <- median(master_volChart$put_vol_premium)
master_volChart$put_relative_value <- percent(volMed-master_volChart$put_vol_premium)

# Calculate the options greeks for calls
S=master_volChart$underlying
X=master_volChart$put_strike
v = master_volChart$put_implied_vol
r = master_volChart$put_risk_free
Time = master_volChart$put_dte
b = master_volChart$put_div_yield
master_volChart$d1 <- (log(S/X) + (r - b + (v^2)/2)*Time ) / (v * sqrt(Time))
master_volChart$d2 <- master_volChart$d1 - v*(sqrt(Time))
master_volChart$put_delta <- -exp( (b-r)*Time ) * pnorm(-master_volChart$d1)
master_volChart$put_theta <- (-exp(-b*Time) * (S*dnorm(master_volChart$d1)*v)/(2*sqrt(Time)) + r * X * exp(-r*Time) * pnorm(-master_volChart$d2))/1000 
master_volChart$put_gamma <- exp(-b*Time) * ( dnorm(master_volChart$d1)/( S*v*sqrt(Time)) )
master_volChart$put_vega  <- S * exp(-b*Time) * dnorm(master_volChart$d1) * sqrt(Time)/100
master_volChart$put_vanna <- -exp(-b*Time) * dnorm(master_volChart$d1) * (master_volChart$d2/v)
master_volChart$put_volga <- master_volChart$put_vega * ((master_volChart$d1*master_volChart$d2) / v)

# Calculate expected value
master_volChart$put_fair_price  <- ((master_volChart$put_vega*master_volChart$put_vrp_edge)+master_volChart$put_mid)
master_volChart$put_exp_value <- ((master_volChart$put_vega*master_volChart$put_vrp_edge)/master_volChart$put_mid)
# Save puts data
puts_master <- master_volChart

#### 6. Straddles
straddles <- merge(calls_master, puts_master, by = 'ticker')
straddles_master <- select(straddles, c(ticker,
                                        underlying = "underlying.x",
                                        strike = "call_strike",
                                        call_mid,
                                        put_mid,
                                        call_volume,
                                        put_volume,
                                        call_implied_vol,
                                        put_implied_vol,
                                        call_mean_vol,
                                        put_mean_vol,
                                        call_fair_price,
                                        put_fair_price))

straddles_master$put_call_ratio <- round(straddles_master$put_volume/ straddles_master$call_volume,3)
straddles_master$straddle_price <- straddles_master$call_mid+straddles_master$put_mid
straddles_master$straddle_fair_price <- straddles_master$call_fair_price+straddles_master$put_fair_price
straddles_master$exp_value <- (straddles_master$straddle_fair_price-straddles_master$straddle_price)/straddles_master$straddle_price
straddles_master$call_put_iv_spread <- round(straddles_master$call_implied_vol-straddles_master$put_implied_vol,4)
straddles_master$directional_lean <-ifelse(straddles_master$call_put_iv_spread > 0.015,"Bearish",
                                           ifelse(straddles_master$call_put_iv_spread < -0.015,"Bullish", "Neutral"))

# Market sector direction estimate
broad_index_sentiment <- mean(straddles_master$call_put_iv_spread)
ifelse(broad_index_sentiment > 0.015,"Bearish",
       ifelse(broad_index_sentiment < -0.015,"Bullish", "Neutral"))

bullish_tickers <-sum(straddles_master$directional_lean == 'Bullish')
bearish_tickers <-sum(straddles_master$directional_lean == 'Bearish')
neutral_tickers <-sum(straddles_master$directional_lean == 'Neutral')
bullish <- bullish_tickers/(bullish_tickers+bearish_tickers+neutral_tickers)
bearish <- bearish_tickers/(bullish_tickers+bearish_tickers+neutral_tickers)
neutral <- neutral_tickers/(bullish_tickers+bearish_tickers+neutral_tickers)
bullish
bearish 
neutral

# Filter final sector dataframe
straddles_master$avg_iv <- (straddles_master$call_implied_vol+straddles_master$put_implied_vol)/2
sector_direction_list <- select(straddles_master, c("sector" =ticker,directional_lean))

# Show as tibble
tibble(sector_direction_list)
