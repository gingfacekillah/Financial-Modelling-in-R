# Returns a rolling volatility estimation using close to close and Garman-Klass-Yhang-Zhang method (GKYZ)

library(quantmod)
library(TTR)

# Define the start and end dates for the data
start_date <- "2018-01-01"
end_date <- Sys.Date()

# Define the symbols for the S&P 500 components
symbols <- "^GSPC"

# Get the S&P 500 data from Yahoo Finance
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)

# Calculate daily, weekly, and monthly returns
sp500_returns_daily <- dailyReturn(Cl(GSPC), type = "log")
sp500_returns_weekly <- weeklyReturn(Cl(GSPC), type = "log")
sp500_returns_monthly <- monthlyReturn(Cl(GSPC), type = "log")

# Calculate volatility using the close-to-close method
sp500_volatility_close_5d <- volatility(GSPC, n = 5, calc = "close")
sp500_volatility_close_10d <- volatility(GSPC, n = 10, calc = "close")
sp500_volatility_close_20d <- volatility(GSPC, n = 20, calc = "close")
sp500_volatility_close_60d <- volatility(GSPC, n = 60, calc = "close")
sp500_volatility_close_180d <- volatility(GSPC, n = 180, calc = "close")

# Calculate volatility using the Garman-Klass-Yhang-Zhang method
sp500_volatility_gkyz_5d <- volatility(GSPC, n = 5, calc = "gk.yz")
sp500_volatility_gkyz_10d <- volatility(GSPC, n = 10, calc = "gk.yz")
sp500_volatility_gkyz_20d <- volatility(GSPC, n = 20, calc = "gk.yz")
sp500_volatility_gkyz_60d <- volatility(GSPC, n = 60, calc = "gk.yz")
sp500_volatility_gkyz_180d <- volatility(GSPC, n = 180, calc = "gk.yz")

# Create a table summarizing the data
summary_table <- data.frame(
    Volatility_Type = c(rep("Close-to-Close", 1), rep("Garman-Klass-Yhang-Zhang", 1),
                        rep("Close-to-Close", 1), rep("Garman-Klass-Yhang-Zhang", 1),
                        rep("Close-to-Close", 1), rep("Garman-Klass-Yhang-Zhang", 1),
                        rep("Close-to-Close", 1), rep("Garman-Klass-Yhang-Zhang", 1),
                        rep("Close-to-Close", 1), rep("Garman-Klass-Yhang-Zhang", 1)),
    Timeframe = c(rep("5 Days", 2), rep("10 Days", 2), rep("20 Days", 2), rep("60 Days", 2), rep("180 Days", 2)),
    Volatility = c(
        sp500_volatility_close_5d[length(sp500_volatility_close_5d)], sp500_volatility_gkyz_5d[length(sp500_volatility_gkyz_5d)],
        sp500_volatility_close_10d[length(sp500_volatility_close_10d)], sp500_volatility_gkyz_10d[length(sp500_volatility_gkyz_10d)],
        sp500_volatility_close_20d[length(sp500_volatility_close_20d)], sp500_volatility_gkyz_20d[length(sp500_volatility_gkyz_20d)],
        sp500_volatility_close_60d[length(sp500_volatility_close_60d)], sp500_volatility_gkyz_60d[length(sp500_volatility_gkyz_60d)],
        sp500_volatility_close_180d[length(sp500_volatility_close_180d)], sp500_volatility_gkyz_180d[length(sp500_volatility_gkyz_180d)])
)

# Print the summary table
print(summary_table)

# Create a graph of the close-to-close volatility over time
plot(sp500_volatility_close_5d, type = "l", col = "blue", main = "S&P 500 Close-to-Close Volatility", ylab = "Volatility", xlab = "Date")

# Add lines for each timeframe
lines(sp500_volatility_close_5d, col = "green")
lines(sp500_volatility_close_10d, col = "red")
lines(sp500_volatility_close_20d, col = "orange")
lines(sp500_volatility_close_60d, col = "purple")
lines(sp500_volatility_close_180d, col = "brown")

# Add a legend
legend("topright", c("Close-to-Close", "5 Days", "10 Days", "20 Days", "60 Days", "180 Days"),
       lty = 1, col = c("blue", "green", "red", "orange", "purple", "brown"))
