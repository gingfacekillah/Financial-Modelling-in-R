# Experiments on acceleration (2nd derivative of price changes) from the paper
# "The Acceleration Effect and Gamma Factor in Asset Pricing" by Diego ARDILA-ALVAREZ et al


# Load required libraries
library(quantmod)
library(tidyverse)

# Set the stock symbol and define the start and end dates
stock_symbol <- "NVDA"
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-05-26")

# Download the historical data from Yahoo Finance
getSymbols(stock_symbol, from = start_date, to = end_date, src = "yahoo")

# Extract the closing prices from the downloaded data
closing_prices <- Ad(get(stock_symbol))

# Print the first few rows of the closing prices
head(closing_prices)

# Extract the opening prices from the downloaded data
opening_prices <- Op(get(stock_symbol))

# Calculate open-to-close log returns
open_to_close_log_returns <- log(closing_prices / opening_prices)

# Print the first few rows of the open-to-close log returns
head(open_to_close_log_returns)

# Calculate log returns for different lookback periods
returns_1d <- diff(log(closing_prices))
returns_5d <- diff(log(closing_prices), lag = 5)
returns_10d <- diff(log(closing_prices), lag = 10)
returns_20d <- diff(log(closing_prices), lag = 20)
returns_30d <- diff(log(closing_prices), lag = 30)
returns_60d <- diff(log(closing_prices), lag = 60)

# Print the first few rows of each returns series
head(returns_1d)
head(returns_5d)
head(returns_10d)
head(returns_20d)
head(returns_30d)
head(returns_60d)

# Create a dataframe with the different returns
returns_df <- data.frame(
    setNames(open_to_close_log_returns, "OpenToClose"),
    setNames(returns_1d, "Returns_1d"),
    setNames(returns_5d, "Returns_5d"),
    setNames(returns_10d, "Returns_10d"),
    setNames(returns_20d, "Returns_20d"),
    setNames(returns_30d, "Returns_30d"),
    setNames(returns_60d, "Returns_60d")
)

# Print the first few rows of the combined dataframe
head(returns_df)

# Set the lookback periods
lookback_periods <- c(5, 10, 20, 30, 60)  # Lookback periods

# Calculate the lagged returns for the 1-day period
lagged_returns_1d <- lag(returns_df[, "Returns_1d"], n = max(lookback_periods))

# Define a function to calculate gamma acceleration for a given lookback period
calculate_gamma <- function(lookback_period) {
    returns_df[, paste0("Returns_", lookback_period, "d")] - lagged_returns_1d
}

# Apply the calculate_gamma function to each lookback period using sapply
gamma1d_df <- sapply(lookback_periods, calculate_gamma)

# Convert the resulting matrix to a dataframe
gamma1d_df <- data.frame(gamma1d_df)

# Assign proper column names to the gamma dataframe
colnames(gamma1d_df) <- paste0("Gamma_", lookback_periods, "d")

# Print the first few rows of the gamma dataframe
head(gamma1d_df)

dates <- rownames(returns_df)
rownames(gamma1d_df) = dates

# Calculate the lagged returns for the 1-day period
lagged_returns_5d <- lag(returns_df[, "Returns_5d"], n = max(lookback_periods))

# Define a function to calculate gamma acceleration for a given lookback period
calculate_gamma <- function(lookback_period) {
    returns_df[, paste0("Returns_", lookback_period, "d")] - lagged_returns_5d
}

# Apply the calculate_gamma function to each lookback period using sapply
gamma5d_df <- sapply(lookback_periods, calculate_gamma)

# Convert the resulting matrix to a dataframe
gamma5d_df <- data.frame(gamma5d_df)

# Assign proper column names to the gamma dataframe
colnames(gamma5d_df) <- paste0("Gamma_", lookback_periods, "d")

# Print the first few rows of the gamma dataframe
head(gamma5d_df)

dates <- rownames(returns_df)
rownames(gamma5d_df) = dates

gamma1d_df <- gamma1d_df %>%
    rename(g1d5d = Gamma_5d,
           g1d10d = Gamma_10d,
           g1d20d = Gamma_20d,
           g1d30d = Gamma_30d,
           g1d60d = Gamma_60d)

gamma5d_df <- gamma5d_df %>%
    rename(g5d5d = Gamma_5d,
           g5d10d = Gamma_10d,
           g5d20d = Gamma_20d,
           g5d30d = Gamma_30d,
           g5d60d = Gamma_60d)

# Add gamma_df_aligned to returns_df
combined_df <- cbind(returns_df, gamma1d_df, gamma5d_df)
# Omit rows with NA values
combined_df <- na.omit(combined_df)

# Lag the open-to-close returns by one day
# combined_df$OpenToClose_lagged <- lag(combined_df$OpenToClose, n = 1)

# Print the first few rows of the combined dataframe
head(combined_df)

combined_df <- combined_df %>%
    select(g1d5d, g1d10d, g1d20d, g1d30d, g1d60d, g5d5d, g5d10d, g5d20d, g5d30d, g5d60d, OpenToClose)
cor(combined_df)
