#### Single Factor Momentum Model #### | Gingfacekillah
#
# This project analyzes volatility-scaled relative returns for S&P 500 stocks using historical price data and polynomial regression models.
# It begins by scraping S&P 500 constituent data from Wikipedia and retrieving historical stock prices and S&P 500 index data using tidyquant.
# Daily log returns are calculated for each stock and the index, with rolling 252-day sums computed after excluding the most recent 21 days.
# Relative returns are derived by subtracting the index returns from individual stock returns and scaling them by rolling volatility.
# A 4th-degree polynomial regression model is applied to identify latent trends and predict returns as of 22 days prior to the most recent observation.
# Stocks are then ranked into deciles based on predicted returns, with the top decile identified as high-potential candidates for further analysis.
# The project also includes visualization of actual versus fitted returns for the top 6 stocks to evaluate model performance. 

# Load necessary libraries
library(tidyverse)
library(quantmod)
library(tidyquant)
library(PerformanceAnalytics)
library(lubridate)
library(rvest)
library(broom)
library(xts)
library(zoo)
library(data.table)
library(pbapply)
library(ggplot2)
library(gridExtra)

# -------------------------
# 1. Scrape S&P 500 Constituents from Wikipedia
# -------------------------

# Define the Wikipedia URL for S&P 500 constituents
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content of the page
sp500_page <- read_html(sp500_url)

# Extract the table with S&P 500 constituents using XPath
sp500_table <- sp500_page %>%
    html_node(xpath = '//*[@id="constituents"]') %>%
    html_table()

# Clean and rename columns for clarity
sp500 <- sp500_table %>%
    select(Symbol, Security, `GICS Sector`, `GICS Sub-Industry`) %>%
    rename(
        Ticker = Symbol,
        Company = Security,
        Sector = `GICS Sector`,
        Sub_Sector = `GICS Sub-Industry`
    )

# Fix problematic tickers with dots (e.g., BRK.B to BRK-B)
sp500$Ticker <- str_replace_all(sp500$Ticker, "\\.", "-")

# -------------------------
# 2. Retrieve SPX Data and Calculate SPX Log Returns
# -------------------------

# Define the analysis period: last 2 years
start_date <- Sys.Date() - years(2)
end_date <- Sys.Date()

# Retrieve historical adjusted closing prices for the S&P 500 Index (SPX)
spx_price <- tq_get("^GSPC", from = start_date, to = end_date, get = "stock.prices") %>%
    select(Date = date, SPX = adjusted)

# Calculate SPX daily log returns
spx_log_returns <- spx_price %>%
    arrange(Date) %>%
    mutate(SPX_Log_Return = log(SPX / lag(SPX))) %>%
    select(Date, SPX_Log_Return) %>%
    na.omit()  # Remove NA from lagging

# -------------------------
# 3. Retrieve Stock Data and Calculate Log Returns
# -------------------------

# Define a custom function to use tq_get for each ticker
get_data <- function(ticker) {
    tryCatch(
        {
            data <- tq_get(ticker, from = start_date, to = end_date, get = "stock.prices")
            if (nrow(data) == 0) {
                message(paste("No data returned for:", ticker))
                # Return an empty data frame with expected columns
                data <- data.frame(symbol = ticker, date = as.Date(NA), adjusted = NA_real_)
            }
            return(data)
        },
        error = function(e) {
            message(paste("Failed to get data for:", ticker))
            # Return an empty data frame with expected columns
            return(data.frame(symbol = ticker, date = as.Date(NA), adjusted = NA_real_))
        }
    )
}

# Use pbapply to retrieve data for all S&P 500 tickers with progress bar
sp500_prices_list <- pbapply::pblapply(sp500$Ticker, get_data)

# Combine the list of data frames into a single data frame
sp500_prices <- bind_rows(sp500_prices_list) %>%
    filter(!is.na(adjusted))  # Remove rows with missing price data

# Clean the combined data
sp500_prices <- sp500_prices %>%
    select(Ticker = symbol, Date = date, Adjusted = adjusted) %>%
    left_join(sp500, by = "Ticker")

# Reshape stock prices to wide format
stock_prices_wide <- sp500_prices %>%
    select(Ticker, Date, Adjusted) %>%
    pivot_wider(names_from = Ticker, values_from = Adjusted) %>%
    arrange(Date)

# Convert the wide dataframe to an xts object
stock_prices_xts <- stock_prices_wide %>%
    column_to_rownames("Date") %>%
    as.xts()

# Calculate log returns for each stock
stock_log_returns_xts <- stock_prices_xts %>%
    Return.calculate(method = "log")

# -------------------------
# 4. Handle Missing Data and Ensure Sufficient Data Length
# -------------------------

# Remove columns (stocks) with insufficient data
min_required_rows <- 252 + 21  # Rolling window size plus skip window

# Identify stocks with sufficient data
sufficient_data_cols <- sapply(stock_log_returns_xts, function(x) sum(!is.na(x)) >= min_required_rows)

# Filter the xts object to include only stocks with sufficient data
stock_log_returns_xts <- stock_log_returns_xts[, sufficient_data_cols]

# Remove rows with NA values
stock_log_returns_xts <- na.omit(stock_log_returns_xts)

# Update the number of rows after filtering
n_rows <- nrow(stock_log_returns_xts)
print(paste("Number of rows after filtering:", n_rows))

# Ensure we have enough data to proceed
if (n_rows < min_required_rows) {
    stop("Not enough data available after filtering to perform rolling calculations.")
}

# -------------------------
# 5. Calculate Rolling Log Returns Excluding the Most Recent Month
# -------------------------

# Define the rolling window length: 252 days (approx. 12 months)
rolling_window <- 252

# Define the skip window length: 21 days (approx. 1 month)
skip_window <- 21

# Shift the data to exclude the last 21 days
stock_log_returns_xts_shifted <- xts::lag.xts(stock_log_returns_xts, k = skip_window)

# Remove initial rows with NA values due to shifting
stock_log_returns_xts_shifted <- stock_log_returns_xts_shifted[-c(1:skip_window), ]

# Calculate rolling sum of log returns over the shifted data
rolling_log_returns_xts <- rollapply(
    stock_log_returns_xts_shifted,
    width = rolling_window,
    FUN = function(x) sum(x),
    by.column = TRUE,
    fill = NA,
    align = "right"
)

# Calculate rolling volatility over the same period
stock_rolling_volatility_xts <- rollapply(
    stock_log_returns_xts_shifted,
    width = rolling_window,
    FUN = function(x) sd(x) * sqrt(252),
    by.column = TRUE,
    fill = NA,
    align = "right"
)

# Remove initial rows with NA values due to rolling calculations
rolling_log_returns_xts <- na.omit(rolling_log_returns_xts)
stock_rolling_volatility_xts <- na.omit(stock_rolling_volatility_xts)

# -------------------------
# 6. Align Dates and Prepare Data for Analysis
# -------------------------

# Shift SPX log returns to exclude the most recent month
spx_log_returns_shifted <- spx_log_returns %>%
    mutate(SPX_Log_Return_Shifted = lag(SPX_Log_Return, n = skip_window))

# Remove initial rows with NA values due to shifting
spx_log_returns_shifted <- spx_log_returns_shifted[-c(1:skip_window), ]

# Calculate rolling SPX returns over the shifted data
rolling_spx_log_returns <- rollapply(
    spx_log_returns_shifted$SPX_Log_Return_Shifted,
    width = rolling_window,
    FUN = sum,
    fill = NA,
    align = "right"
)

# Create a data frame for SPX rolling returns
rolling_spx_log_returns_df <- data.frame(
    Date = spx_log_returns_shifted$Date,
    SPX_Rolling_Log_Return = rolling_spx_log_returns
) %>%
    na.omit()

# Ensure that dates align between stock and SPX data
common_dates <- index(rolling_log_returns_xts)
rolling_spx_log_returns_df <- rolling_spx_log_returns_df %>%
    filter(Date %in% as.Date(common_dates))

# -------------------------
# 7. Merge Stock and SPX Data to Calculate Relative Returns and Apply Volatility Scaling
# -------------------------

# Convert rolling_log_returns_xts to data frame
rolling_log_returns_df <- fortify.zoo(rolling_log_returns_xts) %>%
    rename(Date = Index)

# Convert stock_rolling_volatility_xts to data frame
stock_rolling_volatility_df <- fortify.zoo(stock_rolling_volatility_xts) %>%
    rename(Date = Index)

# Merge data and calculate relative returns
relative_returns <- rolling_log_returns_df %>%
    pivot_longer(-Date, names_to = "Ticker", values_to = "Stock_Rolling_Log_Return") %>%
    left_join(rolling_spx_log_returns_df, by = "Date") %>%
    mutate(Relative_Return = Stock_Rolling_Log_Return - SPX_Rolling_Log_Return) %>%
    left_join(stock_rolling_volatility_df %>%
                  pivot_longer(-Date, names_to = "Ticker", values_to = "Volatility"),
              by = c("Date", "Ticker")) %>%
    mutate(Volatility_Scaled_Return = Relative_Return / Volatility) %>%
    na.omit()

# Convert to wide format
relative_returns_wide <- relative_returns %>%
    select(Date, Ticker, Volatility_Scaled_Return) %>%
    pivot_wider(names_from = Ticker, values_from = Volatility_Scaled_Return)

# -------------------------
# Adjust the Dates to Exclude the Most Recent Month
# -------------------------

# Exclude the most recent 21 days from the dates in relative_returns_wide
max_date_in_data <- max(relative_returns_wide$Date)

# Calculate the cutoff date (t-22)
cutoff_date <- max_date_in_data - days(skip_window)

# Filter the data to include dates up to t-22
relative_returns_wide <- relative_returns_wide %>%
    filter(Date <= cutoff_date)

# -------------------------
# 8. Polynomial Regression to Predict Returns as of t-22
# -------------------------

# Define the non-linear model for the polynomial regression
fit_non_linear_model <- function(latent_factor, degree = 4) {
    # Define the non-linear model formula for polynomial regression
    terms <- paste0("c", 0:degree, " * x^", 0:degree)
    model_formula <- as.formula(paste("latent_factor ~", paste(terms, collapse = " + ")))

    # Set up initial starting values for the coefficients
    start_values <- setNames(rep(0.1, degree + 1), paste0("c", 0:degree))

    # Prepare data for modeling
    data <- data.frame(latent_factor = latent_factor, x = seq_along(latent_factor))

    # Fit the non-linear model using nls with error handling
    poly_fit <- tryCatch({
        nls(model_formula, data = data, start = start_values, control = nls.control(maxiter = 500))
    }, error = function(e) {
        message(paste("Non-linear model fitting failed:", e$message))
        return(NULL)
    })

    return(poly_fit)
}

# Function to fit and extract fitted values
fit_and_extract_fitted_values <- function(latent_factor, degree = 4) {
    # Fit non-linear model
    poly_fit <- fit_non_linear_model(latent_factor, degree)

    if (is.null(poly_fit)) {
        return(rep(NA, length(latent_factor)))
    }

    # Extract fitted values (predicted returns)
    fitted_values <- predict(poly_fit)

    return(fitted_values)
}

# Fit non-linear polynomial regression for each stock and predict returns as of t-22
predicted_poly_returns <- pbapply::pblapply(colnames(relative_returns_wide)[-1], function(ticker) {
    stock_latent_factor <- relative_returns_wide[[ticker]]

    # Check if data is valid
    if (all(is.na(stock_latent_factor))) {
        message(paste("Invalid data for ticker:", ticker))
        return(data.frame(Ticker = ticker, Predicted_Return = NA))
    }

    # Fit non-linear polynomial regression model
    poly_model <- fit_non_linear_model(stock_latent_factor, degree = 4)

    if (is.null(poly_model)) {
        return(data.frame(Ticker = ticker, Predicted_Return = NA))
    }

    # Use the fitted value at t-22 as the prediction
    fitted_values <- predict(poly_model)
    predicted_return <- tail(fitted_values, 1)

    return(data.frame(Ticker = ticker, Predicted_Return = predicted_return))
})

# Convert list to data frame and remove rows where prediction failed (NA)
predicted_poly_returns <- bind_rows(predicted_poly_returns) %>%
    filter(!is.na(Predicted_Return))

# Sort stocks into deciles based on predicted returns
predicted_poly_returns <- predicted_poly_returns %>%
    arrange(desc(Predicted_Return)) %>%
    mutate(Decile = ntile(Predicted_Return, 10))

# Print the tickers in the top decile
top_decile_poly <- predicted_poly_returns %>% filter(Decile == 10)

print("Top Decile Tickers based on Non-Linear Polynomial Estimation with Volatility Scaling (as of t-22):")
print(top_decile_poly$Ticker)

# Create a watchlist with additional company information
watchlist <- left_join(top_decile_poly, sp500, by = "Ticker")

# -------------------------
# 9. Visualize Model Fits for Top 6 Tickers (up to t-22)
# -------------------------

# Adjust plotting to include data only up to t-22
relative_returns_wide_plot <- relative_returns_wide

# Extract top 6 tickers based on predicted returns
top_6_tickers <- predicted_poly_returns %>%
    arrange(desc(Predicted_Return)) %>%
    slice(1:6) %>%
    pull(Ticker)

# Create a list to store the plots
plot_list <- list()

# Loop over the top 6 tickers to create actual vs. fitted plots
for (ticker in top_6_tickers) {
    stock_latent_factor <- relative_returns_wide_plot[[ticker]]

    # Fit the model and get fitted values
    fitted_values <- fit_and_extract_fitted_values(stock_latent_factor)

    # Create a data frame with actual and fitted values for the plot
    plot_data <- data.frame(
        Date = relative_returns_wide_plot$Date,
        Actual_Rolling_Return = stock_latent_factor,
        Fitted_Rolling_Return = fitted_values
    )

    # Create the plot for the current ticker
    p <- ggplot(plot_data, aes(x = Date)) +
        geom_line(aes(y = Actual_Rolling_Return, color = "Actual"), size = 1) +
        geom_line(aes(y = Fitted_Rolling_Return, color = "Fitted"), size = 1, linetype = "dashed") +
        labs(title = paste("Actual vs. Fitted Rolling Returns for", ticker, "(up to t-22)"),
             y = "Volatility-Scaled Relative Return", color = "Legend") +
        theme_minimal() +
        scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red"))

    # Add the plot to the list
    plot_list[[ticker]] <- p
}

# Arrange the plots in a grid for easy comparison (showing 6 plots in a 2x3 grid)
do.call("grid.arrange", c(plot_list, ncol = 2))

