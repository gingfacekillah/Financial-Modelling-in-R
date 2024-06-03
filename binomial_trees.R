# Load required libraries
library(quantmod)

# Function to calculate the price of an American option using the Binomial Tree model
binomial_tree_american_option <- function(S, K, r, T, sigma, n, option_type = "call") {
    dt <- T / n
    u <- exp(sigma * sqrt(dt))
    d <- 1 / u
    p <- (exp(r * dt) - d) / (u - d)

    # Initialize stock price tree
    stock_price <- matrix(0, nrow = n + 1, ncol = n + 1)
    for (i in 0:n) {
        for (j in 0:i) {
            stock_price[j + 1, i + 1] <- S * u^j * d^(i - j)
        }
    }

    # Initialize option price tree
    option_price <- matrix(0, nrow = n + 1, ncol = n + 1)
    if (option_type == "call") {
        option_price[, n + 1] <- pmax(stock_price[, n + 1] - K, 0)
    } else {
        option_price[, n + 1] <- pmax(K - stock_price[, n + 1], 0)
    }

    # Backward induction to calculate option price
    for (i in (n - 1):0) {
        for (j in 0:i) {
            if (option_type == "call") {
                option_price[j + 1, i + 1] <- max(stock_price[j + 1, i + 1] - K, exp(-r * dt) * (p * option_price[j + 2, i + 2] + (1 - p) * option_price[j + 1, i + 2]))
            } else {
                option_price[j + 1, i + 1] <- max(K - stock_price[j + 1, i + 1], exp(-r * dt) * (p * option_price[j + 2, i + 2] + (1 - p) * option_price[j + 1, i + 2]))
            }
        }
    }

    return(option_price[1, 1])
}

# Parameters
S <- 5277  # Underlying
K <- 5280  # Strike price
r <- 0.0538  # Risk-free rate
T <- 5 / 252  # Time to expiration in years
sigma <- 0.105  # Volatility
n <- 100  # Number of steps in the binomial tree

# Calculate American call and put option prices
american_call_price <- binomial_tree_american_option(S, K, r, T, sigma, n, option_type = "call")
american_put_price <- binomial_tree_american_option(S, K, r, T, sigma, n, option_type = "put")

# Print results
cat("American Call Option Price:", american_call_price, "\n")
cat("American Put Option Price:", american_put_price, "\n")
