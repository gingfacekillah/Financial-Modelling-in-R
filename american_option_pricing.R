### American Option Pricing Models in R
### @Gingfacekillah
# Load required libraries
library(quantmod)


### 1. Binomial Trees
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


### 2. Least Squares Monte Carlo
# Function to estimate American option price using LSMC
lsmc_american_option <- function(S, K, r, T, sigma, n_simulations, n_steps, option_type = "call") {
    dt <- T / n_steps
    discount_factor <- exp(-r * dt)

    # Simulate price paths
    price_paths <- matrix(0, nrow = n_simulations, ncol = n_steps + 1)
    price_paths[, 1] <- S
    for (i in 2:(n_steps + 1)) {
        z <- rnorm(n_simulations)
        price_paths[, i] <- price_paths[, i - 1] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
    }

    # Calculate payoffs at maturity
    if (option_type == "call") {
        payoffs <- pmax(price_paths[, n_steps + 1] - K, 0)
    } else {
        payoffs <- pmax(K - price_paths[, n_steps + 1], 0)
    }

    for (i in n_steps:1) {
        exercise_values <- if (option_type == "call") {
            pmax(price_paths[, i] - K, 0)
        } else {
            pmax(K - price_paths[, i], 0)
        }

        continuation_values <- discount_factor * payoffs
        regression <- lm(continuation_values ~ price_paths[, i])
        estimated_continuation_values <- predict(regression, newdata = data.frame(price_paths[, i]))

        payoffs <- ifelse(exercise_values > estimated_continuation_values, exercise_values, continuation_values)
    }

    return(mean(payoffs) * discount_factor)
}

# Parameters
S <- 5277  # Start price
K <- 5280  # Strike price
r <- 0.0538  # Risk-free rate
T <- 5 / 252  # Time to expiration in years
sigma <- 0.105  # Volatility
n_simulations <- 10000  # Number of simulations
n_steps <- 50  # Number of steps

# Calculate American call and put option prices
american_call_price <- lsmc_american_option(S, K, r, T, sigma, n_simulations, n_steps, option_type = "call")
american_put_price <- lsmc_american_option(S, K, r, T, sigma, n_simulations, n_steps, option_type = "put")

# Print results
cat("American Call Option Price (LSMC):", american_call_price, "\n")
cat("American Put Option Price (LSMC):", american_put_price, "\n")


### 3. Finite Difference Model
# Function to price an American option using the Finite Difference method
finite_difference_american_option <- function(S, K, r, T, sigma, N, M, option_type = "call") {
    # Parameters
    dt <- T / N
    dS <- 2 * S / M
    S_grid <- seq(0, 2 * S, by = dS)
    option_grid <- matrix(0, nrow = M + 1, ncol = N + 1)

    # Set terminal payoff
    if (option_type == "call") {
        option_grid[, N + 1] <- pmax(S_grid - K, 0)
    } else {
        option_grid[, N + 1] <- pmax(K - S_grid, 0)
    }

    # Boundary conditions
    if (option_type == "call") {
        option_grid[M + 1, ] <- (S_grid[M + 1] - K) * exp(-r * dt * (N:0))
        option_grid[1, ] <- 0
    } else {
        option_grid[M + 1, ] <- 0
        option_grid[1, ] <- (K - S_grid[1]) * exp(-r * dt * (N:0))
    }

    # Finite difference coefficients
    alpha <- 0.5 * dt * (sigma^2 * (0:M)^2 - r * (0:M))
    beta <- 1 - dt * (sigma^2 * (0:M)^2 + r)
    gamma <- 0.5 * dt * (sigma^2 * (0:M)^2 + r * (0:M))

    # Backward induction
    for (j in N:1) {
        for (i in 2:M) {
            option_grid[i, j] <- alpha[i] * option_grid[i - 1, j + 1] +
                beta[i] * option_grid[i, j + 1] +
                gamma[i] * option_grid[i + 1, j + 1]
            # Early exercise condition for American options
            if (option_type == "call") {
                option_grid[i, j] <- max(option_grid[i, j], S_grid[i] - K)
            } else {
                option_grid[i, j] <- max(option_grid[i, j], K - S_grid[i])
            }
        }
    }

    # Interpolate to find the option price at the initial stock price
    option_price <- approx(S_grid, option_grid[, 1], xout = S)$y
    return(option_price)
}

# Parameters
S <- 5277  # Current stock price
K <- 5280  # Strike price
r <- 0.0538  # Risk-free rate
T <- 5 / 252  # Time to expiration in years
sigma <- 0.105  # Volatility
N <- 1000  # Number of time steps
M <- 1000  # Number of price steps

# Calculate American call and put option prices
american_call_price <- finite_difference_american_option(S, K, r, T, sigma, N, M, option_type = "call")
american_put_price <- finite_difference_american_option(S, K, r, T, sigma, N, M, option_type = "put")

# Print results
cat("American Call Option Price (Finite Difference):", american_call_price, "\n")
cat("American Put Option Price (Finite Difference):", american_put_price, "\n")
