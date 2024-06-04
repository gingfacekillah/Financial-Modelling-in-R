### Fitting a Heston model & Simulating data | @Gingfacekillah
# Libraries
library(quantmod)
library(ggplot2)

# Step 1: Get historical stock price data
getSymbols("^GSPC", src = "yahoo", from = Sys.Date()-1000, to = Sys.Date())
stock_data <- Cl(GSPC)

# Calculate daily logRets
log_returns <- diff(log(stock_data))[-1]

# Step 2: Estimate initial variance
initial_variance <- var(log_returns)

# Calculate drift
mu <- mean(log_returns)

# Step 3: Define Heston model
heston_variance <- function(params, log_returns, dt) {
    kappa <- params[1]
    theta <- params[2]
    sigma_v <- params[3]
    rho <- params[4]
    v0 <- params[5]
    
    n <- length(log_returns)
    v <- numeric(n)
    v[1] <- v0
    W1 <- rnorm(n - 1)
    W2 <- rho * W1 + sqrt(1 - rho^2) * rnorm(n - 1)
    
    for (t in 2:n) {
        v[t] <- max(v[t-1] + kappa * (theta - v[t-1]) * dt + sigma_v * sqrt(v[t-1] * dt) * W2[t-1], 0)
    }
    
    return(v)
}

# Objective function to minimize
objective_function <- function(params, log_returns, dt) {
    v <- heston_variance(params, log_returns, dt)
    model_variance <- mean(v)
    empirical_variance <- var(log_returns)
    return((model_variance - empirical_variance)^2)
}

# Step 4: Optimize params w/ DEoptim
# Define ranges for the params
lower_bounds <- c(0.01, 0.01, 0.01, -0.99, 0.01)
upper_bounds <- c(10, 1, 1, 0.99, 1)

# Time step (daily)
dt <- 1 / 252

# Initial param guesses
initial_params <- c(kappa = 2, theta = 0.04, sigma_v = 0.2, rho = -0.5, v0 = initial_variance)

# Calibrate Heston model w/ DEoptim
result_de <- DEoptim(objective_function, lower = lower_bounds, upper = upper_bounds, log_returns = log_returns, dt = dt)

# Extract calibrated params
calibrated_params_de <- result_de$optim$bestmem
cat("Calibrated Parameters (DE):\n")
print(calibrated_params_de)

# Step 5: Simulation model
simulate_heston <- function(S0, v0, kappa, theta, sigma_v, rho, mu, T, dt, n_paths) {
    n_steps <- T / dt
    S <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
    v <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
    S[1, ] <- S0
    v[1, ] <- v0
    for (i in 2:(n_steps + 1)) {
        Z1 <- rnorm(n_paths)
        Z2 <- rnorm(n_paths)
        W1 <- Z1
        W2 <- rho * Z1 + sqrt(1 - rho^2) * Z2
        v[i, ] <- pmax(v[i - 1, ] + kappa * (theta - v[i - 1, ]) * dt + sigma_v * sqrt(v[i - 1, ] * dt) * W2, 0)
        S[i, ] <- S[i - 1, ] * exp((mu - 0.5 * v[i - 1, ]) * dt + sqrt(v[i - 1, ] * dt) * W1)
    }
    return(S)
}

# Params for simulation
S0 <- as.numeric(last(stock_data))
v0 <- calibrated_params_de[5]
kappa <- calibrated_params_de[1]
theta <- calibrated_params_de[2]
sigma_v <- calibrated_params_de[3]
rho <- calibrated_params_de[4]
T <- 4/252  # Time as days to simulate (% of 1 year)
n_paths <- 10000

# Simulate future stock prices using the calibrated Heston model
simulated_prices <- simulate_heston(S0, v0, kappa, theta, sigma_v, rho, mu, T, dt, n_paths)


time <- seq(1, T * 252, length.out = nrow(simulated_prices))  # Days as whole numbers
path_df <- data.frame(time = rep(time, n_paths), value = as.vector(simulated_prices), path = rep(1:n_paths, each = length(time)))

ggplot(path_df, aes(x = time, y = value, group = path, color = factor(path))) +
    geom_line() +
    scale_x_continuous(breaks = seq(1, T * 252, by = 1)) +  # Days as whole numbers
    labs(title = "Heston Model Simulated Paths", x = "Days", y = "Stock Price") +
    theme_minimal() +
    theme(legend.position = "none")
