### Heston stochastic model for simulating volatility and/or underlying prices
### @Gingfacekillah

# Load necessary libraries
library(quantmod)

# Download SPX data
getSymbols("^GSPC", src = "yahoo", from = Sys.Date()-1000, to = Sys.Date())
spx_prices <- Cl(GSPC)

# Calculate log returns
log_returns <- diff(log(spx_prices))
log_returns <- na.omit(log_returns)

# Estimate parameters (initial guesses)
mu_est <- mean(log_returns)
var_est <- var(log_returns)
kappa <- 2
theta <- var_est
sigma <- 0.1
rho <- -0.7

# Define the simulation function for the Heston model
simulate_heston <- function(S0, V0, mu, kappa, theta, sigma, rho, T, dt, n_sim) {
    n_steps <- T / dt
    S <- matrix(0, nrow = n_steps + 1, ncol = n_sim)
    V <- matrix(0, nrow = n_steps + 1, ncol = n_sim)

    S[1, ] <- S0
    V[1, ] <- V0

    for (j in 1:n_sim) {
        for (i in 2:(n_steps + 1)) {
            Z1 <- rnorm(1)
            Z2 <- rnorm(1)
            Wt_S <- Z1
            Wt_V <- rho * Z1 + sqrt(1 - rho^2) * Z2

            S[i, j] <- S[i - 1, j] * exp((mu - 0.5 * V[i - 1, j]) * dt + sqrt(V[i - 1, j] * dt) * Wt_S)
            V[i, j] <- abs(V[i - 1, j] + kappa * (theta - V[i - 1, j]) * dt + sigma * sqrt(V[i - 1, j] * dt) * Wt_V)
        }
    }
    return(list(S = S, V = V))
}

# Parameters for simulation
S0 <- as.numeric(last(spx_prices))
V0 <- var_est
T <- 60  # Total time (days)
dt <- 1 / 252  # Time step (1 trading day)
n_sim <- 250  # Number of simulations

# Simulate Heston model
sim_results <- simulate_heston(S0, V0, mu_est, kappa, theta, sigma, rho, T, dt, n_sim)

# Extract simulated prices
simulated_prices <- sim_results$S

# Calculate statistics
median_prices <- apply(simulated_prices, 1, median)
upper_bound <- apply(simulated_prices, 1, quantile, probs = 0.975)
lower_bound <- apply(simulated_prices, 1, quantile, probs = 0.025)

# Generate date sequence for the next 60 days
start_date <- as.Date(last(index(spx_prices)))
future_dates <- seq(start_date + 1, by = "day", length.out = T)

# Determine y-axis limits based on quantiles of the simulated paths
y_limits <- range(c(5100, 5400))

# Plot the simulated prices
plot(future_dates, simulated_prices[2:(T + 1), 1], type = 'n', col = 'blue', lwd = 2,
     main = 'Heston Model Simulated SPX Prices: 60 Days',
     xlab = 'Date', ylab = 'Simulated SPX Price', ylim = y_limits)

# Add all paths with different shades of blue
colors <- colorRampPalette(c("lightblue", "darkblue"))(n_sim)
for (i in 1:n_sim) {
    lines(future_dates, simulated_prices[2:(T + 1), i], col = colors[i], lwd = 2)
}

# Add median, upper and lower bounds
lines(future_dates, median_prices[2:(T + 1)], col = 'green', lwd = 2, lty = 2)
lines(future_dates, upper_bound[2:(T + 1)], col = 'red', lwd = 4, lty = 3)
lines(future_dates, lower_bound[2:(T + 1)], col = 'red', lwd = 4, lty = 3)

# Add legend
legend('topright', legend = c("Median", "95% CI"), col = c("green", "red"), lwd = 2, lty = c(2, 3))

# Calculate the percentage of the time the final simulated price is above the current SPX price
final_simulated_prices <- simulated_prices[nrow(simulated_prices), ]
percentage_above <- mean(final_simulated_prices > S0) * 100

# Print the result
cat("Percentage of the time the final simulated price is above the current SPX price:", percentage_above, "%\n")
tail(median_prices,1)
