#### Simple particle filter model with Bayesian optimization of parameters | @gingfacekillah
# Libraries
library(quantmod)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(PerformanceAnalytics)
library(rBayesianOptimization)

# Get SPX data
getSymbols("^GSPC", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Closing prices
spx <- Cl(GSPC)

# Remove rows with NAs
spx <- na.omit(spx)

# Inspect data
head(spx)

# Particle filter functions
# Initialization of particles
initialize_particles <- function(n_particles, initial_price, state_dim) {
    particles <- list()
    for (i in 1:n_particles) {
        particles[[i]] <- list(
            stock_state = rnorm(state_dim, mean = initial_price, sd = initial_price * 0.01),
            weight = 1 / n_particles
        )
    }
    return(particles)
}

# Prediction step
predict_particles <- function(particles, state_dim, observation, obs_noise_var, alpha = 0.5) {
    for (i in 1:length(particles)) {
        particles[[i]]$stock_state <- alpha * particles[[i]]$stock_state + (1 - alpha) * observation + rnorm(state_dim, 0, 0.02)
    }
    return(particles)
}

# Update step with resampling
update_particles <- function(particles, observation, obs_noise_var) {
    weights <- numeric(length(particles))
    for (i in 1:length(particles)) {
        predicted_price <- mean(particles[[i]]$stock_state)
        error <- observation - predicted_price
        particles[[i]]$weight <- dnorm(error, mean = 0, sd = sqrt(obs_noise_var))
        weights[i] <- particles[[i]]$weight
    }

    # Normalize weights and handle zero or NA weights
    weights_sum <- sum(weights, na.rm = TRUE)
    if (weights_sum == 0 || all(is.na(weights))) {
        weights <- rep(1 / length(particles), length(particles))
    } else {
        weights <- weights / weights_sum
    }

    # Resample particles based on weights
    resampled_indices <- sample(1:length(particles), size = length(particles), replace = TRUE, prob = weights)
    resampled_particles <- particles[resampled_indices]

    # Add noise to maintain diversity
    for (i in 1:length(resampled_particles)) {
        resampled_particles[[i]]$stock_state <- resampled_particles[[i]]$stock_state + rnorm(length(resampled_particles[[i]]$stock_state), 0, 0.02)
    }

    return(resampled_particles)
}

# Apply functions
ddpf <- function(data, n_particles, state_dim, obs_noise_var, alpha = 0.5) {
    initial_price <- as.numeric(data[1])
    particles <- initialize_particles(n_particles, initial_price, state_dim)
    predictions <- numeric(length(data))
    predictions[1] <- initial_price

    for (t in 2:length(data)) {
        particles <- predict_particles(particles, state_dim, as.numeric(data[t]), obs_noise_var, alpha)
        particles <- update_particles(particles, as.numeric(data[t]), obs_noise_var)

        # Estimate the predicted price
        predicted_prices <- sapply(particles, function(p) mean(p$stock_state))
        predictions[t] <- mean(predicted_prices)
    }

    return(predictions)
}

# Objective function for Bayesian optimization
objective_function <- function(n_particles, obs_noise_var, alpha) {
    n_particles <- round(n_particles)
    obs_noise_var <- exp(obs_noise_var)  # Transform back to original scale

    # Run the PF model
    predictions <- ddpf(spx, n_particles, state_dim = 1, obs_noise_var, alpha)

    # Convert data to a data frame
    data_df <- data.frame(Date = index(spx), SPX = as.numeric(spx), Predicted_SPX = predictions)

    # Remove any rows with NA predictions
    data_df <- na.omit(data_df)

    # Generate signals: 1 for buy, -1 for sell, 0 for hold
    signals <- ifelse(data_df$Predicted_SPX > data_df$SPX, 1, -1)

    # Calculate daily returns
    data_df$Returns <- ROC(data_df$SPX, type = "discrete")

    # Calculate strategy returns based on signals
    data_df$Strategy_Returns <- lag(signals) * data_df$Returns

    # Remove NA values resulting from lag
    data_df <- na.omit(data_df)

    # Convert strategy returns to time series
    strategy_returns <- xts(data_df$Strategy_Returns, order.by = data_df$Date)

    # Calculate Sharpe ratio
    sharpe_ratio <- SharpeRatio.annualized(strategy_returns)

    return(list(Score = sharpe_ratio, Pred = sharpe_ratio))
}

# Define parameter space
bounds <- list(
    n_particles = c(10L, 200L),          # Number of particles
    obs_noise_var = c(log(1), log(1000)),  # Observation noise variance (log scale)
    alpha = c(0.1, 0.9)                  # Weighting factor for particle update
)

# Bayesian optimization
result <- BayesianOptimization(
    FUN = objective_function,
    bounds = bounds,
    init_points = 10,
    n_iter = 50,
    acq = "ucb",
    kappa = 2.576,
    eps = 0.0
)

# Print the best result
print(result)

# Extract the best params
best_params <- result$Best_Par
n_particles_opt <- round(best_params["n_particles"])
obs_noise_var_opt <- exp(best_params["obs_noise_var"])
alpha_opt <- best_params["alpha"]

# Run model with best params
predictions_opt <- ddpf(spx, n_particles_opt, state_dim = 1, obs_noise_var_opt, alpha_opt)

# Convert data to a dataframe
data_df_opt <- data.frame(Date = index(spx), SPX = as.numeric(spx), Predicted_SPX = predictions_opt)

# Remove NAs
data_df_opt <- na.omit(data_df_opt)

# Plot
ggplot(data_df_opt, aes(x = Date)) +
    geom_line(aes(y = SPX, color = "Actual SPX"), size = 1.2) +
    geom_line(aes(y = Predicted_SPX, color = "Predicted SPX"), size = 1.2) +
    labs(title = "SPX Predictions using Optimized Particle Filter", y = "Price", x = "Date") +
    scale_color_manual("", breaks = c("Actual SPX", "Predicted SPX"), values = c("blue", "red")) +
    theme_minimal()

# Define the optimized trading strategy
signals_opt <- ifelse(data_df_opt$Predicted_SPX > data_df_opt$SPX, 1, -1)

# Calculate daily returns
data_df_opt$Returns <- ROC(data_df_opt$SPX, type = "discrete")

# Calculate strategy returns based on signals
data_df_opt$Strategy_Returns <- lag(signals_opt) * data_df_opt$Returns

# Remove NA values resulting from lag
data_df_opt <- na.omit(data_df_opt)

# Convert strategy returns to time series
strategy_returns_opt <- xts(data_df_opt$Strategy_Returns, order.by = data_df_opt$Date)

# Calculate performance metrics
win_rate_opt <- mean(strategy_returns_opt > 0)
annualized_return_opt <- Return.annualized(strategy_returns_opt)
sharpe_ratio_opt <- SharpeRatio.annualized(strategy_returns_opt)
avg_win_opt <- mean(strategy_returns_opt[strategy_returns_opt > 0])
avg_loss_opt <- mean(strategy_returns_opt[strategy_returns_opt < 0])

# Calculate Kelly criterion
kelly_criterion_opt <- win_rate_opt - (1 - win_rate_opt) / (avg_win_opt / abs(avg_loss_opt))

# Print performance metrics
print(sprintf("Optimized Win Rate: %f", win_rate_opt))
print(sprintf("Optimized Annualized Return: %f", annualized_return_opt))
print(sprintf("Optimized Sharpe Ratio: %f", sharpe_ratio_opt))
print(sprintf("Optimized Average Win: %f", avg_win_opt))
print(sprintf("Optimized Average Loss: %f", avg_loss_opt))
print(sprintf("Optimized Kelly Criterion: %f", kelly_criterion_opt))

# Display a performance summary
charts.PerformanceSummary(strategy_returns_opt)
