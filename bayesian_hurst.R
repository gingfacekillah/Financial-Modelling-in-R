### -- Estimation of Hurst Exponent for different timeframes with MCMC Simulation | @Gingfacekillah
### -- The Hurst exponent is referred to as the "index of dependence" or "index of long-range dependence". 
### -- It quantifies the relative tendency of a time series either to regress strongly to the mean or to cluster in a direction. [Wikipedia]

### -- Libraries
library(HKprocess)
library(quantmod)
library(ggplot2)

### -- Get data
spx_data <- getSymbols("^GSPC", from = "2023-10-01", to = Sys.Date(), auto.assign = FALSE)
spx_returns <- dailyReturn(Cl(spx_data), method = "log")

### -- Estimate Hurst exponent with MCMC (Metropolis-Hastings)
sim <- inferHmetrop(spx_returns, theta.init=0.7, burnin = 500, mcmc = 20000, thin = 1,
             tune = 1,verbose = 1,seed = NA)

### -- Histogram of Hurst exponent posterior
hist(sim, breaks = 20, main = "Hurst Exponent Posterior", xlab = "H")
abline(v = median(sim), col = "red", lty = 4) # Median line
median(sim)

### -- Estimate Mu & Sigma from sampling
mean_sd <- infermsmetrop(sim, spx_returns)

### -- Histogram of Mu
hist(mean_sd[,1], breaks = 20, main = expression(paste("Histogram of ",mu)),
     xlab = expression(paste(mu)))

### -- Histogram of Sigma
hist(sqrt(mean_sd[,2]), breaks = 20,
     main = expression(paste("Histogram of ",sigma)),
     xlab = expression(paste(sigma)))


### -- Create a data frame for ggplot
data <- data.frame(Hurst = sim)

### -- Plot the histogram
ggplot(data, aes(x = Hurst)) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "white", alpha = 0.7) +
    labs(title = "Hurst Exponent Posterior",
         x = "Hurst Exponent",
         y = "Frequency") +
    theme_minimal() +
    geom_vline(xintercept = median(sim), linetype = "dashed", color = "red")

### -- Calculate the probability that the true value is over 0.5
probability_over_0.5 <- length(sim[sim > 0.5]) / length(sim)
print(paste("Probability that true Hurst value is over 0.5:", probability_over_0.5))

### -- Hurst Estimation Function
hurst <- function(data){
    inferHmetrop(data, theta.init=0.7, burnin = 500, mcmc = 20000, thin = 1,
                 tune = 1,verbose = 1,seed = NA)
}

### -- Estimate Hurst exponent for different time periods
# Get data for 1 month, 3 months, 6 months, and 1 year
spx_data_1_month <- getSymbols("^GSPC", from = Sys.Date() - 30, to = Sys.Date(), auto.assign = FALSE)
spx_data_3_months <- getSymbols("^GSPC", from = Sys.Date() - 90, to = Sys.Date(), auto.assign = FALSE)
spx_data_6_months <- getSymbols("^GSPC", from = Sys.Date() - 180, to = Sys.Date(), auto.assign = FALSE)
spx_data_12_months <- getSymbols("^GSPC", from = Sys.Date() - 365, to = Sys.Date(), auto.assign = FALSE)

### -- Convert daily returns to log returns
spx_returns_1_month <- dailyReturn(Cl(spx_data_1_month), method = "log")
spx_returns_3_months <- dailyReturn(Cl(spx_data_3_months), method = "log")
spx_returns_6_months <- dailyReturn(Cl(spx_data_6_months), method = "log")
spx_returns_12_months <- dailyReturn(Cl(spx_data_12_months), method = "log")

### -- Estimate Hurst exponent for each time period
sim_1_month <- hurst(spx_returns_1_month)
sim_3_months <- hurst(spx_returns_3_months)
sim_6_months <- hurst(spx_returns_6_months)
sim_12_months <- hurst(spx_returns_12_months)

### -- Extract median
sim_1_month %>% median()
sim_3_months %>% median()
sim_6_months %>% median()
sim_12_months %>% median()

### -- Create a new data frame
hurst_data <- data.frame(one_month = sim_1_month, three_months = sim_3_months, six_months = sim_6_months, sim_12_months = sim_12_months)

### -- Print the new data frame
print(hurst_data)

### -- Plot histogram with overlay and vertical median lines
ggplot() +
    geom_histogram(aes(x = hurst_data$one_month, fill = "one_month"), alpha = 0.5) +
    geom_histogram(aes(x = hurst_data$three_months, fill = "three_months"), alpha = 0.5) +
    geom_histogram(aes(x = hurst_data$six_months, fill = "six_months"), alpha = 0.5) +
    geom_histogram(aes(x = hurst_data$sim_12_months, fill = "twelve_months"), alpha = 0.5) +
    geom_vline(xintercept = median(hurst_data$one_month), color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median(hurst_data$three_months), color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median(hurst_data$six_months), color = "green", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median(hurst_data$sim_12_months), color = "purple", linetype = "dashed", size = 1) +
    labs(title = "MCMC Hurst Exponent by Period", x = "Hurst Exponent", y = "Frequency")

### -- Calculate and print the probabilities for each time period
print(paste("Probability that true Hurst value is over 0.5 (1 Month):", length(sim_1_month[sim_1_month > 0.5]) / length(sim_1_month)))
print(paste("Probability that true Hurst value is over 0.5 (3 Months):", length(sim_3_months[sim_3_months > 0.5]) / length(sim_3_months)))
print(paste("Probability that true Hurst value is over 0.5 (6 Months):", length(sim_6_months[sim_6_months > 0.5]) / length(sim_6_months)))
print(paste("Probability that true Hurst value is over 0.5 (12 Months):", length(sim_12_months[sim_12_months > 0.5]) / length(sim_12_months)))
