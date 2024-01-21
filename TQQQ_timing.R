# TQQQ Timing with technical indicators | @Gingfacekillah
### -- Long only, no shorting with PSQ

# Suppress all package startup messages
suppressPackageStartupMessages({
    library(PerformanceAnalytics)   # Strategy backtesting metrics
    library(quantmod)               # stock data & technical analysis
    library(tidymodels)             # Tidymodels
    library(tidyverse)              # Core tidyverse functions
    library(tidymodels)             # Tidymodels
    library(usemodels)              # Template code suggestions for ML models
    library(corrr)                  # Correlation
    library(ggplot2)                # Plots
    library(parsnip)                # ML Engines (random forest)
    library(workflows)              # Modeling workflows
    library(recipes)                # Preprocessing recipes
    library(rsample)                # Resampling
    library(tune)                   # Parameter tuning
    library(dials)                  # Adjustments
    library(yardstick)              # Evaluation metrics
    library(vip)                    # Variable importance
    library(MASS)                   # Sample data
    library(shapviz)                # Shap charts
    library(lubridate)              # Dates & times wrangling
    library(zoo)                    # Rollmeans & cumsum
})

# Clear environment
rm(list = ls(all = TRUE))

### 1. -- Download financial data
getSymbols("TQQQ", from = "2011-01-01", to = "2016-01-01")
data <- TQQQ
data$ma_data <- SMA(Cl(data), n = 10)

### 2. -- Add target variable
data$price <- lag(data$ma_data, 2) - data$ma_data # lag1 originally
data$class <- as.factor(ifelse(data$price > 0, 1, 0))

### 3. -- Technical Analysis feature engineering [TTR]
addTechnicalIndicators <- function(data) {
    cci <- CCI(HLC(data), n = 20, c = 0.015)
    cmf <- CMF(HLC(data), Vo(data), n = 20)
    cmo <- CMO(Cl(data), n = 14)
    dvi <- DVI(Cl(data))$dvi
    macd <- MACD(Cl(data), nFast = 12, nSlow = 26, nSig = 9)$macd
    mfi <- MFI(HLC(data), Vo(data), n = 14)
    obv <- OBV(Cl(data), Vo(data))
    MOMO <- ROC(Cl(data))
    rsi <- RSI(Cl(data), n = 14)
    Stoch <- stoch(HLC(data), nFastK = 14, nFastD = 3, nSlowD = 3)
    fastk <- Stoch$fastK
    fastd <- Stoch$fastD
    slowd <- Stoch$slowD
    tdi <- TDI(Cl(data), n = 20, multiple = 2)$tdi
    williamsad <- williamsAD(HLC(data))
    wpr <- WPR(HLC(data), n = 14)
    bol <- BBands(data[, c(2,3,4)])
    bol_pct <- bol$pctB
    emv <- EMV(data[, c(2, 3)], data[, 5])
    emv_raw <- emv$emv
    emv_smooth <- emv$maEMV
    # Add indicators to the original data
    data <- cbind(data, cci, cmf, cmo, dvi, macd, mfi,obv, MOMO,
                  rsi, fastk, fastd, slowd, tdi, williamsad, wpr,
                  bol_pct,emv_raw,emv_smooth)

    # Rename columns
    colnames(data)[10:ncol(data)] <- c("CCI", "CMF", "CMO", "DVI", "MACD", "MFI", "OBV", "MOMO",
                                       "RSI", "FastK", "FastD", "SlowD", "TDI", "WilliamsAD", "WPR",
                                       "BOLPCT", "EMV", "EMV_SMOOTH")

    return(data)
}
data <- addTechnicalIndicators(data)

### -- 4. Wrangle, splits, resamples
data <- as.data.frame(data) %>%
    mutate(class = as.factor(class))
data$class <- lag(data$class, n=2)
data <- data %>% drop_na()
#-- Remove columns we don't want
columns_to_remove_indices <- c(1:8)
data <- data[, -columns_to_remove_indices]
#-- Partition data
set.seed(123)
df_split <- initial_split(data, strata = class)
model_train <- training(df_split)
model_test <- testing(df_split)
#-- Bootstrap resamples
set.seed(123)
data_resamples <- bootstraps(model_train, strata = class)
data_resamples

### -- 5. Models
#-- Random forest
rf_spec <- rand_forest() %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("classification")
#-- PCA recipe - normalize and perform PCA
pca_recipe <-
    recipe(class ~ ., data = model_train) %>%
    step_normalize() %>%
    step_pca()
#-- Model workflow
model_workflow <- workflow() %>%
    add_recipe(pca_recipe)
#-- Random forest
rf_results <- model_workflow %>%
    add_model(rf_spec) %>%
    fit_resamples(
        resamples = data_resamples,
        control = control_resamples(save_pred = TRUE, verbose = TRUE))

### -- 6. Training results
collect_metrics(rf_results)
#-- Confusion matrix
rf_results %>% conf_mat_resampled()
#-- ROC
rf_results %>% collect_predictions() %>%
    group_by(id) %>%
    roc_curve(class, .pred_1) %>%
    autoplot()

### -- 7. Fit & tune final model
rf_mod <- rand_forest(
    mtry = tune(), min_n = tune(), trees = 1000) %>%
    set_engine("ranger") %>%
    set_mode("classification")

# PCA recipe - normalize and perform PCA
pca_recipe <- recipe(class~ ., data = model_train) %>%
    step_normalize() %>%
    step_pca()
#-- model workflow
rf_workflow <- workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(pca_recipe)
rf_mod
#-- Tune model parameters with grid search
extract_parameter_set_dials(rf_mod)
set.seed(123)
rf_res <- rf_workflow %>%
    tune_grid(data_resamples,
              grid = 25,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(roc_auc))
#-- Show results & select best parameters
rf_res %>% show_best(metric = "roc_auc")
autoplot(rf_res)
#-- Select best parameters
rf_best <- rf_res %>%
    select_best(metric = "roc_auc")
rf_best
rf_res %>% collect_predictions()
#-- Fit the final model
last_rf_mod <- rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = 1000) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
last_rf_workflow <- rf_workflow %>%
    update_model(last_rf_mod)
set.seed(123)
last_rf_fit <- last_rf_workflow %>%
    last_fit(df_split)
last_rf_fit$.workflow
last_rf_fit %>% collect_metrics()
#-- Show variable importance
last_rf_fit %>% extract_fit_parsnip() %>%
    vip(num_features = 30)
#-- Save final model as RDS
final_fit <- last_rf_fit %>% extract_fit_parsnip()
saveRDS(final_fit, "final_fit.RDS")

### -- 8. Produce ML predictions, convert to trade signal, calculate cumsum returns
#-- Create test data and merge signals with actual returns
getSymbols("TQQQ", from = "2017-01-01", to = as.Date(Sys.Date()))
getSymbols("QQQ", from = "2017-01-01", to = as.Date(Sys.Date()))
qqq_data <- QQQ
predict_data <- TQQQ
predict_data$ma_data <- SMA(Cl(predict_data), n = 10)
predict_data$price <- lag(predict_data$ma_data, 2) - predict_data$ma_data #lag1 originally
predict_data$class <- as.factor(ifelse(predict_data$price > 0, 1, 0))
predict_data <- addTechnicalIndicators(predict_data)
predict_data$returns <- dailyReturn(predict_data$TQQQ.Adjusted)
qqq_returns <- dailyReturn(qqq_data$QQQ.Adjusted)
predict_data <- as.data.frame(predict_data) %>%
    mutate(class = as.factor(class))
predict_data$class <- lag(predict_data$class, n=2)
predict_data <- predict_data %>% drop_na()
#-- Extract probabilities
rf_test_probabilities <- predict(final_fit, new_data = predict_data, type = "prob")
#-- Create a data frame with signals based on predicted probabilities
signals <- data.frame(date = index(predict_data),
                      signal = ifelse(rf_test_probabilities$.pred_1 > 0.5, 1, 0)) # [0.5, 1, -1] if long/short
returns <- data.frame(signals, predict_data$returns, rownames(predict_data))
#-- Create a column with the strategy returns
returns$strategy_return <- returns$signal * lag(returns$predict_data.returns)
returns <- na.omit(returns)
#-- Cumulative strategy returns
returns$cum_strategy_return <- cumsum(returns$strategy_return)
columns_to_remove_indices <- c(1,3)
returns <- returns[, -columns_to_remove_indices]
colnames(returns) <- c("signal", "date", "strategy_return", "cumsum_return")
#-- Plot cumulative strategy returns
plot(returns$cumsum_return, main = "ML Algo Strategy Returns", col = "blue", type = "l", ylab = "Returns", xlab = "Days")

### -- 9. Performance metrics
returns$date <- as_date(returns$date, format = "%Y-%m-%d")
#-- Convert returns to xts
returns_xts <- xts(returns$strategy_return, order.by = returns$date)
cumsum_xts <- xts(returns$cumsum_return, order.by = returns$date)
#-- Calculate performance metrics
Performance_xts <- function(x) {
    cumRetx <- Return.cumulative(x)
    annRetx <- Return.annualized(x, scale = 252)
    sharpex <- SharpeRatio.annualized(x, scale = 252)
    winpctx <- sum(x > 0) / sum(x != 0)
    annSDx <- sd.annualized(x, scale = 252)

    DDs <- findDrawdowns(x)
    maxDDx <- min(DDs$return)
    maxLx <- max(DDs$length)

    Perf <- c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
    names(Perf) <- c("Cumulative Return", "Annual Return", "Annualized Sharpe Ratio",
                     "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
    return(Perf)
}
#-- Create TQQQ returns, convert to xts
tqqq_returns_df <- data.frame(date = as.Date(rownames(predict_data)), tqqq_returns = predict_data$returns)
tqqq_returns_xts <- xts(tqqq_returns_df$tqqq_returns, order.by = as.Date(tqqq_returns_df$date))
qqq_returns_xts <- xts(qqq_returns$daily.returns)

#-- Calculate performance metrics for strategy vs QQQ/TQQQ
myReturns_xts <- xts(returns$strategy_return, order.by = returns$date)
cbind(Mack = Performance_xts(myReturns_xts), TQQQ = Performance_xts(tqqq_returns_xts), QQQ = Performance_xts(qqq_returns_xts))
#-- Plots
colnames(myReturns_xts) <- "Mack_Timing_Strategy"
colnames(tqqq_returns_xts) <- "TQQQ_Buy_Hold"
colnames(qqq_returns_xts) <- "QQQ_Buy_Hold"
charts.PerformanceSummary(cbind(myReturns_xts,tqqq_returns_xts, qqq_returns_xts), colorset = rich6equal)
charts.RollingPerformance(cbind(myReturns_xts, tqqq_returns_xts), colorset = c("red", "green"), lwd = 2)
table.AnnualizedReturns(myReturns_xts)
table.Distributions(myReturns_xts)
table.RollingPeriods(myReturns_xts)
table.CAPM(myReturns_xts, tqqq_returns_xts)

# Tomorrow's prediction
predict_data %>%
    dplyr::select(class) %>%
    slice_tail(n =1)
rf_test_probabilities %>%
    slice_tail(n =1) %>%
    rename(up = .pred_1, down = .pred_2)
