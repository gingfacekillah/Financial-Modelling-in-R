# Technical Indicator Study: What technical indicators have any predictive power for next day's open-to-close returns?
# Answer: Pretty much none of them
### | @Gingfacekillah

# Libraries
library(zoo)
library(quantmod)
library(ggplot2)
library(TTR)
library(tidyverse)
library(corrr)

### -- 4 years of SPX data
spx_data <- getSymbols("^GSPC", from = Sys.Date() - 1460, to = Sys.Date(), auto.assign = FALSE)
### -- convert to log returns
c2c_logRet <- round(dailyReturn(Cl(spx_data), method = "log"),3) # close to close log returns
o2c_logRet <- round(log(spx_data$GSPC.Close/spx_data$GSPC.Open),3) # open to close log returns

### -- Technical Indicators from TTR library
# 1. ADX
indicators_df <- ADX(HLC(spx_data), n = 14)
colnames(indicators_df) <- c("ADX_DIP", "ADX_DIN", "ADX_DX", "ADX")
# 2. AROON
aroon <- aroon(spx_data$GSPC.Adjusted)
colnames(aroon) <- c("AROON_UP", "AROON_DOWN", "AROON")
indicators_df <- merge.xts(indicators_df, aroon)
rm(aroon)
# 3. BOLLINGER BANDS
bollinger <- BBands(HLC(spx_data),n = 20, sd = 2)
colnames(bollinger) <- c("BOL_DOWN", "BOL_MA", "BOL_UP", "BOL_PCT")
indicators_df <- merge.xts(indicators_df, bollinger)
rm(bollinger)
# 4. COMMODITY CHANNEL INDEX [CCI]
cci <- CCI(HLC(spx_data), n =  20)
colnames(cci) <- c("CCI")
indicators_df <- merge.xts(indicators_df, cci)
rm(cci)
# 5. CHAIKIN ACCUMULATION/DISTRIBUTION [CHAIKIN AD]
chaikinAD <- chaikinAD(HLC(spx_data), volume = spx_data$GSPC.Volume)
colnames(chaikinAD) <- c("CHAIKIN_AD")
indicators_df <- merge.xts(indicators_df, chaikinAD)
rm(chaikinAD)
# 6. CHAIKIN VOLATILITY
chaikinVOL <- chaikinVolatility(HLC(spx_data), n = 10)
colnames(chaikinVOL) <- c("CHAIKIN_VOL")
indicators_df <- merge.xts(indicators_df, chaikinVOL)
rm(chaikinVOL)
# 7. RATE OF CHANGE [ROC]
roc <- ROC(spx_data$GSPC.Adjusted, n = 1)
colnames(roc) <- c("ROC")
indicators_df <- merge.xts(indicators_df, roc)
rm(roc)
# 8. CLOSE LOCATION VALUE [CLV]
clv <- CLV(HLC(spx_data))
colnames(clv) <- c("CLV")
indicators_df <- merge.xts(indicators_df, clv)
rm(clv)
# 9. CHAIKIN MONEY FLOW
cmf <- CMF(HLC(spx_data), n = 20, volume = spx_data$GSPC.Volume)
colnames(cmf) <- c("CHAIKIN_MF")
indicators_df <- merge.xts(indicators_df, cmf)
rm(cmf)
# 10. CHANDE MOMEMTUM OSCILLATOR
cmo <- CMO(spx_data$GSPC.Adjusted, n = 14)
colnames(cmo) <- c("CHANDE_MO")
indicators_df <- merge.xts(indicators_df, cmo)
rm(cmo)
# 11. DONCHIAN CHANNEL
dc <- DonchianChannel(spx_data$GSPC.Adjusted, n = 10)
colnames(dc) <- c("DONCHIAN_HIGH", "DONCHIAN_MID", "DONCHIAN_LOW")
indicators_df <- merge.xts(indicators_df, dc)
rm(dc)
# 12. DE-TRENDED PRICE OSCILLATOR
# dpo <- DPO(spx_data$GSPC.Adjusted, n = 10)
# colnames(dpo) <- c("DPO")
# indicators_df <- merge.xts(indicators_df, dpo)
# rm(dpo)
# 13. DV INTERMEDIATE OSCILLATOR
dvi <- DVI(spx_data$GSPC.Adjusted, n = 14, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 100, 5),
           stretch = c(10, 100, 2), exact.multiplier = 1)
colnames(dvi) <- c("DVI_MA", "DVI_STR", "DVI")
indicators_df <- merge.xts(indicators_df, dvi)
rm(dvi)
# 14. EASE OF MOVEMENT VALUE
emv <- EMV(HLC(spx_data), volume = spx_data$GSPC.Volume, n = 9)
colnames(emv) <- c("EMV", "EMV_SMOOTH")
indicators_df <- merge.xts(indicators_df, emv)
rm(emv)
# 15. GUPPY MULTIPLE MOVING AVERAGES
# guppy <- GMMA(spx_data$GSPC.Adjusted,
#               short = c(3, 5, 8, 10, 12, 15),
#               long = c(30, 35, 40, 45, 50, 60))
# 16. KNOW SURE THING
kst <- KST(spx_data$GSPC.Adjusted)
colnames(kst) <- c("KST", "KST_SIGNAL")
indicators_df <- merge.xts(indicators_df, kst)
rm(kst)
# 17. MACD OSCILLATOR
macd <- MACD(spx_data$GSPC.Adjusted,  nFast = 12, nSlow = 26, nSig = 9, percent = TRUE)
colnames(macd) <- c("MACD", "MACD_SIGNAL")
indicators_df <- merge.xts(indicators_df, macd)
rm(macd)
# 18. MONEY FLOW INDEX
mfi <- MFI(HLC(spx_data), volume = spx_data$GSPC.Volume, n = 14)
colnames(mfi) <- c("MFI")
indicators_df <- merge.xts(indicators_df, mfi)
rm(mfi)
# 19. MOVING AVERAGES
### -- SMA
indicators_df$SMA8 <- SMA(spx_data$GSPC.Adjusted, n =8)
indicators_df$SMA13 <- SMA(spx_data$GSPC.Adjusted, n =13)
indicators_df$SMA21 <- SMA(spx_data$GSPC.Adjusted, n =21)
### -- EMA
indicators_df$EMA8 <- EMA(spx_data$GSPC.Adjusted, n =8)
indicators_df$EMA13 <- EMA(spx_data$GSPC.Adjusted, n =13)
indicators_df$EMA21 <- EMA(spx_data$GSPC.Adjusted, n =21)
### -- ZLEMA
indicators_df$ZLEMA8 <- ZLEMA(spx_data$GSPC.Adjusted, n =8)
indicators_df$ZLEMA13 <- ZLEMA(spx_data$GSPC.Adjusted, n =13)
indicators_df$ZLEMA21 <- ZLEMA(spx_data$GSPC.Adjusted, n =21)
### -- HMA
indicators_df$HMA8 <- HMA(spx_data$GSPC.Adjusted, n =8)
indicators_df$HMA13 <- HMA(spx_data$GSPC.Adjusted, n =13)
indicators_df$HMA21 <- HMA(spx_data$GSPC.Adjusted, n =21)
### -- VWAP
indicators_df$VWAP8 <- VWAP(spx_data$GSPC.Adjusted,volume = spx_data$GSPC.Volume, n =8)
indicators_df$VWAP13 <- VWAP(spx_data$GSPC.Adjusted,volume = spx_data$GSPC.Volume, n =13)
indicators_df$VWAP21 <- VWAP(spx_data$GSPC.Adjusted,volume = spx_data$GSPC.Volume, n =21)
### -- ALMA
indicators_df$ALMA8 <- ALMA(spx_data$GSPC.Adjusted, n =8)
indicators_df$ALMA13 <- ALMA(spx_data$GSPC.Adjusted, n =13)
indicators_df$ALMA21 <- ALMA(spx_data$GSPC.Adjusted, n =21)
# 20. ON BALANCE VOLUME [OBV]
indicators_df$OBV <- OBV(spx_data$GSPC.Adjusted, volume = spx_data$GSPC.Volume)
# 21. PRICE BANDS
price_bands <- PBands(spx_data$GSPC.Adjusted, n =20)
colnames(price_bands) <- c("PB_DOWN", "PB_CENTER", "PB_UP")
indicators_df <- merge.xts(indicators_df, price_bands)
rm(price_bands)
# 22. RSI
indicators_df$RSI <- RSI(spx_data$GSPC.Adjusted, n = 14)
# 23. STOCHASTIC OSCILLATOR
stoch <- stoch(HLC(spx_data))
colnames(stoch) <- c("STOCH_FK", "STOCH_FD", "STOCH_SD")
indicators_df <- merge.xts(indicators_df, stoch)
rm(stoch)
# 24. TREND DETECTION INDEX
tdi <- TDI(spx_data$GSPC.Adjusted)
colnames(tdi) <- c("TDI", "TDI_DI")
indicators_df <- merge.xts(indicators_df, tdi)
rm(tdi)
# 25. TRIPLE SMOOTHED EXPONENTIAL OSCILLATOR
trix <- TRIX(spx_data$GSPC.Adjusted)
colnames(trix) <- c("TRIX", "TRIX_SIGNAL")
indicators_df <- merge.xts(indicators_df, trix)
rm(trix)
# 26. VERTICAL HORIZON FILTER
indicators_df$VHF <- VHF(spx_data$GSPC.Adjusted)
# 27. ULTIMATE OSCILLATOR
indicators_df$ULTIMATE_OSC <- ultimateOscillator(HLC(spx_data))
# 28. WILLIAMS ACCUMULATION/DISTRIBUTION
indicators_df$WILLIAMS_AD <- williamsAD(HLC(spx_data))
# 29. WILLIAM'S %R
indicators_df$WILLIAMS_R <- WPR(HLC(spx_data), n = 14)
#####################################################

### -- Add Log Returns
indicators_df$logRets <- o2c_logRet$GSPC.Close
indicators_df$logRets <- lead(indicators_df$logRets, 1)
indicators_df <- na.omit(indicators_df)

### -- Correlations
corr_chart <- indicators_df %>% correlate()
indicators_df %>% correlate()
# Compute the correlation matrix
cor_matrix <- cor(indicators_df)
# Set the correlation threshold
threshold <- 0.01
# Find variables with correlation greater than the threshold with the target variable
high_corr_vars <- names(which(abs(cor_matrix[,"logRets"]) > threshold & colnames(cor_matrix) != "logRets"))
indicators_df <- as.data.frame(indicators_df)
# Create a new data frame with only the selected variables using tidyverse functions
filtered_data <- indicators_df %>%
    dplyr::select(all_of(c("logRets", high_corr_vars)))

final_corr_chart <- filtered_data %>% correlate()
final_corr_chart
