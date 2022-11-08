#### Script information ####
#
# Trade strategy #
#
# https://www.youtube.com/watch?v=RATy3xedqPs
#
# Productive
#
# Time: ~8 seconds
#
# Example:
# Rscript ~/Drive/Codigos/AlgoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.R $symbol >> ~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/allLogs/$(date '+%Y_%m_%d_%H:%M:%S')_$symbol.log
# Rscript ~/algoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.R $symbol >> ~/algoTrading/DataOut/MASlope_ATRStopLoss/allLogs/$(date '+%Y_%m_%d_%H:%M:%S')_$symbol.log


#### Load libraries ####
print("#### Load libraries ####")
suppressMessages(library(binancer))
# suppressMessages(library(keyring))
# suppressMessages(library(ggplot2))
# suppressMessages(library(scales))
suppressMessages(library(data.table))
suppressMessages(library(TTR))
suppressMessages(library(stringr))
# suppressMessages(library(zoo))
suppressMessages(library(plotly))
# suppressMessages(library(purrr))
# suppressMessages(library(roll))
suppressMessages(library(patchwork))
suppressMessages(library(lubridate))
library(GGally)
library(xgboost)
library(caret)


#### Functions ####
print("#### Functions ####")


#### Paths ####
pathDataInAllOrders <- "~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/Orders/"
pathDataInAllData <- "~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/Trades/"
pathDataInYml <- "~/Drive/Codigos/AlgoTrading/"
# pathDataInAllOrders <- "~/algoTrading/DataOut/MASlope_ATRStopLoss/Orders/"
# pathDataInAllData <- "~/algoTrading/DataOut/MASlope_ATRStopLoss/Trades/"
# pathDataInYml <- "~/algoTrading/"
# pathDataOut


#### Credentials ####
kp <- config::get(value = "Binance", file = paste0(pathDataInYml, "config.yml"))
binance_credentials(key = kp$key, secret = kp$secret)


#### Load data ####
print("#### Load data ####")
# binance_coins()
# binance_coins_prices() #%>% View
# binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs
allOrders <- fread(paste0(pathDataInAllOrders, "allOrders_year_20220421.csv"))
allData <- fread(paste0(pathDataInAllData, "allData_year_4h_20220421.csv"))


#### Initial parameters ####
print("#### Initial parameters ####")

# maSlope constant
rad2degree <- 180/3.14159265359
# Stop Loss constant
multiplier <- 1.5

# BUSDpairs %>% data.table() #%>% View
# allData <- NULL
# allOrders <- NULL
# summaryAll_all <- NULL

# Last day:     Sys.Date() 
date <- Sys.Date() #as.Date("2022-01-10") 
limit <- 1000
days <- 365
fee <- 0.00075

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  pair <- "FXSBUSD"
}else{
  pair <- args[1]
}

print(paste0("Pair: ", pair))

klines <- allData[symbol == pair, .(symbol, open_time, open, high, low, close, volume, close_time, trades)]

difftime(floor_date(Sys.time() + 4 * 3600, unit = "hour"), Sys.time(), units = "secs") #%>% Sys.sleep()

# klines <- binance_klines(pair, limit = limit, interval = '5m') # 'BTCUSDT'
# klines <- binance_klines(pair, limit = limit, interval = '5m', start_time = '2021-12-03')
tmp <- binance_klines(pair, limit = 400, interval = '4h'
                      )[, .(symbol, open_time, open, high, low, close, volume, close_time, trades)]
# Do not add 5 hours because rbind bind the POSIXct columns in the same zone 
# tmp[, ':='(open_time = open_time + 5*60*60, close_time = close_time + 5*60*60)] # Add 7 hours to match with TrendingView

tmp[.N, open_time]
klines[.N, open_time]
rbind(klines, tmp)[.N, open_time]
klines <- rbind(klines, tmp)[order(open_time, -trades)] %>% distinct(open_time, .keep_all = TRUE)
# klines[order(open_time)]


print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
print("")


#### Rules ####
print("#### Rules ####")

an <- function(n, len){
  if(len-n > 0){
    c(seq.int(n), rep(n, len-n)) 
  }else{
    seq.int(len)
  }
}


# Moving Average Slope #
klines[, ohlc4 := rowSums(.SD)/4, .SDcols = c("open", "high", "low", "close")] %>% suppressWarnings()

# EMA of ohlc4 #
klines[, ema := EMA(ohlc4, 55)]
klines[, ema_lag :=  c(NA, ema[-.N])] # ema ohlc4 lag

# ATR #
klines[, c("tr", "atr", "trueHigh", "trueLow") := ATR(klines[, .(high, low, close)], 14) %>% data.table()]

# MA Slope #
klines[, maSlope := rad2degree*atan((ema - ema_lag)/atr)]
klines2 <- klines[, .(symbol, open_time, open, high, low, close, volume, close_time, trades, 
                      ohlc4, ema, atr, maSlope)]

# Moving average #
klines2[, rollmean05 := frollmean(close, n = an(5, .N), adaptive = TRUE)]
klines2[, rollmean10 := frollmean(close, n = an(10, .N), adaptive = TRUE)]
klines2[, rollmean20 := frollmean(close, n = an(20, .N), adaptive = TRUE)]
klines2[, rollmean51 := frollmean(close, n = an(51, .N), adaptive = TRUE)]

# Bollinger Bands #
# drop mavg because is the same than rollmean20
klines2[, c("dn", "up", "pctB") := (BBands(klines2[, .(close)]) %>% data.table())[, .(dn, up, pctB)]] 

# MACD #
klines2[, c("macd", "signal") := (MACD(klines2[, .(close)]) %>% data.table())]
klines2[, macdChange := ifelse(macd < signal, "down", "up")]

# RSI #
# By defaul, the MA is 14, but binance graphics take it on 6
klines2[, c("RSI") := (RSI(klines2[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)]] 

# Wilder’s Smoothing #

# Average Directional Index #

# Stochastic Oscillators #

# Rate of Change #


# ATR Stop Loss #
klines2[, shortStopLoss := close + atr * multiplier]
klines2[, longStopLoss := close - atr * multiplier]

# Target #
klines2[, close_lead := shift(close, type = "lead")]
klines2[, target := ifelse(close_lead > close, "1", "0")]


#### Features relations ####
klines2[, 
        .(symbol, open_time, close_time, 
          open, high, low, close, volume, trades, 
          ohlc4, ema, atr, maSlope, rollmean05, rollmean10, rollmean20, rollmean51,
          dn, up, pctB, macd, signal, macdChange, RSI, shortStopLoss, longStopLoss, 
          target)
        ] %>% na.omit() -> klines3


ggpairs(klines3, columns = 4:26, 
        aes(color = target, alpha = 0.5)
        # lower = list(continuous = "smooth")
        )


#### XGBoost ####

# Split train test data
sample <- sample(c(TRUE,FALSE), nrow(klines3), replace=TRUE, prob=c(0.7,0.3))

x_train <- klines3[sample, 
                   .(open, high, low, close, volume, trades, 
                     ohlc4, ema, atr, maSlope, rollmean05, rollmean10, rollmean20, rollmean51,
                     dn, up, pctB, macd, signal, RSI, shortStopLoss, longStopLoss)]
y_train <- klines3[sample, target]
x_test <- klines3[!sample, 
                  .(open, high, low, close, volume, trades, 
                    ohlc4, ema, atr, maSlope, rollmean05, rollmean10, rollmean20, rollmean51,
                    dn, up, pctB, macd, signal, RSI, shortStopLoss, longStopLoss)]
y_test <- klines3[!sample, target]

y_train %>% table()
y_test %>% table()

xgboost_train <- xgb.DMatrix(data = data.matrix(x_train), label = y_train)
xgboost_test <- xgb.DMatrix(data = data.matrix(x_test), label = y_test)

# Model
xgboost::xgb.cv()

watchlist <- list(train = xgboost_train, test = xgboost_test)
model <- xgb.train(data = xgboost_train,
                   objective = "binary:logistic",
                   max.depth = 3,
                   eta = 1,
                   nthread = 2,
                   nrounds = 1000,
                   early_stopping_rounds = 10,
                   watchlist = watchlist
                   )
summary(model)


xgboost::xgb.importance(model = model)
xgboost::xgb.plot.importance(xgb.importance(model = model))

# Prdeictions
pred_test <- predict(model, xgboost_test)
pred_test

# Confussion Matrix

confusionMatrix(data = ifelse(pred_test > 0.5, 1, 0) %>% factor(levels = c(0, 1)),
                reference = y_test %>% factor(levels = c(0, 1)),
                positive = "1")
