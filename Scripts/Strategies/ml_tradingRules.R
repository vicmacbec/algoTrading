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
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
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
  pair <- "BTCBUSD"
}else{
  pair <- args[1]
}

print(paste0("Pair: ", pair))

# # Cada 4 horas
# days <- 2000
# date <-  as.Date("2022-11-08") # Sys.Date() # Last day
# klines <- NULL
# for(day in days:0){
#   tmp <- binance_klines(pair, limit = 24/4, interval = '4h', start_time = date - day)
#   klines <- rbind(klines, tmp) %>% unique()
# }
# klines

# Diario
years <- 10
date <-  as.Date("2022-11-08") # Sys.Date() # Last day
klines <- NULL
for(year in years:0){
  tmp <- binance_klines(pair, limit = 370, interval = '1d', start_time = date - year * 370
  )[, .(symbol, open_time, open, high, low, close, volume, close_time, trades)]
  klines <- rbind(klines, tmp) %>% unique()
}
klines


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
# Quizás cambiar target 1 si es que es más grande por 10 % de su valor inicial anterior
# Quizás solo quedarnos con los registros que están en cierta cantidad de precio
klines2[, close_lead := shift(close, type = "lead")]
klines2[, target := ifelse(close_lead > (1.02 * close), "1", "0")]
table(klines2[, target])

#### Features relations ####
klines2[, 
        .(symbol, open_time, close_time, 
          open, high, low, close, volume, trades, 
          ohlc4, ema, atr, maSlope, rollmean05, rollmean10, rollmean20, rollmean51,
          dn, up, pctB, macd, signal, macdChange, RSI, shortStopLoss, longStopLoss, 
          target)
        ] %>% na.omit() -> klines3


#### Feauter selection ####
# All features
features1 <- c("open", "high", "low", "close", "volume", "trades", 
               "ohlc4", "ema", "atr", "maSlope", 
               "rollmean05", "rollmean10", "rollmean20", "rollmean51",
               "dn", "up", "pctB", "macd", "signal", "RSI", "shortStopLoss", "longStopLoss")
# Half most important features
features2 <- c("open", "volume", "trades", 
               "atr", "maSlope", 
               # "dn", "up", 
               "pctB", "macd", "signal", "RSI")
features <- features2


ggpairs(klines3, columns = features, 
        aes(color = target, alpha = 0.5)
        # lower = list(continuous = "smooth")
        )


#### XGBoost ####

# Split train test data
# sample <- sample(c(TRUE,FALSE), nrow(klines3), replace=TRUE, prob=c(0.7,0.3))
sample <- createDataPartition(klines3[, target], p = 0.7, list = FALSE)

x_train <- klines3[sample, ..features]
y_train <- klines3[sample, target]
x_test <- klines3[-sample, ..features]
y_test <- klines3[-sample, target]

y_train %>% table()
y_test %>% table()


# Scaling data
# To get the column names into the lapply .SD function
# https://stackoverflow.com/questions/59074426/using-sd-column-names-in-lapply-with-data-tables
x_test[, c(features) := lapply(seq_along(names(.SD)), 
                               function(y, n, i){ (y[[i]] - mean(unlist(x_train[, n[[i]], with = FALSE]))) / sd(unlist(x_train[, n[[i]], with = FALSE])) },
                               y = .SD,
                               n = names(.SD)), 
       .SDcols = features]
x_train[, c(features) := lapply(.SD, function(x){scale(x = x)}), .SDcols = features]


xgboost_train <- xgb.DMatrix(data = data.matrix(x_train), label = y_train)
xgboost_test <- xgb.DMatrix(data = data.matrix(x_test), label = y_test)


# Comparing train and test distributions
features
feature <- features[2]
plot(ecdf(x_train[, ..feature][[1]]), main = feature)
lines(ecdf(x_test[, ..feature][[1]]), col = 'blue')


# Model
# xgboost::xgb.cv()

watchlist <- list(train = xgboost_train, test = xgboost_test)
model <- xgb.train(data = xgboost_train,
                   objective = "binary:logistic",
                   # nthread = 2, # threads of the computer
                   max.depth = 5,
                   eta = 0.001, # learning rate (gradient, [0,1]) https://machinelearningmastery.com/tune-learning-rate-for-gradient-boosting-with-xgboost-in-python/
                   nrounds = 10000,
                   # early_stopping_rounds = 10,
                   lambda = 10,
                   watchlist = watchlist
                   # tree_method = "gpu_hist"
                   )

curves <- rbind(model$evaluation_log[, .(iter, type = "train", values = train_error)],
                model$evaluation_log[, .(iter, type = "test", values = test_error)])
p <- ggplot(curves, aes(x = iter, y = values, color = type)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Features: All")
  # ggtitle("Features: Using feature importance")
p %>% ggplotly()


xgboost::xgb.importance(model = model)
xgboost::xgb.plot.importance(xgb.importance(model = model))


# Predictions
pred_test <- predict(model, xgboost_test)
pred_test

# Confussion Matrix

confusionMatrix(data = ifelse(pred_test > 0.5, 1, 0) %>% factor(levels = c(0, 1)),
                reference = y_test %>% factor(levels = c(0, 1)),
                positive = "1")
