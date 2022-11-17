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
library(pROC)


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
# kp <- config::get(value = "Binance", file = paste0(pathDataInYml, "config.yml"))
# binance_credentials(key = kp$key, secret = kp$secret)


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
# date <- Sys.Date() # as.Date("2022-11-08") # Sys.Date() # Last day
# klines <- NULL
# for(day in days:0){
#   tmp <- binance_klines(pair, limit = 24/4, interval = '4h', start_time = date - day)
#   klines <- rbind(klines, tmp) %>% unique()
# }
# klines

# Diario
years <- 10
date <-  Sys.Date() # as.Date("2022-11-08") # Sys.Date() # Last day
klines <- NULL
for(year in years:0){
  tmp <- binance_klines(pair, limit = 370, interval = '1d', start_time = date - year * 370
  )[, .(symbol, open_time, open, high, low, close, volume, close_time, trades)]
  klines <- rbind(klines, tmp) %>% unique()
}
klines

# Creating the target
klines[, high7Days := frollapply(high, n = 7, FUN = max, align = "left")]
klines[, increment := (high7Days - open) / open]
# The 50 % of the week increments, are high by 5 %
klines[, .(percentil = seq(0, 1, 0.1),
           value = increment %>% quantile(probs = seq(0, 1, 0.1), na.rm = TRUE))]


# # Week
# years <- 10
# date <-  Sys.Date() # as.Date("2022-11-08") # Sys.Date() # Last day
# klines <- NULL
# for(year in years:1){
#   tmp <- binance_klines(pair, limit = 53, interval = '1w', start_time = date - year * 370
#                         )[, .(symbol, open_time, open, high, low, close, volume, close_time, trades)]
#   klines <- rbind(klines, tmp) %>% unique()
# }
# klines


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
klines2 <- klines[, .(symbol, open_time, open, high, low, close, volume, close_time, trades, increment, 
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
# # Quizás cambiar target 1 si es que es más grande por 10 % de su valor inicial anterior
# # Quizás solo quedarnos con los registros que están en cierta cantidad de precio
# klines2[, close_lead := shift(close, type = "lead")]
# klines2[, target := ifelse(close_lead > (1.02 * close), "1", "0")]
# table(klines2[, target])
klines2[, target := ifelse(increment > 0.05, "1", "0")]
table(klines2[, target])


#### Features relations ####
klines2[, 
        .(symbol, open_time, close_time, 
          open, high, low, close, volume, trades, 
          ohlc4, ema, atr, maSlope, rollmean05, rollmean10, rollmean20, rollmean51,
          dn, up, pctB, macd, signal, macdChange, RSI, shortStopLoss, longStopLoss, 
          target)
        ] %>% na.omit() -> klinesTmp1

# klines3 <- rbind(klinesTmp1, klinesTmp2)
klines3 <- klinesTmp1


#### Feature selection ####
# All features
features1 <- c("open", "high", "low", "close", "volume", "trades", 
               "ohlc4", "ema", "atr", "maSlope", 
               "rollmean05", "rollmean10", "rollmean20", "rollmean51",
               "dn", "up", "pctB", "macd", "signal", "RSI", "shortStopLoss", "longStopLoss")
# Half most important features
features2 <- c("open", "volume", "trades", 
               "atr", "maSlope", 
               "rollmean05", "rollmean20", "ema",
               "pctB", "macd", "signal", "RSI")
# Best 5 important features
features3 <- c("open", 
               "atr", "maSlope", 
               "signal", "RSI")
features <- features2


# Transforming data getting the increments between each period time
# klines3[, (features) := lapply(.SD, function(x){ (x - shift(x)) / shift(x)} ),
#         .SDcols = features]
# klines3 <- klines3 %>% na.omit()


# Plotting feature relations
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
par("mar")
par(mar=c(1,1,1,1))
dimPlot <- ceiling(length(features)**(0.5))
par(mfrow=c(dimPlot, dimPlot))
for(feature in features){
  plot(ecdf(x_train[, ..feature][[1]]), main = feature)
  lines(ecdf(x_test[, ..feature][[1]]), col = 'blue')
}
par(mfrow=c(1,1))


# Model
# xgboost::xgb.cv()

xgb.max_specificity <- function(pred, dtrain) {
  # https://github.com/Laurae2/Laurae/blob/master/R/xgb.max_specificity.R
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, spec := tn_v / (tn_v + fp_v)]
  
  best_row <- which.max(DT$spec)
  
  if (length(best_row) > 0) {
    return(list(metric = "spec", value = DT$spec[best_row[1]]))
  } else {
    return(list(metric = "spec", value = -1))
  }
}


#### Cross Validation ####

param <- list(objective = "binary:logistic",
              subsample = 0.8, #0.5,
              colsample_bytree = 0.5,
              max_depth = 5, #3, 
              eta = 0.3, #0.001, # c(0.001, 0.01, 0.3), # learning rate (gradient, [0,1]) https://machinelearningmastery.com/tune-learning-rate-for-gradient-boosting-with-xgboost-in-python/
              min_child = 1,
              scale_pos_weight = 1,#c(0.5, 1, 1.5), # minimize false positive rate https://stackoverflow.com/questions/66716611/how-to-reduce-false-positives-in-xgboost
              lambda = 10) # c(5, 10, 20))
watchlist <- list(train = xgboost_train, test = xgboost_test)
model_cv <- xgb.cv(data = xgboost_train,
                   params = param,
                   nrounds = 25000,
                   nfold = 5,
                   # early_stopping_rounds = 10,
                   # feval = xgb.max_specificity,
                   watchlist = watchlist,
                   print_every_n = 1000
                   # tree_method = "gpu_hist"
                   )
gc()

curves_avg <- rbind(model_cv$evaluation_log[, .(iter, type = "train_error_mean", values = train_error_mean)],
                    model_cv$evaluation_log[, .(iter, type = "test_error_mean", values = test_error_mean)])
curves_stdu <- rbind(model_cv$evaluation_log[, .(iter, type = "train_error_std_up", values = train_error_mean + train_error_std)],
                     model_cv$evaluation_log[, .(iter, type = "test_error_std_up", values = test_error_mean + test_error_std)])
curves_stdd <- rbind(model_cv$evaluation_log[, .(iter, type = "train_error_std_down", values = train_error_mean - train_error_std)],
                     model_cv$evaluation_log[, .(iter, type = "test_error_std_down", values = test_error_mean - test_error_std)])

curves <- rbind(curves_avg, 
                rbind(curves_stdu, 
                      curves_stdd))

p <- ggplot(curves, aes(x = iter, y = values, color = type)) +
  # geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  # ggtitle("Features: All")
  ggtitle("Features: Using feature importance")
p %>% ggplotly()


#### Grid Search with Cross Validation ####

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.8), 
                                colsample_bytree = c(0.5, 0.8),
                                max_depth = c(3, 5),
                                min_child = seq(1), 
                                eta = c(0.001, 0.01, 0.3),
                                scale_pos_weight = c(0.2, 1, 2),
                                lambda = c(5, 10, 20)
                                )
searchGridSubCol <- cbind(iter = 1:nrow(searchGridSubCol), searchGridSubCol)
system.time(
  xgboostHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    print(paste0("Iter: ", parameterList[["iter"]]))
    #Extract Parameters to test
    param <- list(objective = "binary:logistic",
                  subsample = parameterList[["subsample"]],
                  colsample_bytree = parameterList[["colsample_bytree"]],
                  max_depth = parameterList[["max_depth"]], 
                  min_child = parameterList[["min_child"]],
                  eta = parameterList[["eta"]], 
                  scale_pos_weight = parameterList[["scale_pos_weight"]],
                  lambda = parameterList[["lambda"]])
    xgboostModelCV <- xgb.cv(data =  xgboost_train, 
                             params = param,
                             nrounds = 25000, 
                             nfold = 5, 
                             watchlist = watchlist,
                             print_every_n = 1000
                             # early_stopping_rounds = 10
                             )
    gc(); gc()
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    accuracy_test <- tail(xvalidationScores$test_error_mean, 1)
    accuracy_train <- tail(xvalidationScores$train_error_mean, 1)
    output <- return(c(accuracy_test = accuracy_test, 
                       accuracy_train = accuracy_train, 
                       SubsampleRate = parameterList[["subsample"]], 
                       ColsampleRate = parameterList[["colsample_bytree"]], 
                       Depth = parameterList[["max_depth"]], 
                       Eta = parameterList[["eta"]], 
                       MinChild = parameterList[["min_child"]],
                       ScalePosWeight = parameterList[["scale_pos_weight"]],
                       Lambda = parameterList[["lambda"]]))
    }
    )
  )

output <- as.data.table(t(xgboostHyperparameters))
output

fwrite(output, "~/Drive/Codigos/AlgoTrading/DataOut/MLRules/GridSearch/1GridSearch.csv")


#### Best Model ####
model <- xgb.train(data = xgboost_train,
                   params = param,
                   nrounds = 2500,
                   early_stopping_rounds = 100,
                   # feval = xgb.max_specificity,
                   watchlist = watchlist,
                   print_every_n = 100
                   # tree_method = "gpu_hist"
                   )
gc()

curves_bestModel <- rbind(model$evaluation_log[, .(iter, type = "train_error", values = train_error)],
                          model$evaluation_log[, .(iter, type = "test_error", values = test_error)])

p <- ggplot(curves_bestModel, aes(x = iter, y = values, color = type)) +
  # geom_point() +
  geom_line() +
  geom_point(aes(x = model$best_iteration, y = model$evaluation_log$test_error[model$best_iteration]), color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  # ggtitle("Features: All")
  ggtitle("Features: Using feature importance")
p %>% ggplotly()

xgboost::xgb.importance(model = model)
xgboost::xgb.plot.importance(xgb.importance(model = model))


# Predictions
pred_test <- predict(model, xgboost_test)
pred_test

# Confussion Matrix Analysis
i <- 0.6107571
metrics <- NULL
for(i in seq(0, 1, 0.01)){
  cm_threshold <- i
  cm <- confusionMatrix(data = ifelse(pred_test > cm_threshold, 1, 0) %>% factor(levels = c(0, 1)),
                        reference = y_test %>% factor(levels = c(0, 1)),
                        positive = "1")
  metrics <- rbind(metrics,
                   data.table(threshold = i,
                              accuracy = cm$overall[1],
                              sensitivity = cm$byClass[1],
                              specificity = cm$byClass[2],
                              F1 = cm$byClass[7]))
}
ggplot(metrics, aes(x = accuracy, y = specificity)) +
  geom_line()
ggplot(metrics, aes(x = sensitivity, y = specificity)) +
  geom_line()
ggplot(metrics, aes(x = threshold)) +
  geom_line(aes(y = specificity), color = "red") +
  geom_line(aes(y = sensitivity), color = "blue") +
  geom_line(aes(y = accuracy), color = "black") +
  geom_point(aes(x = threshold[which.max(accuracy)], y = max(accuracy))) +
  geom_line(aes(y = F1), color = "green")

pROC_obj <- roc(predictor = pred_test, response = y_test,
                smoothed = TRUE,
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")
best_threshold <- coords(pROC_obj, x="best", input="threshold", best.method="youden")
best_threshold

cm_threshold <- best_threshold[1] %>% as.numeric()
confusionMatrix(data = ifelse(pred_test > cm_threshold, 1, 0) %>% factor(levels = c(0, 1)),
                reference = y_test %>% factor(levels = c(0, 1)),
                positive = "1")

# Testing plots
klines3[sample, train := TRUE]
klines3[, train := ifelse(is.na(train), FALSE, train)]
klines3[!sample, pred_tests := ifelse(pred_test > cm_threshold, pred_test, NA)]

symbol_plot <- pair#"BTCBUSD"
p <- ggplot(klines3[symbol == symbol_plot],
       # aes(x = open_time, y = open, text = paste("pred_tests:", pred_tests))) +
       aes(x = open_time, y = open)) +
  geom_line() +
  geom_point(data = klines3[symbol == symbol_plot & target == 1 & train == FALSE], 
             shape = 15, size = 3) + # test target
  geom_point(data = klines3[symbol == symbol_plot & !is.na(pred_tests)],
             shape = 23, color = "red") + # predicted target
  ggtitle(symbol_plot)
p %>% ggplotly()  


# Maximizar specificity (1 - False Positive Rate)
# Minimizar rombos rojos


# To do:

# Probar más iteraciones en Colab

# Cross Validation

# Grid Search

# Backtesting

# Re-entrenamiento
# - Si datos actuales ya no se ajustan al cumulative distribution function (cdf) 
# - Si bajan el specificity con respecto al calculado en entrenamiento.

# Ejecución del modelo (multicore (uno por cada modelo))
# - Obtención de datos 
# - Procesamiento de datos
# - Predicción de datos
# - Ejecución de compras/ventas