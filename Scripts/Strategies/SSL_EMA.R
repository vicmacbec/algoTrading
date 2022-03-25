#### Script information ####
#
# Trade strategy #
#
# https://www.youtube.com/watch?v=rOnv5yFuqT0
#
# Time: 
21:22:00
#### Load libraries ####
print("#### Load libraries ####")
library(binancer)
library(keyring)
library(ggplot2)
library(scales)
library(data.table)
library(TTR)
library(stringr)
library(zoo)
library(plotly)
library(purrr)
library(roll)
library(patchwork)


#### Functions ####
print("#### Functions ####")

# # n <- 4
# # len <- 7
an <- function(n, len){
  if(len-n > 0){
    c(seq.int(n), rep(n, len-n))
  }else{
    seq.int(len)
  }
}


#### Load data ####
print("#### Load data ####")
binance_coins()
binance_coins_prices() #%>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs


#### Initial parameters ####
print("#### Initial parameters ####")

BUSDpairs %>% data.table() #%>% View
allData <- NULL
allOrders <- NULL

# initialInvestment <- 1000
# initialRate <- 0.01
# avgRate <- 0.01

limit <- 1000


for(pair in BUSDpairs){
  # pair <- "BTCBUSD" # "AXSBUSD" # "SANDBUSD"# "SOLBUSD"# "ADABNB"#"XRPBUSD"
  print(pair)
  
  klines <- binance_klines(pair, limit = limit, interval = '5m') # 'BTCUSDT'
  print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
  print("")
  
  
  ## Summary data: All data ##
  allData <- rbind(allData, klines)
  #
  
  #### Strategy ####
  print("#### Strategy ####")
  
  # Exponential Moving average #
  klines[, ema := EMA(close, n = 200)]
  # klines
  
  # SSL #
  klines[, smaHigh := SMA(high, n = 10)]
  klines[, smaLow := SMA(low, n = 10)]
  klines[, Hlv := ifelse(close > smaHigh, 1, ifelse(close < smaLow, -1, NA))]
  klines[, Hlv2 := as.numeric(na.locf(zoo(as.integer(Hlv)), fromLast = FALSE, na.rm = FALSE))]
  klines[, sslDown := ifelse(Hlv2 < 0, smaHigh, smaLow)]
  klines[, sslUp := ifelse(Hlv2 < 0, smaLow, smaHigh)]
  # klines
  
  klines2 <- klines[, .(symbol, open_time, close_time, open, high, low, close, volume, trades, 
                        ema, smaHigh, smaLow, Hlv, Hlv2, sslDown, sslUp)]
  # klines2
  
  # Falta ver cómo acomodar los NA. 
  #   - Si son muchos NA y -1 consecutivos, salir en la próxima vela con ganancias
  # Si recorrerlos hacia abajo con el último Hlv encontrado o ver cómo manejarlo.
  # Probar con diferentes parámetros
  # Calcular stop loss
  # Probar con y sin EMA.
  
  #### Alert flags ####
  print("#### Alert flags ####")
  
  #### Testing strategy ####
  print("#### Testing strategy ####")
  
  # Tomando en cuenta solo SSL, sin EMA
  orders <- NULL 
  sslCross <- FALSE
  validCandle <- klines2[, sum(is.na(Hlv2))] + 1 # Count the candles until the first no NA value 
  for(candle in validCandle:nrow(klines2)){
  # for(candle in 898:944){
    # candle <- 898 # 898
    # candle <- candle + 1
    # print(candle)
    
    # klines2[candle, Hlv2] == 1 & !sslCross
    if(klines2[candle, Hlv2] == 1 & !sslCross){
      sslCross <- TRUE
      
      klines2[candle, 
              .(symbol,
                order_openTime = close_time, 
                order_closeTime = close_time,
                active = 1, 
                price_0 = close,
                price_f = close,
                stopLoss = sslDown,
                rate = 0.0,
                riskRewardRatio = 0.0)] -> tmp
      
      orders <- rbind(orders, tmp)
    }else{
      # klines2[candle, Hlv2] == 1
      if(klines2[candle, Hlv2] == 1){
        # Mismo cruce: Revisar si algún orden abierta tocó stop Loss o subir stop Loss
        # orders[active == 1 & klines2[candle, low] < stopLoss]
        orders[active == 1 & klines2[candle, low] < stopLoss, 
               ':='(order_closeTime = klines2[candle, close_time],
                    active = 0,
                    price_f = stopLoss,
                    rate = (stopLoss - price_0)/price_0,
                    riskRewardRatio = 0)]
      }else{
        # Tendencia bajista: Ver si se cierra orden o si se deja abierta hasta nuevo cruce
        # sslCross
        if(sslCross){ # Cambio a negativo
          # orders[active == 1 & klines2[candle, close] > price_0]
          # Vende con ganancias
          orders[active == 1 & klines2[candle, close] > price_0,
                 ':='(order_closeTime = klines2[candle, close_time],
                      active = 0,
                      price_f = klines2[candle, close],
                      rate = (klines2[candle, close] - price_0)/price_0,
                      riskRewardRatio = ((klines2[candle, close] - price_0)/price_0) / ((-stopLoss + price_0)/price_0))]
          
          sslCross <- FALSE
        }else{ # Si ya se había hecho el cambio a negativo
          if(!is.null(orders)){ # Revisa si orders está recien creado o ya tiene información
            # orders[active == 1 & klines2[candle, low] < stopLoss]
            # Revisa stop loss
            orders[active == 1 & klines2[candle, low] < stopLoss, 
                   ':='(order_closeTime = klines2[candle, close_time],
                        active = 0,
                        price_f = stopLoss,
                        rate = (stopLoss - price_0)/price_0,
                        riskRewardRatio = 0)] 
          }
        }
      }
    }
  }
  allOrders <- rbind(allOrders, orders)
}
allOrders



# number of orders
allOrders[, .(.N), keyby = .(symbol)] %>% View
# Analysis Time
allOrders[, .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), keyby = .(symbol)
          ][order(-time)]
# summary of number of candles
allOrders[, .(symbol, candels = as.numeric(difftime(order_closeTime, order_openTime, units = "min"))/5)
          ][, .(min = min(candels), 
                fQ = quantile(candels, 0.25), 
                median = quantile(candels, 0.5), 
                mean = mean(candels), 
                tQ = quantile(candels, 0.75), 
                max = max(candels)), 
            keyby = .(symbol)] %>% View
# win rate
merge(allOrders[rate >= 0, .(wins = .N), keyby = .(symbol)],
      allOrders[, .(numOrders = .N), keyby = .(symbol)],
      by = c("symbol"),
      all = TRUE
      )[, .(symbol, wins, numOrders, winRate = wins / numOrders)] %>% View
# Yields: summary by symbol
allOrders[, .(min = min(rate), 
              fQ = quantile(rate, 0.25), 
              median = quantile(rate, 0.5), 
              mean = mean(rate), 
              tQ = quantile(rate, 0.75), 
              max = max(rate)), 
          keyby = .(symbol)] %>% View
# Yields: cumprod
allOrders[, .(yield = cumprod(1 + rate)), keyby = .(symbol)]
allOrders[, .(yield = cumprod(1 + rate)), keyby = .(symbol)][, .SD[.N], keyby = .(symbol)] %>% View
# Yield per day
merge(allOrders[, .(yield = cumprod(1 + rate)), keyby = .(symbol)][, .SD[.N], keyby = .(symbol)],
      allOrders[, .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), keyby = .(symbol)],
      by = c("symbol"),
      all = TRUE
      )[, .(symbol, yield, time, yieldPerDay = (yield - 1)/as.numeric(time))] %>% View


#### Plots ####
print("#### Plots ####")


#### Saving data ####
print("#### Saving data ####")


#### Tests ####

