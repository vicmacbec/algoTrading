#### Script information ####
#
# Trade strategy #
#
# https://www.youtube.com/watch?v=RATy3xedqPs
#
# Time:


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


#### Load data ####
print("#### Load data ####")
binance_coins()
binance_coins_prices() #%>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs


#### Initial parameters ####
print("#### Initial parameters ####")

rad2degree <- 180/3.14159265359

BUSDpairs %>% data.table() #%>% View
allData <- NULL
allOrders <- NULL
summaryAll_all <- NULL

# initialInvestment <- 1000
# initialRate <- 0.01
# avgRate <- 0.01

# Last day:     Sys.Date() 
date <- Sys.Date() #as.Date("2022-01-10") 
limit <- 1000
days <- 365
fees <- 0.00075

for(pair in BUSDpairs){
  # pair <- "BTCBUSD""AXSBUSD""BNBBUSD""BTCBUSD" # "AXSBUSD" # "SANDBUSD"# "SOLBUSD"# "ADABNB"#"XRPBUSD"
  print(pair)
  
  # klines <- binance_klines(pair, limit = limit, interval = '5m') # 'BTCUSDT'
  # klines <- binance_klines(pair, limit = limit, interval = '5m', start_time = '2021-12-03')
  klines <- NULL
  for(day in days:0){
    tmp <- binance_klines(pair, limit = 24/4, interval = '4h', start_time = date - day)
    klines <- rbind(klines, tmp)
  }
  klines <- klines[order(open_time)] %>% unique()
  klines[, ':='(open_time = open_time + 1*60*60, close_time = close_time + 1*60*60)] # Add 7 hours to match with TrendingView
  klines
  
  
  print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
  print("")
  
  #### Strategy ####
  print("#### Strategy ####")

  #### Moving Average Slope ####
  klines[, ohlc4 := (open + high + low + close)/4]
  
  # EMA of ohlc4
  klines[, ema := EMA(ohlc4, 55)]
  klines[, ema_lag :=  c(NA, ema[-.N])] # ema ohlc4 lag
  
  # ATR
  klines[, c("tr", "atr", "trueHigh", "trueLow") := ATR(klines[, .(high, low, close)], 14) %>% data.table()]
  
  # MA Slope
  klines[, maSlope := rad2degree*atan((ema - ema_lag)/atr)]
  klines2 <- klines[, .(symbol, open_time, open, high, low, close, volume, close_time, trades, 
                        ohlc4, ema, atr, maSlope)]
  
  
  #### ATR Stop Loss ####
  
  multiplier <- 1.5
  
  klines2[, shortStopLoss := close + atr * multiplier]
  klines2[, longStopLoss := close - atr * multiplier]
  klines2
  
  
  #### Alert flags ####
  print("#### Alert flags ####")
  
  
  #### Testing strategy ####
  print("#### Testing strategy ####")
  
  # Tomando en cuenta solo MASlope con Stop Loss estático (no dinámico)
  # Solo para Long
  orders <- NULL 
  maSlopeCross <- FALSE
  validCandle <- klines2[, sum(is.na(maSlope))] + 1 # Count the candles until the first no NA value 
  for(candle in validCandle:nrow(klines2)){
    # for(candle in 898:944){
    # candle <- 898 # 898
    # candle <- candle + 1
    # print(candle)
    
    # klines2[candle, maSlope] >= 3 & !maSlopeCross
    if(klines2[candle, maSlope] >= 3 & !maSlopeCross){
      maSlopeCross <- TRUE
      
      klines2[candle, 
              .(symbol,
                order_openTime = close_time, 
                order_closeTime = close_time,
                active = 1, 
                price_0 = close,
                price_f = close,
                stopLoss = longStopLoss,
                rate = 0.0,
                riskRewardRatio = 0.0)] -> tmp
      
      orders <- rbind(orders, tmp)
    }else{
      # klines2[candle, maSlope] >= 3
      if(klines2[candle, maSlope] >= 3){
        # Mismo cruce: Revisar si algún orden abierta tocó stop Loss o subir stop Loss
        # orders[active == 1 & klines2[candle, low] < stopLoss]
        orders[active == 1 & klines2[candle, low] < stopLoss, 
               ':='(order_closeTime = klines2[candle, close_time],
                    active = 0,
                    price_f = stopLoss,
                    rate = (stopLoss - price_0)/price_0,
                    riskRewardRatio = 0)]
      }else{
        # Tendencia bajista o rango: Ver si se cierra orden o si se deja abierta hasta nuevo cruce
        # maSlopeCross
        if(maSlopeCross){ # Cambio a negativo
          # orders[active == 1 & klines2[candle, close] > price_0]
          # Vende con ganancias
          orders[active == 1 & klines2[candle, close] > price_0,
                 ':='(order_closeTime = klines2[candle, close_time],
                      active = 0,
                      price_f = klines2[candle, close],
                      rate = (klines2[candle, close] - price_0)/price_0,
                      riskRewardRatio = ((klines2[candle, close] - price_0)/price_0) / ((-stopLoss + price_0)/price_0))]
          
          # Revisar si algún orden abierta tocó stop Loss o subir stop Loss
          orders[active == 1 & klines2[candle, low] < stopLoss]
          orders[active == 1 & klines2[candle, low] < stopLoss, 
                 ':='(order_closeTime = klines2[candle, close_time],
                      active = 0,
                      price_f = stopLoss,
                      rate = (stopLoss - price_0)/price_0,
                      riskRewardRatio = 0)]
          
          maSlopeCross <- FALSE
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
  
  ## Summary data: All data ##
  allData <- rbind(allData, klines2)
  #
  
  allOrders <- rbind(allOrders, orders)
}
allOrders[, realRate := rate*(1 - fee) - 2*fee]
allOrders[, yield := 1 + realRate]
allOrders[, cumprod(yield)]


allData[as.Date(open_time) <= date] %>% View
allOrders[as.Date(order_closeTime) <= date] %>% View


#### Plots ####
print("#### Plots ####")


#### Saving data ####
print("#### Saving data ####")


#### Tests ####

