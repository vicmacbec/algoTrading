#### Script information ####
#
# Trade strategy #
#
# https://www.youtube.com/watch?v=rOnv5yFuqT0
#
# Time: 14:38 min


#### Load libraries ####
print("#### Load libraries ####")
library(binancer)
library(keyring)
library(ggplot2)
library(tidyquant)
library(plotly)
library(scales)
library(data.table)
library(TTR)
library(stringr)
library(zoo)
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
summaryAll_all <- NULL

# initialInvestment <- 1000
# initialRate <- 0.01
# avgRate <- 0.01

# Down:         "2022-01-10" 
# Up:           "2021-10-08" 
# Lateral Up:   "2022-02-03" 
# Lateral Down: "2021-07-20"
# Last day:     Sys.Date() 
date <- as.Date("2022-01-10") 
limit <- 1000
days <- 365


for(pair in BUSDpairs){
  # pair <- "BTCBUSD""AXSBUSD""BNBBUSD""BTCBUSD" # "AXSBUSD" # "SANDBUSD"# "SOLBUSD"# "ADABNB"#"XRPBUSD"
  print(pair)
  
  # klines <- binance_klines(pair, limit = limit, interval = '5m') # 'BTCUSDT'
  # klines <- binance_klines(pair, limit = limit, interval = '5m', start_time = '2021-12-03')
  klines <- NULL
  for(day in days:0){
    tmp <- binance_klines(pair, limit = 24*60/5, interval = '5m', start_time = date - day)
    klines <- rbind(klines, tmp)
  }
  klines <- klines[order(open_time)] %>% unique()
  klines[, ':='(open_time = open_time + 6*60*60, close_time = close_time + 6*60*60)] # Add 6 hours to match with TrendingView
  klines
  
  
  print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
  print("")
  
  
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
  
  
  #### Alert flags ####
  print("#### Alert flags ####")
  
  #### Testing strategy ####
  print("#### Testing strategy ####")
  
  # Tomando en cuenta SSL con EMA
  orders <- NULL 
  sslCross <- FALSE
  validCandle <- klines2[, sum(is.na(ema))] + 1 # Count the candles until the first no NA value 
  for(candle in validCandle:nrow(klines2)){
  # for(candle in 200:560){
    # candle <- validCandle 12 # 898
    # candle <- candle + 1
    # print(candle)
    
    # Open an order
    # klines2[candle, Hlv2] == 1 & !sslCross & klines2[candle, ema < close]
    # klines2[candle]
    if(klines2[candle, Hlv2] == 1 & !sslCross & klines2[candle, ema < close]){
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
      # klines2[candle, Hlv2] == 1 & !is.null(orders)
      if(klines2[candle, Hlv2] == 1 & !is.null(orders)){
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
          
          # Revisar si algún orden abierta tocó stop Loss o subir stop Loss
          # orders[active == 1 & klines2[candle, low] < stopLoss]
          orders[active == 1 & klines2[candle, low] < stopLoss, 
                 ':='(order_closeTime = klines2[candle, close_time],
                      active = 0,
                      price_f = stopLoss,
                      rate = (stopLoss - price_0)/price_0,
                      riskRewardRatio = 0)]
          
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
  
  ## Summary data: All data ##
  allData <- rbind(allData, klines2)
  #
  
  allOrders <- rbind(allOrders, orders)
}
allData[as.Date(open_time) <= date] %>% View

allOrders[as.Date(order_closeTime) <= date] %>% View


# number of orders
allOrders[as.Date(order_closeTime) <= date, .(numOrders = .N), keyby = .(symbol)] -> numOrders
numOrders %>% View
# Analysis Time
allOrders[as.Date(order_closeTime) <= date, 
          .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), 
          keyby = .(symbol)
          ][order(-time)]
# summary of number of candles
allOrders[as.Date(order_closeTime) <= date, 
          .(symbol, order_closeTime, order_openTime, candels = as.numeric(difftime(order_closeTime, order_openTime, units = "min"))/5)
          ][, .(minNumCandels = min(candels), 
                fQNumCandels = quantile(candels, 0.25), 
                medianNumCandels = quantile(candels, 0.5), 
                meanNumCandels = mean(candels), 
                tQNumCandels = quantile(candels, 0.75), 
                maxNumCandels = max(candels)), 
            keyby = .(symbol)] -> summaryNumCandels
summaryNumCandels %>% View
# win rate
merge(allOrders[as.Date(order_closeTime) <= date & rate >= 0, .(wins = .N), keyby = .(symbol)],
      allOrders[as.Date(order_closeTime) <= date, .(numOrders = .N), keyby = .(symbol)],
      by = c("symbol"),
      all = TRUE
      )[, .(symbol, wins, numOrders, winRate = wins / numOrders)] -> winRate
winRate %>% View
# Yields: summary by symbol
allOrders[as.Date(order_closeTime) <= date, 
          .(minYields = min(rate), 
            fQYields = quantile(rate, 0.25), 
            medianYields = quantile(rate, 0.5), 
            meanYields = mean(rate), 
            tQYields = quantile(rate, 0.75), 
            maxYields = max(rate)), 
          keyby = .(symbol)] -> summaryYields
summaryYields %>% View
# Yields: cumprod
allOrders[as.Date(order_closeTime) <= date, .(order_openTime, yield = cumprod(1 + rate)), keyby = .(symbol)] #%>% View
allOrders[as.Date(order_closeTime) <= date, .(order_openTime, yield = cumprod(1 + rate)), keyby = .(symbol)
          ][order(order_openTime)
            ][, .SD[.N], keyby = .(symbol)][, .(symbol, yield)] -> cumprodYields
cumprodYields %>% View
# Yield per day
merge(allOrders[as.Date(order_closeTime) <= date, 
                .(order_openTime, yield = cumprod(1 + rate)), 
                keyby = .(symbol)][order(order_openTime)][, .SD[.N], keyby = .(symbol)],
      allOrders[as.Date(order_closeTime) <= date, 
                .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), 
                keyby = .(symbol)],
      by = c("symbol"),
      all = TRUE
      )[, .(symbol, yield, time, yieldPerDay = (yield - 1)/as.numeric(time))] -> dayYield
dayYield %>% View


# ¿De qué depende que sea un buen symbol?
# winRate, rendimientos, 
merge(winRate,
      merge(summaryYields,
            merge(dayYield,
                  summaryNumCandels,
                  by = "symbol",
                  all = TRUE
                  ),
            by = "symbol",
            all = TRUE
            ),
      by = "symbol",
      all = TRUE
      ) -> summaryAll
summaryAll %>% View

fwrite(summaryAll, "./DataOut/SSL_EMA/summaryAll.csv", append = TRUE)

summaryAll_all <- summaryAll
summaryAll_all <- merge(summaryAll_all, 
                        summaryAll[, .(symbol, wins, numOrders, winRate, 
                                       minYields, fQYields, medianYields, meanYields, tQYields, maxYields,
                                       yield, time, yieldPerDay)],
                        by = c("symbol"),
                        all = TRUE
                        )

# ¿Cómo se correlaciona el winRate, medianYields, meanYields, yield?


#### Plots ####
print("#### Plots ####")

# Cambiar ejes 
p <- ggplot(summaryAll[symbol != "APEBUSD"], # Se comenta outlier
            aes(color = winRate, y = yield, x = tQYields, text = paste0("symbol: ", symbol))) +
  geom_point() +
  scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept = 1, linetype='dotted')
p %>% ggplotly()

p <- ggplot(summaryAll[symbol != "APEBUSD"], # Se comenta outlier
            aes(color = winRate, y = yield, x = fQYields, text = paste0("symbol: ", symbol))) +
  geom_point() +
  scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept = 1, linetype='dotted')
p %>% ggplotly()


# Poner por color winRate y/o yields y/o riskReward (hacer merge primero)
p <- ggplot(allOrders[as.Date(order_closeTime) <= date, .(symbol, rate)], 
            aes(x = symbol, y = rate, color = symbol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p %>% ggplotly()

merge(merge(allOrders[as.Date(order_closeTime) <= date, .(symbol, rate)],
            winRate,
            by = "symbol",
            all = TRUE),
      dayYield,
      by = "symbol",
      all = TRUE) -> allYields

p <- ggplot(allYields[symbol != "APEBUSD", 
                      .(symbol, rate, winRate, yield)], # Se quita outlier
            aes(x = reorder(symbol, -yield), y = rate, color = yield, text = paste0("winRate: ", winRate ))) +
  geom_boxplot() +
  scale_color_gradient(low="blue", high="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p %>% ggplotly()

p <- ggplot(allYields[symbol != "APEBUSD", 
                      .(symbol, rate, winRate, yield)], # Se quita outlier
            aes(x = reorder(symbol, -winRate), y = rate, color = winRate, text = paste0("yield: ", yield ))) +
  geom_boxplot() +
  scale_color_gradient(low="blue", high="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p %>% ggplotly()


# ¿Por qué los boxplot se ven muy bajos para los sḿbolos con buenos rendimientos?
# Revisar riskReward




p <- ggplot(allData[symbol == pair], aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Symbol:', pair, 'Last Updated:', Sys.time())) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c('#1a9850', '#d73027')) + # RdYlGn
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
p %>% ggplotly()


ggplot(klines2, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Symbol:', pair, 'Last Updated:', Sys.time())) +
  scale_color_manual(values = c('#1a9850', '#d73027')) +
  facet_wrap(~symbol, scales = 'free', nrow = 2)



#### Saving data ####
print("#### Saving data ####")


#### Tests ####

# Yield tests #

# Initial data
feeRate <- (1-0.00075) 
initialCapital <- 10
rate <- 0.008033788

# First fee attempt
buy <- feeRate*initialCapital
sell <- buy*(1+rate)*feeRate # feeRate*initialCapital*(1+rate)*feeRate = feeRate²*initialCapital*(1+rate)
initialCapital <- sell

# (1/feeRate**2)-1 is the minimum rate to be profency
rate >= (1/feeRate**2)-1

# Initial data with prices
price_s <- 33921.73 
price_f <- 34194.25   
yield <- 0.008033788

btc <- initialCapital/price_s
finalCapital <- btc*price_f

# Prove of yield doesn't depend of capital
yield2 <- (finalCapital-initialCapital)/initialCapital
# yield2 \eq (price_f-price_s)/price_s \eq yield

# From fees documentation (https://www.binance.com/en/support/faq/e85d6e703b874674840122196b89780a)
(btc <- initialCapital/price_s)
(feeBuy <- 0.00075*btc)
(finalCapital <- (btc-feeBuy)*price_f)
(feeSell <- 0.00075*btc*price_f) # Aquí está la diferencia con "first fee attempt", feeSell va sobre btc, no sobre (btc-feeBuy)

# Real final capital after fees
(finalCapital-feeSell)

# ENJBUSD example (order No. 239712854)
8.8*1.666
8.8*1.666*0.00075 # fee calculation
0.00002640*415.94 # BNB fee


# Real field
realYield <- ((finalCapital-feeSell) - initialCapital)/initialCapital

# Difference between yield without fees and yield with fees
yield2 - realYield

# Despejando realYield. realYield doesn't depend of capital, just price
((btc-feeBuy)*price_f - 0.00075*btc*price_f - initialCapital)/initialCapital
((initialCapital/price_s - 0.00075*btc)*price_f - 0.00075*btc*price_f - initialCapital)/initialCapital
((initialCapital/price_s - 0.00075*initialCapital/price_s)*price_f - 0.00075*(initialCapital/price_s)*price_f - initialCapital)/initialCapital
(price_f/price_s - 0.00075*price_f/price_s - 0.00075*price_f/price_s - 1)
((1 - 0.00075 - 0.00075)*price_f/price_s) - 1
((1 - 2*0.00075)*price_f/price_s) - 1
((1 - 2*0.00075)*price_f/price_s) - price_s/price_s
((1 - 2*0.00075)*price_f - price_s)/price_s # realYield = ((1-2fee)*price_f - price_s)/price_s

# Constanta que multiplica a price_f en realYield, tiene semejanza a fee**2
(1-0.00075)*(1-0.00075) = 1 - 2*0.00075 + 0.00075**2