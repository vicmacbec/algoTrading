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


#### Functions ####
print("#### Functions ####")


#### Paths ####
# pathDataInAllOrders <- "~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/Orders/"
# pathDataInAllData <- "~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/Trades/"
# pathDataInYml <- "~/Drive/Codigos/AlgoTrading/"
pathDataInAllOrders <- "~/algoTrading/DataOut/MASlope_ATRStopLoss/Orders/"
pathDataInAllData <- "~/algoTrading/DataOut/MASlope_ATRStopLoss/Trades/"
pathDataInYml <- "~/algoTrading/"
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
tmp <- binance_klines(pair, limit = 10, interval = '4h'
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

#### Strategy ####
print("#### Strategy ####")

#### Moving Average Slope ####
klines[, ohlc4 := rowSums(.SD)/4, .SDcols = c("open", "high", "low", "close")] %>% suppressWarnings()

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
klines2[, shortStopLoss := close + atr * multiplier]
klines2[, longStopLoss := close - atr * multiplier]
# klines2


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
    
    # # Si la última vela es la que abre operación: Compra y stop loss
    # if(candle == nrow(klines2)){
    #   binance_new_order(symbol = pair, side = "BUY", type = "LIMIT",
    #                     time_in_force = "GTC", # Good til cancelled
    #                     quantity = 10,
    #                     price = tmp[, price_0],
    #                     test = TRUE
    #                     )
    #   print(paste0("BUY: ", 10, " ", pair, " at ", tmp[, price_0], " usd."))
    # 
    #   # Crea Stop Loss
    #   binance_new_order(symbol = pair, side = "SELL", type = "LIMIT",
    #                     time_in_force = "GTC", # Good til cancelled
    #                     quantity = 10, # Lo que se compró arriba en la orden de compra
    #                     price = tmp[, stopLoss],
    #                     test = TRUE
    #                     )
    #   print(paste0("SELL: ", 10, " ", pair, " at ", orders[.N, stopLoss], " usd. Stop Loss."))
    # }
    
    orders <- rbind(orders, tmp)
  }else{
    # klines2[candle, maSlope] >= 3
    if(klines2[candle, maSlope] >= 3){
      # Mismo cruce: Revisar si algún orden abierta tocó stop Loss o subir stop Loss
      # orders[active == 1 & klines2[candle, low] < stopLoss]
      
      closingOrders <- orders[active == 1 & klines2[candle, low] < stopLoss] %>% nrow()
      
      orders[active == 1 & klines2[candle, low] < stopLoss, 
             ':='(order_closeTime = klines2[candle, close_time],
                  active = 0,
                  price_f = stopLoss,
                  rate = (stopLoss - price_0)/price_0,
                  riskRewardRatio = 0)]
      
      print("Stop Loss 1.")

    }else{
      # Tendencia bajista o rango: Ver si se cierra orden o si se deja abierta hasta nuevo cruce
      # maSlopeCross
      if(maSlopeCross){ # Cambio a negativo
        # orders[active == 1 & klines2[candle, close] > price_0]
        
        closingOrders <- orders[active == 1 & klines2[candle, close] > price_0] %>% nrow()
        
        # Vende con ganancias
        orders[active == 1 & klines2[candle, close] > price_0,
               ':='(order_closeTime = klines2[candle, close_time],
                    active = 0,
                    price_f = klines2[candle, close],
                    rate = (klines2[candle, close] - price_0)/price_0,
                    riskRewardRatio = ((klines2[candle, close] - price_0)/price_0) / ((-stopLoss + price_0)/price_0))]
        
        # if(closingOrders > 0){
        #   # Cierra por ganancias
        #   binance_new_order(symbol = pair, side = "SELL", type = "LIMIT",
        #                     time_in_force = "GTC", # Good til cancelled
        #                     quantity = 10,
        #                     price = orders[.N, price_f],
        #                     test = TRUE
        #                     )
        #   print(paste0("SELL: ", 10, " ", pair, " at ", orders[.N, price_f], " usd."))
        # }
        
        # Revisar si algún orden abierta tocó stop Loss o subir stop Loss
        # orders[active == 1 & klines2[candle, low] < stopLoss]
        
        closingOrders <- orders[active == 1 & klines2[candle, low] < stopLoss] %>% nrow()
        
        orders[active == 1 & klines2[candle, low] < stopLoss, 
               ':='(order_closeTime = klines2[candle, close_time],
                    active = 0,
                    price_f = stopLoss,
                    rate = (stopLoss - price_0)/price_0,
                    riskRewardRatio = 0)]
        
        print(paste0("Stop Loss 2."))

        maSlopeCross <- FALSE
      }else{ # Si ya se había hecho el cambio a negativo
        if(!is.null(orders)){ # Revisa si orders está recien creado o ya tiene información
          # orders[active == 1 & klines2[candle, low] < stopLoss]
          
          closingOrders <- orders[active == 1 & klines2[candle, low] < stopLoss] %>% nrow()
          
          # Revisa stop loss
          orders[active == 1 & klines2[candle, low] < stopLoss, 
                 ':='(order_closeTime = klines2[candle, close_time],
                      active = 0,
                      price_f = stopLoss,
                      rate = (stopLoss - price_0)/price_0,
                      riskRewardRatio = 0)] 
          
          print(paste0("Stop Loss 3."))
          
        }
      }
    }
  }
}

allOrders <- rbind(allOrders[symbol != pair][, c("realRate", "yield", "cumYield") := NULL], orders)

allOrders[, realRate := rate*(1 - fee) - 2*fee]
allOrders[, yield := 1 + realRate]
allOrders[, cumYield := cumprod(yield), keyby = .(symbol)]
# allOrders[symbol == pair, cumYield]
# allOrders[symbol == pair]

## Summary data: All data ##
klines2[, direction := ifelse(close >= open, 'Increasing', 'Decreasing')]
klines2[, directionMaSlope := ifelse(maSlope > 3, 'Increasing', ifelse(maSlope < -3, 'Decreasing', 'Range'))]
allData <- rbind(allData[symbol != pair], klines2)
# allData[symbol == pair] %>% View


#### Saving data ####
print("#### Saving data ####")

fwrite(allOrders, paste0(pathDataInAllOrders, "allOrders_year_20220421.csv"))
fwrite(allData, paste0(pathDataInAllData, "allData_year_4h_20220421.csv"))


#### End Script ####
print("#### End Script ####")


# # ¿Cómo escoger a los symbols que se ajustan mejor a la estrategia?
# 
# # number of orders
# allOrders[as.Date(order_closeTime) <= date, .(numOrders = .N), keyby = .(symbol)] -> numOrders
# numOrders %>% View
# # Analysis Time
# allOrders[as.Date(order_closeTime) <= date, 
#           .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), 
#           keyby = .(symbol)
#           ][order(-time)]
# # summary of number of candles
# allOrders[as.Date(order_closeTime) <= date, 
#           .(symbol, order_closeTime, order_openTime, candels = as.numeric(difftime(order_closeTime, order_openTime, units = "hours"))/4)
#           ][, .(minNumCandels = min(candels), 
#                 fQNumCandels = quantile(candels, 0.25), 
#                 medianNumCandels = quantile(candels, 0.5), 
#                 meanNumCandels = mean(candels), 
#                 tQNumCandels = quantile(candels, 0.75), 
#                 maxNumCandels = max(candels)), 
#             keyby = .(symbol)] -> summaryNumCandels
# summaryNumCandels %>% View
# # Best number of candles for greater rate
# allOrders[as.Date(order_closeTime) <= date, 
#           .(symbol, order_closeTime, order_openTime, 
#             candels = as.numeric(difftime(order_closeTime, order_openTime, units = "hours"))/4,
#             yield)
#           ][order(symbol, candels, yield)] -> rateCandles
# p <- ggplot(rateCandles, aes(x = candels, y = yield, color = symbol)) +
#   geom_point() +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm')
# p %>% ggplotly()
# 
# # p <- ggplot(rateCandles, aes(x = candels, y = yield, text = paste0("symbol: ", symbol))) +
# #   geom_point() +
# #   stat_summary(fun.data= mean_cl_normal) + 
# #   geom_smooth(method='lm')
# # p %>% ggplotly()
# 
# p <- ggplot(rateCandles, aes(x = candels, y = yield)) +
#   geom_point() +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm')
# p %>% ggplotly()
# 
# model <- lm(yield ~ poly(candels, 1), data = rateCandles)
# summary(model)
# 
# # Cada 2.12 (0.01/0.004707) velas que dura la estrategia aumenta un 0.01 % el rendimiento, 
# # empezando desde 0.9372 %. 
# # Entonces, en promedio, después de la vela 14 ((1-0.9372)*2.12/0.01), ya se es rentable
# 
# 
# # USDCBUSD, IDEXBUSD, ALPACABUSD
# 
# # win rate
# merge(allOrders[as.Date(order_closeTime) <= date & realRate >= 0, .(wins = .N), keyby = .(symbol)],
#       allOrders[as.Date(order_closeTime) <= date, .(numOrders = .N), keyby = .(symbol)],
#       by = c("symbol"),
#       all = TRUE
# )[, .(symbol, wins, numOrders, winRate = wins / numOrders)] -> winRate
# winRate[order(-winRate)] %>% View
# # Yields: summary by symbol
# allOrders[as.Date(order_closeTime) <= date, 
#           .(minYields = min(realRate), 
#             fQYields = quantile(realRate, 0.25), 
#             medianYields = quantile(realRate, 0.5), 
#             meanYields = mean(realRate), 
#             tQYields = quantile(realRate, 0.75), 
#             maxYields = max(realRate)), 
#           keyby = .(symbol)] -> summaryYields
# summaryYields %>% View
# # Yields: cumprod
# allOrders[as.Date(order_closeTime) <= date, .(order_openTime, yield = cumprod(1 + realRate)), keyby = .(symbol)] #%>% View
# allOrders[as.Date(order_closeTime) <= date, .(order_openTime, yield = cumprod(1 + realRate)), keyby = .(symbol)
#           ][order(order_openTime)
#             ][, .SD[.N], keyby = .(symbol)][, .(symbol, yield)] -> cumprodYields
# cumprodYields[order(-yield)] %>% View
# cumprodYields[, mean(yield, na.rm = TRUE)] # Avg yield for all symbols
# 
# p <- ggplot(rateCandles[symbol %in% cumprodYields[yield > 1.11, symbol]], 
#             aes(x = candels, y = yield, color = symbol)) +
#   geom_point() +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm')
# p %>% ggplotly()
# 
# p <- ggplot(rateCandles[symbol %in% cumprodYields[yield > 1.11, symbol]], 
#             aes(x = candels, y = yield)) +
#   geom_point() +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm')
# p %>% ggplotly()
# 
# model <- lm(yield ~ poly(candels, 1), 
#             data = rateCandles[symbol %in% cumprodYields[yield > 1.11, symbol]])
# summary(model)
# 
# 
# # Yield per day
# merge(allOrders[as.Date(order_closeTime) <= date, 
#                 .(order_openTime, yield = cumprod(1 + realRate)), 
#                 keyby = .(symbol)][order(order_openTime)][, .SD[.N], keyby = .(symbol)],
#       allOrders[as.Date(order_closeTime) <= date, 
#                 .(time = difftime(max(order_closeTime), min(order_openTime), units = "days")), 
#                 keyby = .(symbol)],
#       by = c("symbol"),
#       all = TRUE
# )[, .(symbol, yield, time, yieldPerDay = (yield - 1)/as.numeric(time))] -> dayYield
# dayYield %>% View
# 
# 
# # ¿De qué depende que sea un buen symbol?
# # Parece que entre más larga e inclinada sea la recta, mejor yield final hay.
# # Entonces el tiempo que dura la tendencia está correlacionado fuertemente positivo con el rendimiento.
# # ¿Quién es el más probable en tener una tendencia alcista larga?
# # ¿Quién es el más probable en ser el próximo outlier?
# 
# 
# daysList <- NULL
# for(order in 1:nrow(allOrders)){
#   tmp <- allOrders[order, .(symbol, 
#                             dates = seq(as.Date(order_openTime), as.Date(order_closeTime), by = "days"), 
#                             yield)]
#   daysList <- rbind(daysList, tmp)
# }
# daysList
# 
# tmp <- dcast(daysList, symbol ~ dates, fun.aggregate = mean, value.var = "yield")
# daysList <- melt(tmp, measure.vars = patterns("^202"), variable.name = "dates", value.name = "yield")
# 
# daysList[, symbol := factor(symbol, levels = cumprodYields[order(-yield)][, symbol])]
# daysList %>% str
# 
# ceilYield <- 2
# p <- ggplot(daysList[, .(symbol, dates, yield = ifelse(yield > ceilYield, ceilYield, yield))], 
#             aes(dates, symbol, fill= yield)) + 
#   geom_tile() + 
#   scale_fill_gradient(low="cornflowerblue", high="black") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# p %>% ggplotly()
# 
# 
# # winRate, rendimientos, 
# merge(winRate,
#       merge(summaryYields,
#             merge(dayYield,
#                   summaryNumCandels,
#                   by = "symbol",
#                   all = TRUE
#             ),
#             by = "symbol",
#             all = TRUE
#       ),
#       by = "symbol",
#       all = TRUE
# ) -> summaryAll
# summaryAll[order(-yield)] %>% View
# 
# summaryAll[, paysOff := ifelse(yield >= 1.11, 1, 0)]
# summaryAll[, .(avgFinalYield = mean(yield),
#                avgYield = mean(meanYields),
#                avgCandles = mean(meanNumCandels),
#                avgFinalYieldPaysOff = mean(ifelse(paysOff == 1, yield, NA), na.rm = TRUE),
#                avgYieldPaysOff = mean(ifelse(paysOff == 1, meanYields, NA), na.rm = TRUE),
#                avgCandlesPaysOff = mean(ifelse(paysOff == 1, meanNumCandels, NA), na.rm = TRUE),
#                avgFinalYieldNoPaysOff = mean(ifelse(paysOff == 0, yield, NA), na.rm = TRUE),
#                avgYieldNoPaysOff = mean(ifelse(paysOff == 0, meanYields, NA), na.rm = TRUE),
#                avgCandlesNoPaysOff = mean(ifelse(paysOff == 0, meanNumCandels, NA), na.rm = TRUE)
# )]
# 
# summaryAll_all <- summaryAll
# summaryAll_all <- merge(summaryAll_all, 
#                         summaryAll[, .(symbol, wins, numOrders, winRate, 
#                                        minYields, fQYields, medianYields, meanYields, tQYields, maxYields,
#                                        yield, time, yieldPerDay)],
#                         by = c("symbol"),
#                         all = TRUE
# )
# 
# # ¿Cómo se correlaciona el winRate, medianYields, meanYields, yield?
# 
# 
# 
# 
# #### Plots ####
# print("#### Plots ####")
# 
# 
# #### Plotly ####
# 
# # Candle color #
# allData[, direction := ifelse(close >= open, 'Increasing', 'Decreasing')]
# allData[, directionMaSlope := ifelse(maSlope > 3, 'Increasing', ifelse(maSlope < -3, 'Decreasing', 'Range'))]
# allData
# 
# 
# # Candles #
# 
# # i <- list(line = list(color = '#17BECF'))
# # d <- list(line = list(color = '#7F7F7F'))
# i <- list(line = list(color = 'green'))
# d <- list(line = list(color = 'red'))
# 
# # plot candlestick chart
# # rm(fig)
# fig <- allData[symbol == "FXSBUSD"] %>% 
#   plot_ly(x = ~open_time, type="candlestick",
#           open = ~open, close = ~close,
#           high = ~high, low = ~low, name = ~symbol,
#           increasing = i, decreasing = d) 
# fig <- fig %>% add_trace(x = ~open_time, y = ~longStopLoss, name = 'Short Stop Loss', 
#                          type = 'scatter', mode = 'markers',
#                          legendgroup = "SSL Channel",
#                          hoverinfo = "none", inherit = F)
# 
# # # https://plotly.com/r/horizontal-vertical-shapes/#horizontal-and-vertical-lines-and-rectangles-in-r
# # vline <- function(x = 0, color = "black") {
# #   list(
# #     type = "line",
# #     x0 = x, x1 = x,
# #     yref = "paper",
# #     y0 = 0, y1 = 1,
# #     line = list(color = color)
# #   )
# # }
# # 
# # fig <- fig %>% layout(yaxis = list(title = "Price"),
# #                       shapes = list(vline(allOrders[symbol == "FXSBUSD", order_openTime], 
# #                                           ifelse(allOrders[symbol == "FXSBUSD", yield] > 1,
# #                                                  "green", "red"))))
# fig <- fig %>% layout(yaxis = list(title = "Price"))
# 
# # plot volume bar chart
# fig2 <- allData[symbol == "FXSBUSD"]
# fig2 <- fig2 %>% plot_ly(x=~open_time, y=~maSlope, type='scatter', name = paste0("FXSBUSD", " MA Slope"),
#                          color = ~directionMaSlope, colors = c('red', 'green', 'black')) # c('#7F7F7F', '#17BECF') 
# fig2 <- fig2 %>% layout(yaxis = list(title = "MA Slope"))
# 
# # create rangeselector buttons
# rs <- list(visible = TRUE, x = 0.5, y = -0.055,
#            xanchor = 'center', yref = 'paper',
#            font = list(size = 9),
#            buttons = list(
#              list(count=1,
#                   label='RESET',
#                   step='all'),
#              list(count=1,
#                   label='1 YR',
#                   step='year',
#                   stepmode='backward'),
#              list(count=3,
#                   label='3 MO',
#                   step='month',
#                   stepmode='backward'),
#              list(count=1,
#                   label='1 MO',
#                   step='month',
#                   stepmode='backward')
#            ))
# 
# # subplot with shared x axis
# fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
#                shareX = TRUE, titleY = TRUE)
# fig <- fig %>% layout(title = paste0("FXSBUSD", ": ", date-days," - ", date),
#                       xaxis = list(rangeselector = rs),
#                       legend = list(orientation = 'h', x = 0.5, y = 1,
#                                     xanchor = 'center', yref = 'paper',
#                                     font = list(size = 10),
#                                     bgcolor = 'transparent'))
# 
# fig %>% layout(plot_bgcolor = "#bababa", # change color of plots
#                paper_bgcolor = "#bababa", # change color of layout behind plots
#                yaxis = list(fixedrange = FALSE)) # Allow y axis zoom


#### Tests ####

