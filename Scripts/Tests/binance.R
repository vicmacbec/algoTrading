
#### Loading libraries ####

# install.packages("binancer")
# install.packages("TTR")
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


#### Initial data ####

binance_coins()
binance_coins_prices() #%>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs
binance_symbols() -> allpairs

historicBUSDpairs <- NULL
for(pair in BUSDpairs){
  print(pair)
  
  tmp <- binance_klines(pair, interval = '1d')
  historicBUSDpairs <- rbind(historicBUSDpairs, tmp)
}
historicBUSDpairs
klines <- historicBUSDpairs

(klines <- binance_klines('PERPBUSD', interval = '1d')) # 'BTCUSDT'



#### Binancer Functions ####

# Credentials
binance_credentials(key = key_get("Binance_key"), secret = key_get("Binance_secret"))

# Information about account
binance_ping()
binance_account()
binance_all_orders(c("ENJBUSD")) # SOLBUSD
binance_balances(usdt = TRUE)[order(-usd)]
binance_mytrades(c("SOLBUSD", "XRPBUSD", "ENJBUSD"))


# Orders
binance_new_order(symbol = "ENJBUSD", side = "BUY", type = "LIMIT", 
                  quantity = 10, price = 1.20,
                  time_in_force = "GTC", # Good til cancelled
                  test = FALSE) 
binance_open_orders("ENJBUSD")
binance_query_order("ENJBUSD", order_id = 238191114) # Only works with real order_id
binance_cancel_order("ENJBUSD", order_id = 238191114)


# Criptos information
binance_coins()
binance_coins_prices()
binance_symbols()
binance_depth("ENJBUSD")
binance_klines("ENJBUSD")
binance_ticker_all_prices()


# Binance information
binance_exchange_info()
binance_time()


# Don't work
binance_avg_price("SOLBUSD")
binance_filters("ENJBUSD")
binance_trades("SOLBUSD")
binance_ticker_24hr("SOLBUSD")
binance_ticker_all_books()
binance_ticker_book("SOLBUSD")
binance_ticker_price("SOLBUSD")
binance_trades("ENJBUSD")


#### Moving average ####

# n <- 4
# len <- 7
an <- function(n, len){
  if(len-n > 0){
    c(seq.int(n), rep(n, len-n)) 
  }else{
    seq.int(len)
  }
}

klines[, rollmean05 := frollmean(close, n = an(5, .N), adaptive = TRUE)]
klines[, rollmean10 := frollmean(close, n = an(10, .N), adaptive = TRUE)]
klines[, rollmean20 := frollmean(close, n = an(20, .N), adaptive = TRUE)]
klines[, rollmean51 := frollmean(close, n = an(51, .N), adaptive = TRUE)]
klines


#### Bollinger Bands ####

# drop mavg because is the same than rollmean20
klines[, c("dn", "up", "pctB") := (BBands(klines[, .(close)]) %>% data.table())[, .(dn, up, pctB)]] 
klines

#### MACD ####

klines[, c("macd", "signal") := (MACD(klines[, .(close)]) %>% data.table())]
klines[, macdChange := ifelse(macd < signal, "down", "up")]
klines


#### RSI ####

# By defaul, the MA is 14, but binance graphics take it on 6
klines[, c("RSI") := (RSI(klines[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)]] 
klines


#### Alert flags ####

# Bollinger Bands
# klines[low <= dn | high >= up]# %>% View
klines[low <= dn, BB_flag := 1]
klines[high >= up, BB_flag := 2]
klines[is.na(BB_flag), BB_flag := 0]

# RSI
# klines[RSI <= 30 | RSI >= 70]# %>% View
klines[RSI <= 30, RSI_flag := 1]
klines[RSI >= 70, RSI_flag := 2]
klines[is.na(RSI_flag), RSI_flag := 0]

klines[, RSI_flag_lag := c(NA, RSI_flag[-.N])]
klines[, RSI_flag_change := ifelse(RSI_flag != RSI_flag_lag, 1, 0)]
klines[, RSI_flag_change_cum := cumsum(ifelse(is.na(RSI_flag_change), 0, RSI_flag_change))]

# Reduce columns
klines[, .(symbol, open_time, open, high, low, close, volume, trades, 
           dn, up, BB_flag, 
           RSI, RSI_flag, #RSI_flag_lag, RSI_flag_change, 
           RSI_flag_change_cum, 
           macdChange)
       ] -> klines2

# Add RSI combined Bollinger Bands alerts
klines2[, alert := ifelse(RSI_flag != 0 & any(RSI_flag == BB_flag), TRUE, ""), 
        keyby = .(RSI_flag_change_cum)
        ]

# Add change of tendency (MACD) alerts
klines2[, macdChange_lag := c(NA, macdChange[-.N])]
klines2[, macdChange_change := ifelse(macdChange != macdChange_lag, 1, 0)]
klines2[, macdChange_change_cum := cumsum(ifelse(is.na(macdChange_change), 0, macdChange_change))]

klines2[, .(symbol, open_time, open, high, low, close, volume, trades, 
            dn, up, BB_flag, 
            RSI, RSI_flag, RSI_flag_change_cum, 
            alert, 
            macdChange, macdChange_change_cum)
        ] -> klines3


#### Compute trades ####
klines3[, alert_lag := c(NA, alert[-.N])]
klines3[, alert_change := ifelse(alert != alert_lag, 1, 0)]
klines3[, alert_change_cum := cumsum(ifelse(is.na(alert_change), 0, alert_change))]

klines3[, .(symbol, open_time, open, high, low, close, volume, trades, 
            dn, up, BB_flag, 
            RSI, RSI_flag, RSI_flag_change_cum, 
            alert, alert_change, alert_change_cum,
            macdChange, macdChange_change_cum)
        ] -> klines4

# Número de alertas que han habido
ceiling(klines4[, max(alert_change_cum)]/2)

# Número de alertas de ventas (2) y compras (1)
(klines4[alert == TRUE, .(RSI_flag, alert, alert_change_cum)] %>% unique())[, .(.N), keyby = .(RSI_flag)]
# Si hay más ofertas de venta que de compra quiere decir que este par tiende a bajar más de precio
# Apostar a long por los que tienden a subir más de precio que bajar
# Apostar short por los que tienden a bajar más de precio que subir
# También depende del tamaño del periodo que se quedó en alert_flag = TRUE

# Precios para comprar o vender
klines4[alert == "" & alert_change == 1, 
        baseTradePrice := close]
klines4[, baseTradePrice_cum := cumsum(ifelse(is.na(baseTradePrice), 0, baseTradePrice)), 
        keyby = .(alert_change_cum, macdChange)]

klines4[, .(symbol, open_time, open, high, low, close, volume, trades, 
            dn, up, BB_flag, 
            RSI, RSI_flag, RSI_flag_change_cum, 
            alert, alert_change, alert_change_cum,
            macdChange, macdChange_change_cum, 
            baseTradePrice, baseTradePrice_cum)
          ][order(open_time)] -> klines5

klines5[macdChange == "up" & baseTradePrice_cum > 0 & close >= baseTradePrice_cum & alert == "" & RSI_flag != 1, 
        tradePrice := close]
klines5[macdChange == "down" & close <= baseTradePrice_cum & alert == "" & RSI_flag != 2, 
        tradePrice := close]
klines5 <- klines5[order(open_time)]
# obtener promedio de tradePrice por alert_change_cum ignorando NA's
# Calcular cuánto de la moneda opuesta se compra/vende
# Si alert change != 1, tomar otra estrategia de compra/venta
# klines5[, operation := ifelse(macdChange == "down", -tradePrice * 10, tradePrice * 10)] 
# klines5[, operation_cum := cumsum(ifelse(is.na(operation), 0, operation))]

klines5[, .(symbol, open_time, close, 
            up, dn, BB_flag, 
            RSI, RSI_flag, RSI_flag_change_cum, 
            alert, alert_change, alert_change_cum, 
            macdChange, 
            baseTradePrice, baseTradePrice_cum, tradePrice)] %>% View


# Cuidado con alertas que el mercado tiene tendencia bajista,
# se activa la alerta por subida, 
# los siguientes días sigue bajando
# ejemplo: ADABNB 2021-02-06 18:00:00
# posible solución: Agregar lag a macdChange y que el filtro de kline5 se base en él 
# esta posible solución también arreglaría la parte de vender todo porque el mercado cambió de tendencia



#### Trade strategy ####

# La principal bandera es RSI_flag. 
# Si RSI_flag se activa y también está activada BB_flag, 
# entonces en los días posteriores se debe activar BB_flag hasta que RSI_flag se desactive.
# Cuando esto pase entonces es señal de venta o compra

# Si al día siguiente que se desactiva RSI_flag, sigue activado BB_flag, 
# entonces es señal de compra o venta con mayor fuerza, porque aún sigue en descuento o a sobreprecio

# Seguir comprando o vendiendo si el precio baja o sube respectivamente 
# para ir promediando (con respecto a la compra o venta de la alerta) 
# hasta que se confirme cambio de tendencia con el MACD


#### Notes #####

# Eliminar pares que tengan BUSDT
# Revisar si aumentan o disminuyen significativamente los trades, el volumen u otra variable 
# cuando se activan alertas de venta o compra
# Plot close, close_time and volume
# Calcular cuántos cambios de RSI_flag_change_cum hay por cada macdChange_change_cum (hay algún patrón?)
# plot vertical histogram of prices (or Volume profile)






#### Plots ####

ggplot(klines, aes(close_time, close)) + geom_line()


ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c('#1a9850', '#d73027')) # RdYlGn


klines <- rbindlist(lapply(
  c('ETHBTC', 'ARKBTC', 'NEOBTC', 'IOTABTC'),
  binance_klines,
  interval = '15m',
  limit = 4*24))
ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_color_manual(values = c('#1a9850', '#d73027')) +
  facet_wrap(~symbol, scales = 'free', nrow = 2)


#### TEST ####


# # n <- 4
# # len <- 7
an <- function(n, len){
  if(len-n > 0){
    c(seq.int(n), rep(n, len-n))
  }else{
    seq.int(len)
  }
}


BUSDpairs %>% data.table() #%>% View
pairs2Follow <- NULL
alertsQty <- NULL
tradesNow <- NULL
allData <- NULL
yields <- NULL
candle <- NULL
pumpNgo <- NULL
pumpNgoRates <- NULL

initialInvestment <- 1000
initialRate <- 0.01
avgRate <- 0.01

limit <- 500 

for(pair in BUSDpairs){
  # pair <- "AXSBUSD" # "SANDBUSD"# "SOLBUSD"# "ADABNB"#"XRPBUSD"
  print(pair)
  
  klines <- binance_klines(pair, limit = limit, interval = '1d') # 'BTCUSDT'
  print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
  print("")
  
  
  #### Moving average ####
  
  klines[, rollmean05 := frollmean(close, n = an(5, .N), adaptive = TRUE)]
  klines[, rollmean10 := frollmean(close, n = an(10, .N), adaptive = TRUE)]
  klines[, rollmean20 := frollmean(close, n = an(20, .N), adaptive = TRUE)]
  klines[, rollmean51 := frollmean(close, n = an(51, .N), adaptive = TRUE)]
  # klines
  
  
  #### Bollinger Bands ####
  
  cbind(klines, 
        (BBands(klines[, .(close)]) %>% data.table())[, .(dn, up, pctB)] # drop mavg because is the same than rollmean20
  ) -> klines
  # klines
  
  
  #### MACD ####
  
  cbind(klines,
        (MACD(klines[, .(close)]) %>% data.table())
  ) -> klines
  klines[, macdChange := ifelse(macd < signal, "down", "up")]
  # klines
  
  
  #### RSI ####
  
  # By defaul, the MA is 14, but binance graphics take it on 6
  cbind(klines,
        (RSI(klines[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)] 
  ) -> klines
  # klines

  
  #### Alert flags ####
  # Bollinger Bands
  # klines[low <= dn | high >= up]# %>% View
  klines[low <= dn, BB_flag := 1]
  klines[high >= up, BB_flag := 2]
  klines[is.na(BB_flag), BB_flag := 0]
  
  # RSI
  # klines[RSI <= 30 | RSI >= 70]# %>% View
  klines[RSI <= 30, RSI_flag := 1]
  klines[RSI >= 70, RSI_flag := 2]
  klines[is.na(RSI_flag), RSI_flag := 0]
  
  klines[, RSI_flag_lag := c(NA, RSI_flag[-.N])]
  klines[, RSI_flag_change := ifelse(RSI_flag != RSI_flag_lag, 1, 0)]
  klines[, RSI_flag_change_cum := cumsum(ifelse(is.na(RSI_flag_change), 0, RSI_flag_change))]
  
  # Reduce columns
  klines[, .(symbol, open_time, open, high, low, close, volume, trades, 
             dn, up, BB_flag, 
             RSI, RSI_flag, #RSI_flag_lag, RSI_flag_change, 
             RSI_flag_change_cum, 
             macdChange)
         ] -> klines2
  
  # Add RSI combined Bollinger Bands alerts
  klines2[, alert := ifelse(RSI_flag != 0 & any(RSI_flag == BB_flag), TRUE, ""), 
          keyby = .(RSI_flag_change_cum)
          ]
  
  # Add change of tendency (MACD) alerts
  klines2[, macdChange_lag := c(NA, macdChange[-.N])]
  klines2[, macdChange_change := ifelse(macdChange != macdChange_lag, 1, 0)]
  klines2[, macdChange_change_cum := cumsum(ifelse(is.na(macdChange_change), 0, macdChange_change))]
  
  klines2[, .(symbol, open_time, open, high, low, close, volume, trades, 
              dn, up, BB_flag, 
              RSI, RSI_flag, RSI_flag_change_cum, 
              alert, 
              macdChange, macdChange_change_cum)
          ] -> klines3
  
  
  #### Compute trades ####
  klines3[, alert_lag := c(NA, alert[-.N])]
  klines3[, alert_change := ifelse(alert != alert_lag, 1, 0)]
  klines3[, alert_change_cum := cumsum(ifelse(is.na(alert_change), 0, alert_change))]
  
  klines3[, .(symbol, open_time, open, high, low, close, volume, trades, 
              dn, up, BB_flag, 
              RSI, RSI_flag, RSI_flag_change_cum, 
              alert, alert_change, alert_change_cum,
              macdChange, macdChange_change_cum)
          ] -> klines4

  # Precios para comprar o vender
  klines4[alert == "" & alert_change == 1, 
          baseTradePrice := close]
  klines4[, baseTradePrice_cum := cumsum(ifelse(is.na(baseTradePrice), 0, baseTradePrice)), 
          keyby = .(alert_change_cum, macdChange)]
  
  klines4[, .(symbol, open_time, open, high, low, close, volume, trades, 
              dn, up, BB_flag, 
              RSI, RSI_flag, RSI_flag_change_cum, 
              alert, alert_change, alert_change_cum,
              macdChange, macdChange_change_cum, 
              baseTradePrice, baseTradePrice_cum)
          ][order(open_time)] -> klines5
  
  klines5[macdChange == "up" & baseTradePrice_cum > 0 & close >= baseTradePrice_cum & alert == "" & RSI_flag != 1, 
          tradePrice := close]
  klines5[macdChange == "down" & close <= baseTradePrice_cum & alert == "" & RSI_flag != 2, 
          tradePrice := close]
  klines5 <- klines5[order(open_time)]
  
  
  #### Yields ####
  klines5[, ":="(totalMount = initialInvestment,
                 orderMount = initialInvestment*initialRate,
                 tradeNumber = NA)]


  # Computing totalMount, orderMount and tradeNumber (the calculation is based after the buy/sell day)
  flag <- FALSE
  # tradeNmbr <- 0
  for(day in 2:nrow(klines5)){
    #day <- 97

    if(is.na(klines5$tradePrice[day])){
      klines5$totalMount[day] <- klines5$totalMount[day-1]
      klines5$orderMount[day] <- klines5$orderMount[day-1]
      klines5$tradeNumber[day] <- klines5$tradeNumber[day-1]
    }else{
      if(klines5$tradePrice[day] > 0 & !flag){
        if(klines5$macdChange[day] == "down"){
          flag <- TRUE
          
          totalMount <- klines5$totalMount[day]
          orderMount <- klines5$orderMount[day]
          macdChange <- klines5$macdChange[day]
          
          klines5$tradeNumber[day] <- 1
        }else{
          klines5$totalMount[day] <- klines5$totalMount[day-1]
          klines5$orderMount[day] <- klines5$orderMount[day-1]
          klines5$tradeNumber[day] <- klines5$tradeNumber[day-1]
        }
      }else{
        klines5$totalMount[day] <- totalMount + ifelse(macdChange == "down", -orderMount, orderMount)
        klines5$orderMount[day] <- klines5$totalMount[day] * ifelse(klines5$alert_change[day] == 1, initialRate, avgRate)
        klines5$tradeNumber[day] <- klines5$tradeNumber[day-1] + 1

        totalMount <- klines5$totalMount[day]
        orderMount <- klines5$orderMount[day]
        macdChange <- klines5$macdChange[day]
      }
    }
  }

  # Make as na all totalMount and order Mount that weren't theri buy/sell day and then apply na.locf fromLast
  klines5[is.na(tradePrice) | is.na(tradeNumber), ":="(totalMount = NA, orderMount = NA)]
  # Get the last no Na in totalMount in order to avoid error when na.locf function is applied
  lastNoNA <- max(ifelse(is.na(klines5$totalMount), NA, row(klines5)), na.rm = TRUE)
  klines5[1:lastNoNA, 
          ":="(totalMount = na.locf(totalMount, fromLast = TRUE))]

  klines5[, Qty := orderMount / close]
  # Problema: Valores cumsum se va a negativo
  # klines5[, Qty_cum := cumsum(ifelse(is.na(Qty), 0, ifelse(macdChange == "down", Qty, -Qty)))]
  klines5[, Qty_cum := accumulate(ifelse(is.na(Qty), 0, ifelse(macdChange == "down", Qty, -Qty)),  
                                  ~ ifelse(.x + .y < 0, 0, .x + .y))]
  
  klines5[, orderMount_cum := cumsum(ifelse(is.na(orderMount) | Qty_cum == 0, 
                                            0, 
                                            ifelse(macdChange == "down", orderMount, -orderMount)))]
  klines5[, avgPrice := orderMount_cum / Qty_cum]

  klines5[, win := close >= avgPrice]
  klines5[, totalMountAll := totalMount + 
                             ifelse(Qty_cum == 0, 0, ifelse(macdChange == "down", -orderMount, orderMount)) + 
                             (Qty_cum*close)]
  # Yield is higher for those symbols that get negative Qty_cum
  klines5[, yield := (totalMountAll - initialInvestment)/initialInvestment]
  
  
  ## Summary data: pairs to follow ##
  tmp1 <- klines5[.N][alert == TRUE, .(symbol, RSI_flag, macdChange)]
  if(tmp1 %>% nrow() > 0){
    pairs2Follow <- rbind(pairs2Follow, tmp1) 
  }
  #
  
  ## Summary data: Alerts quantity ##
  # Número de alertas que han habido
  tmp2.1 <- ceiling(klines4[, max(alert_change_cum)]/2)
  
  # Número de alertas de ventas (2) y compras (1)
  tmp2.2 <- (klines4[alert == TRUE, .(RSI_flag, alert, alert_change_cum)] %>% unique())[, .(.N), keyby = .(RSI_flag)]
  # Si hay más ofertas de venta que de compra quiere decir que este par tiende a bajar más de precio
  # Apostar a long por los que tienden a subir más de precio que bajar
  # Apostar short por los que tienden a bajar más de precio que subir
  # También depende del tamaño del periodo que se quedó en alert_flag = TRUE
  
  alertsQty <- rbind(alertsQty, 
                     data.table(symbol = klines5[, symbol] %>% unique(),
                                totalAlerts = tmp2.1,
                                buyAlert = tmp2.2[RSI_flag == 1, N],
                                saleAlert = tmp2.2[RSI_flag == 2, N]))
  #
  
  ## Summary data: trade now ##
  tmp3 <- klines5[.N
                  ][!is.na(tradePrice), 
                    .(symbol, open_time, open, high, low, close, volume, trades, 
                      dn, up, BB_flag, RSI, RSI_flag, alert, macdChange, 
                      baseTradePrice_cum, tradePrice)]
  
  if(tmp3 %>% nrow() > 0){
    tradesNow <- rbind(tradesNow, tmp3)
  }
  #
  
  ## Summary data: All data ##
  allData <- rbind(allData, klines5)
  #
  
  ## Summary data: Yields ##
  yields <- rbind(yields, 
                  klines5[, .(symbol, yield, max_yield = max(yield, na.rm = TRUE), min_yield = min(yield, na.rm = TRUE))
                          ][lastNoNA, .(symbol, yield, max_yield, min_yield)])
  #

  ## Summary data: Wicks ##
  klines5[, .(symbol, open_time, open, high, close, low,
              wickRange = high - open, 
              wickPercent = (high - open)/close, 
              candle = ifelse(close >= open, "green", "red"))] -> kline6
  candle <- rbind(candle,
                 kline6[, .(.N), keyby = .(symbol, candle)] %>% dcast(symbol ~ candle, value.var = "N"))
  # kline6[, wickRange] %>% summary()
  # kline6[candle == "green", wickRange] %>% summary()
  # kline6[candle == "red", wickRange] %>% summary()
  # kline6[, wickPercent] %>% summary()
  # kline6[candle == "green", wickPercent] %>% summary()
  # kline6[candle == "red", wickPercent] %>% summary()
  
  kline6[, .(symbol, open_time, open, high, low, close, wickRange, wickPercent, candle,
             yield1 = (1 + kline6[candle == "red", (wickPercent %>% summary())[2]]) %>% as.double(),
             # yield = (1 + frollapply(x = ifelse(candle == "red", wickPercent, NA), 
             #                         n = an(.N, .N), 
             #                         # adaptive = TRUE, 
             #                         FUN = function(x){summary(x)[2] %>% as.double()})),
             yield2 = (1 + roll_quantile(ifelse(candle == "red", wickPercent, NA), width = .N, min_obs = 1, p = 0.25))
             )
         ][, .(symbol, open_time, open, high, low, close, wickRange, wickPercent, candle,
               yield1, yield2,
               sell = open * yield2,
               win = high >= (open * yield2)
               )] -> kline7
  
  pumpNgo <- rbind(pumpNgo,
                   kline7
                   )
  
  # Win percentage
  pumpNgoRates <- rbind(pumpNgoRates,
                        kline7[, .(.N), keyby = .(symbol, win)
                               ][, .(symbol, winRate = sum(ifelse(win == TRUE, N, NA), na.rm = TRUE)/sum(N))][1]
                        )
  
  #
  
}
pairs2Follow
alertsQty
tradesNow
allData
yields
candle
pumpNgo
pumpNgoRates



p <- ggplot(allData, aes(y = yield, color = symbol, x = symbol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p %>% ggplotly()

p <- ggplot(allData[symbol %in% tradesNow[macdChange == "down", symbol]], aes(y = yield, color = symbol, x = symbol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p %>% ggplotly()

p <- ggplot(allData, aes(y = yield)) +
  geom_boxplot()
p %>% ggplotly()

allData[symbol == "SOLBUSD", .(volume, trades)] %>% cor()
allData[, .(volume, trades)] %>% cor()

enj 


lastDays <- 500

pumpNgo[, N := 1:.N, keyby = .(symbol)]
pumpNgo[, lastDay := .N - lastDays, keyby = .(symbol)]
pumpNgo2 <- pumpNgo[N > lastDay]

pumpNgo2[, realYield := ifelse(win == "TRUE", yield2 - 1, (close - open)/open)]
pumpNgo2[, earns := 10*cumprod(ifelse(is.na(realYield), 1, 1 + realYield)), keyby = .(symbol)]

# Get the last day to obtain the current earn
pumpNgo2[, tmp := max(N) == N, keyby = .(symbol)]
pumpNgo2[tmp == TRUE] #%>% View

pumpNgo2[tmp == TRUE, sum(earns) - 10*.N]


## 2nd phase
pumpNgo2[, close_cumMax := rev(cummax(rev(high))), keyby = .(symbol)] # Check if it can be sold in the future
pumpNgo2[, maxCount := close_cumMax %>% unique() %>% length(), keyby = .(symbol)] # Count all maximums 

pumpNgo2[, win2 := sell <= close_cumMax]
pumpNgo2[, realYield2 := ifelse(win2 == "TRUE", yield2 - 1, (close - open)/open)]
pumpNgo2[, earns2 := 10*cumprod(ifelse(is.na(realYield2), 1, 1 + realYield2)), keyby = .(symbol)]
pumpNgo2[, extraBuys := sum(win2 == FALSE & !is.na(win2)) + 1, keyby = .(symbol)]

pumpNgo2[, mountInvested := earns2/(1+realYield2)]
pumpNgo2[, currentClosePrice := last(close), keyby = .(symbol)]
pumpNgo2[, mountCurrent := (mountInvested/open)*currentClosePrice]
pumpNgo2[, currentInvMountValue := ifelse(win2 == FALSE, mountCurrent - mountInvested, NA)]


pumpNgo2[tmp == TRUE] %>% View
# Real earns (earn2 - investment)
pumpNgo2[tmp == TRUE, sum(earns2) - 10*.N]
pumpNgo2[win2 == FALSE, sum(currentInvMountValue)]
# pumpNgo2[tmp == TRUE, sum(extraBuys)] * 10
# pumpNgo2[tmp == TRUE, sum(earns2) - 10*.N] - pumpNgo2[tmp == TRUE, sum(extraBuys)] * 10
pumpNgo2[tmp == TRUE, sum(earns2) - 10*.N] + pumpNgo2[win2 == FALSE, sum(currentInvMountValue)]
pumpNgo2[tmp == TRUE, earns2] %>% summary()


# pumpNgoRates
pumpNgo2[, .(.N), keyby = .(symbol, win2)
         ][, .(winRate = sum(ifelse(win2 == TRUE, N, NA), na.rm = TRUE)/sum(N)), keyby = .(symbol)] -> pumpNgoRates

View(pumpNgoRates)

pumpNgo2[, .(symbol, open_time, open, high, low, close, 
             wickRange, wickPercent, candle, sell, win, win2, 
             percent_range = (high-low)/low)]
allData[, .(symbol, open_time, open, high, close, low, percent_range = (high-low)/low)] %>% View



# Fijarme si hay señales con indicadores de que ese día va a estar bajando
# En algoritmo ML, ese día sería clasificado como no comprar
# ¿Cómo gestionar el riesgo?, 
# ¿Qué porcentaje es el que se está dispuesto a perder?
# Depende cuánto es lo que se gana, si en promedio se gana el 80 % de los días un 1 % diario, ¿cuánto se acepta perder en el día sin que haya pérdidas en el histórico?
# En el trading, lo único que se controla es la pérdida.
# Regla 1,2, 3, 4
# Hacer backtesting con los movimientos como si hubieran pasado día a día
# 

# Probar pumpNgo con temporalidad semanal

# Probar estrategia de SSL con EMA 200 (https://www.youtube.com/watch?v=rOnv5yFuqT0)

# Plots
p1 <- ggplot(pumpNgo, aes(y = realYield2, color = symbol, x = symbol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1 %>% ggplotly()

p3 <- ggplot(pumpNgo, aes(y = realYield2)) +
  geom_boxplot()
p3 %>% ggplotly()

# p1 / p3

#  10     118.292  640    -521.7073
#  30     437.718 1006    -568.282
# 100    2682.481 1737     945.481
# 200   11214.84  1960    9254.836
# 300   57212.36  2210   55002.36
# 365  139133.4   2482  136635.4
# 400  337133     2389  334744
# 500 1426601     2370 1424231


# 10
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.042  10.000  10.411  10.418  10.949  12.349

# 30
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.202  10.316  11.486  11.547  12.485  19.598 

# 100
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.452  13.576  17.683  19.479  23.871  96.387 

# 200
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.91   17.71   27.77   44.96   53.56  456.69 

# 365
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 8.504   35.316  133.697  501.637  440.275 8111.119

# 500
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 8.5     37.7    221.4   5054.5   1143.3 330181.9 