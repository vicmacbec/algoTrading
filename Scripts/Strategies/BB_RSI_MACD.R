#### Script information ####

# Trade strategy #

# La principal bandera es RSI_flag. 
# Si RSI_flag se activa y también está activada BB_flag, 
# entonces en los días posteriores se debe activar BB_flag hasta que RSI_flag se desactive.
# Cuando esto pase entonces es señal de venta o compra

# Si al día siguiente que se desactiva RSI_flag, sigue activado BB_flag, 
# entonces es señal de compra o venta con mayor fuerza, porque aún sigue en descuento o a sobreprecio

# Seguir comprando o vendiendo si el precio baja o sube respectivamente 
# para ir promediando (con respecto a la compra o venta de la alerta) 
# hasta que se confirme cambio de tendencia con el MACD


# Notes #

# Eliminar pares que tengan BUSDT
# Revisar si aumentan o disminuyen significativamente los trades, el volumen u otra variable 
# cuando se activan alertas de venta o compra
# Plot close, close_time and volume
# Calcular cuántos cambios de RSI_flag_change_cum hay por cada macdChange_change_cum (hay algún patrón?)
# plot vertical histogram of prices (or Volume profile)

# Cuidado con alertas que el mercado tiene tendencia bajista,
# se activa la alerta por subida, 
# los siguientes días sigue bajando
# ejemplo: ADABNB 2021-02-06 18:00:00
# posible solución: Agregar lag a macdChange y que el filtro de kline5 se base en él 
# esta posible solución también arreglaría la parte de vender todo porque el mercado cambió de tendencia


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
pairs2Follow <- NULL
alertsQty <- NULL
tradesNow <- NULL
allData <- NULL
yields <- NULL

initialInvestment <- 1000
initialRate <- 0.01
avgRate <- 0.01

limit <- 500


for(pair in BUSDpairs){
  # pair <- "ENJBUSD" # "AXSBUSD" # "SANDBUSD"# "SOLBUSD"# "ADABNB"#"XRPBUSD"
  print(pair)
  
  klines <- binance_klines(pair, limit = limit, interval = '1d') # 'BTCUSDT'
  print(paste0("Number of registers from ", pair, " pair: ", klines %>% nrow()))
  print("")
  
  #### Strategy ####
  print("#### Strategy ####")
  
  # Moving average #
  klines[, rollmean05 := frollmean(close, n = an(5, .N), adaptive = TRUE)]
  klines[, rollmean10 := frollmean(close, n = an(10, .N), adaptive = TRUE)]
  klines[, rollmean20 := frollmean(close, n = an(20, .N), adaptive = TRUE)]
  klines[, rollmean51 := frollmean(close, n = an(51, .N), adaptive = TRUE)]
  # klines
  
  
  # Bollinger Bands #
  cbind(klines, 
        (BBands(klines[, .(close)]) %>% data.table())[, .(dn, up, pctB)] # drop mavg because is the same than rollmean20
        ) -> klines
  # klines
  
  
  # MACD #
  cbind(klines,
        (MACD(klines[, .(close)]) %>% data.table())
        ) -> klines
  klines[, macdChange := ifelse(macd < signal, "down", "up")]
  # klines
  
  
  # RSI #
  # By defaul, the MA is 14, but binance graphics take it on 6
  cbind(klines,
        (RSI(klines[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)] 
        ) -> klines
  # klines
  
  
  #### Alert flags ####
  print("#### Alert flags ####")
  
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
  
  
  # Compute trades #
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
  
  
  #### Testing strategy ####
  print("#### Testing strategy ####")
  
  # Yields #
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
  
}
pairs2Follow
alertsQty
tradesNow
allData
yields


#### Plots ####
print("#### Plots ####")

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


#### Saving data ####
print("#### Saving data ####")


#### Tests ####
