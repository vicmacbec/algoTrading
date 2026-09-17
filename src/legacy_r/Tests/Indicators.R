#### Script information ####


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

# n <- 4
# len <- 7
an <- function(n, len){
  if(len-n > 0){
    c(seq.int(n), rep(n, len-n)) 
  }else{
    seq.int(len)
  }
}


#### Load data ####
print("#### Load data ####")

# binance_coins()
# binance_coins_prices() #%>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs
klines <- binance_klines('ENJBUSD', interval = '4h') # 'BTCUSDT'


#### Initial parameters ####
print("#### Initial parameters ####")


#### Strategy ####
print("#### Strategy ####")

# Moving average #
klines[, rollmean05 := frollmean(close, n = an(5, .N), adaptive = TRUE)]
klines[, rollmean10 := frollmean(close, n = an(10, .N), adaptive = TRUE)]
klines[, rollmean20 := frollmean(close, n = an(20, .N), adaptive = TRUE)]
klines[, rollmean51 := frollmean(close, n = an(51, .N), adaptive = TRUE)]
klines


# Bollinger Bands #
# drop mavg because is the same than rollmean20
klines[, c("dn", "up", "pctB") := (BBands(klines[, .(close)]) %>% data.table())[, .(dn, up, pctB)]] 
klines


# MACD #
klines[, c("macd", "signal") := (MACD(klines[, .(close)]) %>% data.table())]
klines[, macdChange := ifelse(macd < signal, "down", "up")]
klines


# RSI #
# By defaul, the MA is 14, but binance graphics take it on 6
klines[, c("RSI") := (RSI(klines[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)]] 
klines


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

# Número de alertas que han habido
ceiling(klines4[, max(alert_change_cum)]/2)

# Número de alertas de ventas (2) y compras (1)
(klines4[alert == TRUE, .(RSI_flag, alert, alert_change_cum)] %>% unique())[, .(.N), keyby = .(RSI_flag)]

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

klines5[, .(symbol, open_time, close, 
            up, dn, BB_flag, 
            RSI, RSI_flag, RSI_flag_change_cum, 
            alert, alert_change, alert_change_cum, 
            macdChange, 
            baseTradePrice, baseTradePrice_cum, tradePrice)] %>% View


#### Moving Average Slope ####

rad2degree <- 180/3.14159265359

klines <- binance_klines('SOLBUSD', interval = '4h') # 'BTCUSDT'
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


#### Testing strategy ####
print("#### Testing strategy ####")


#### Plots ####
print("#### Plots ####")

ggplot(klines, aes(close_time, close)) + geom_line()


p <- ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  # geom_line(aes(y = ohlc4)) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c('#1a9850', '#d73027')) + # RdYlGn
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
p %>% ggplotly()


klines5 <- rbindlist(lapply(
  c('ETHBTC', 'ARKBTC', 'NEOBTC', 'IOTABTC'),
  binance_klines,
  interval = '15m',
  limit = 4*24))
ggplot(klines5, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_color_manual(values = c('#1a9850', '#d73027')) +
  facet_wrap(~symbol, scales = 'free', nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#### Saving data ####
print("#### Saving data ####")


#### Tests ####
