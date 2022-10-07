
#### Loading libraries ####

# install.packages("binancer")
# install.packages("TTR")
library(binancer)
library(ggplot2)
library(scales)
library(data.table)
library(TTR)
library(stringr)


#### Initial data ####

binance_coins()
binance_coins_prices() %>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE)


(klines <- binance_klines('ADABNB', interval = '1d'))


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

cbind(klines, 
      (BBands(klines[, .(close)]) %>% data.table())[, .(dn, up, pctB)] # drop mavg because is the same than rollmean20
      ) -> klines
klines


#### MACD ####

cbind(klines,
      (MACD(klines[, .(close)]) %>% data.table())
      ) -> klines
klines[, macdChange := macd < signal]
klines


#### RSI ####

cbind(klines,
      (RSI(klines[, .(close)], n = 6) %>% data.table())[, .(RSI = `.`)] # By defaul, the MA is 14, but binance graphics take it on 6
      ) -> klines
klines


#### Alert flags ####
# Bollinger Bands
klines[low <= dn | high >= up] %>% View
klines[low <= dn, BB_flag := 1]
klines[high >= up, BB_flag := 2]

# RSI
klines[RSI <= 30 | RSI >= 70] %>% View
klines[RSI <= 30, RSI_flag := 1]
klines[RSI >= 70, RSI_flag := 2]


klines[, .(symbol, open_time, open, high, low, close, volume, trades, 
           dn, up, BB_flag, 
           RSI, RSI_flag)] %>% View


# La principal bandera es RSI_flag. 
# Si RSI_flag esta se activa y también está activada BB_flag, entonces en los días posteriores se debe activar BB_flag hasta que RSI_flag se desactive.
# Cuando esto pase entonces es señal de venta o compra
# Si al día siguiente que se desactiva RSI_flag, está activado BB_flag, entonces no es señal de compra o venta aún, porque aún no hay patrón de que se detuvo la bajada o subida


#### Notes #####

# Revisar si aumentan o disminuyen significativamente los trades, el volumen u otra variable cuando se activan alertas de venta o compra
# Plot close, close_time and volume








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



