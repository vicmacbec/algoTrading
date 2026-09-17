#### Script information ####

# https://plotly.com/r/candlestick-charts/
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ04-charting-with-tidyquant.html


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
library(quantmod)


#### Initial Data ####

# # get data
# getSymbols("AAPL",src='yahoo')
# AAPL

pair <- "AXSBUSD"

days <- 3
date <-  as.Date("2022-01-10") # "2022-01-10" "2021-10-08" "2022-02-03" "2021-07-20", Sys.Date() # Last day
klines <- NULL
for(day in days:0){
  tmp <- binance_klines(pair, limit = 24*60/5, interval = '5m', start_time = date - day)
  klines <- rbind(klines, tmp)
}
# klines <- binance_klines('AXSBUSD', interval = '1d') # 'BTCUSDT'
klines


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

# Candle color #
klines2[, direction := ifelse(close >= open, 'Increasing', 'Decreasing')]
klines2


#### Plotly ####

# Candles #

# i <- list(line = list(color = '#17BECF'))
# d <- list(line = list(color = '#7F7F7F'))
i <- list(line = list(color = 'green'))
d <- list(line = list(color = 'red'))

# plot candlestick chart
# rm(fig)
fig <- klines2 %>% plot_ly(x = ~open_time, type="candlestick",
                           open = ~open, close = ~close,
                           high = ~high, low = ~low, name = ~symbol,
                           increasing = i, decreasing = d) 
fig <- fig %>% add_lines(x = ~open_time, y = ~sslUp , name = "SSL",
                         line = list(color = 'green', width = 0.5),
                         legendgroup = "SSL Channel",
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% add_lines(x = ~open_time, y = ~sslDown, name = "SSL",
                         line = list(color = 'red', width = 0.5),
                         legendgroup = "SSL Channel", inherit = F,
                         showlegend = FALSE, hoverinfo = "none") 
fig <- fig %>% add_lines(x = ~open_time, y = ~ema, name = "EMA",
                         line = list(color = 'yellow', width = 0.5),
                         legendgroup = "EMA",
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2 <- klines2 
fig2 <- fig2 %>% plot_ly(x=~open_time, y=~volume, type='bar', name = paste0(klines2[, unique(symbol)], " Volume"),
                         color = ~direction, colors = c('green', 'red')) # c('#7F7F7F', '#17BECF') 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# subplot with shared x axis
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)
fig <- fig %>% layout(title = paste0(klines2[, unique(symbol)], ": ", date-days," - ", date),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

fig %>% layout(plot_bgcolor = "#bababa", # change color of plots
               paper_bgcolor = "#bababa", # change color of layout behind plots
               yaxis = list(fixedrange = FALSE)) # Allow y axis zoom

