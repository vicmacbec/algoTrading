#### Script information ####
#
# Trade strategy #
#
#
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

  #### Strategy ####
  print("#### Strategy ####")
  
  ## Summary data: All data ##
  allData <- rbind(allData, klines)
  #
  
  ## Summary data: Wicks ##
  klines[, .(symbol, open_time, open, high, close, low,
             wickRange = high - open, 
             wickPercent = (high - open)/close, 
             candle = ifelse(close >= open, "green", "red"))] -> kline2
  candle <- rbind(candle,
                  kline2[, .(.N), keyby = .(symbol, candle)] %>% dcast(symbol ~ candle, value.var = "N"))
  # kline2[, wickRange] %>% summary()
  # kline2[candle == "green", wickRange] %>% summary()
  # kline2[candle == "red", wickRange] %>% summary()
  # kline2[, wickPercent] %>% summary()
  # kline2[candle == "green", wickPercent] %>% summary()
  # kline2[candle == "red", wickPercent] %>% summary()
  
  #### Alert flags ####
  print("#### Alert flags ####")
  
  #### Testing strategy ####
  print("#### Testing strategy ####")
  
  kline2[, .(symbol, open_time, open, high, low, close, wickRange, wickPercent, candle,
             yield1 = (1 + kline[candle == "red", (wickPercent %>% summary())[2]]) %>% as.double(),
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
               )] -> kline3
  
  pumpNgo <- rbind(pumpNgo,
                   kline3
                   )
  
  # Win percentage
  pumpNgoRates <- rbind(pumpNgoRates,
                        kline3[, .(.N), keyby = .(symbol, win)
                               ][, .(symbol, winRate = sum(ifelse(win == TRUE, N, NA), na.rm = TRUE)/sum(N))][1]
                        )
  
  #
  
}
candle
pumpNgo
pumpNgoRates



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


#### Plots ####
print("#### Plots ####")

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


#### Saving data ####
print("#### Saving data ####")


#### Tests ####
