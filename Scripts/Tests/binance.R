
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
library(config)


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
# binance_credentials(key = key_get("Binance_key"), secret = key_get("Binance_secret"))
kp <- config::get(value = "Binance")
binance_credentials(key = kp$key, secret = kp$secret)


# Information about account
binance_ping()
binance_account()
binance_all_orders(c("ENJBUSD")) # SOLBUSD
binance_balances(usdt = TRUE)[order(-usd)]
binance_balances(usdt = TRUE)[, sum(usd, na.rm = TRUE)]
binance_mytrades(c("SOLBUSD", "XRPBUSD", "ENJBUSD"))


# Orders
binance_new_order(symbol = "ENJBUSD", side = "BUY", type = "LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = 10, price = 1.20, 
                  test = TRUE) 
binance_new_order(symbol = "ENJBUSD", side = "BUY", type = "STOP_LOSS_LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = 10, price = 1.20, stop_price = 1.01,
                  test = TRUE) 
binance_open_orders("ENJBUSD")
binance_query_order("ENJBUSD", order_id = 247813338) # Only works with real order_id
binance_cancel_order("ENJBUSD", order_id = 247813338)


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
binance_filters("BTCBUSD") -> tmp
tmp


binance_avg_price("SOLBUSD") -> tmp
tmp
binance_trades("FXSBUSD") -> tmp
tmp
binance_ticker_24hr("SOLBUSD") -> tmp
tmp
binance_ticker_all_books() -> tmp
tmp
binance_ticker_book("SOLBUSD") -> tmp
tmp
binance_ticker_price("SOLBUSD") -> tmp
tmp
