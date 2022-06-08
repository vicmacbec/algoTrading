
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
minNotional <- binance_filters("ENJBUSD")[filterType == "MIN_NOTIONAL", minNotional]
price <- 0.682
binance_new_order(symbol = "ENJBUSD", side = "BUY", type = "LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = minNotional/price, price = price, 
                  test = TRUE) 
binance_new_order(symbol = "ENJBUSD", side = "SELL", type = "LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = 10, price = price, 
                  test = TRUE) 
# binance_new_order(symbol = "ENJBUSD", side = "BUY", type = "STOP_LOSS_LIMIT", 
#                   time_in_force = "GTC", # Good til cancelled
#                   quantity = 10, price = 1.20, stop_price = 1.01,
#                   test = TRUE) 
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
binance_exchange_info()$symbols[symbol %in% BUSDpairs] %>% View

binance_time()
binance_filters("ENJBUSD") -> tmp
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


#### Computing allowed quantity coins to make a valid order ####
# https://stackoverflow.com/questions/65659346/how-to-place-a-binance-order-through-r

#Fetching the current price of crypto of interest
curr_price <- binance_ticker_price("ENJBUSD")[, price]
#Fetch your wallet balances
avail_busd <- binance_balances()[asset == "BUSD", as.numeric(free)]
#Calculate possible coin quantity
buy_quantity <- avail_busd/curr_price

# Get minimum possible coin quantity
filters <- binance_filters("ENJBUSD")
price <- curr_price
minNotional <- binance_filters("ENJBUSD")[filterType == "MIN_NOTIONAL", minNotional]
buy_quantity <- minNotional/price

# # Price
# quot <- (price - filters[filterType == 'PRICE_FILTER', minPrice]) / filters[filterType == 'PRICE_FILTER', tickSize]
# quot
# stopifnot(abs(quot - round(quot)) < 1e-10)

#Quantity
quot <- (buy_quantity - filters[filterType == 'LOT_SIZE', minQty]) / filters[filterType == 'LOT_SIZE', stepSize]
quot
stopifnot(abs(quot - round(quot)) < 1e-10)

decimalplaces <- function(x) {
  if (class(x)=="character") {
    x<-gsub("(.*)(\\.)|([0]*$)","",x)
    nchar(x)
  } else if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

quantity <- (quot*filters[filterType == 'LOT_SIZE', stepSize]) + filters[filterType == 'LOT_SIZE', minQty]
#Remember to change quot to 0
quantity <- (0*filters[filterType == 'LOT_SIZE', stepSize]) + filters[filterType == 'LOT_SIZE', minQty]
#Since anything * by 0 = 0, our final equation is: 
quantity <- filters[filterType == 'LOT_SIZE', minQty]

#Sometimes the value will be in scientific notation, so we convert it which creates a character string and 
# this is why the decimalplaces function accommodates numbers and character strings.  
t <- format(quantity, scientific = FALSE)
dec <- decimalplaces(t)
buy_quantity <- round(buy_quantity, digits = dec)


#### Get buy_quantity based on the minimum possible minNotional ####
# Get data
symbol <- "FXSBUSD"
avail_busd <- binance_balances()[asset == "BUSD", as.numeric(free)]
filters <- binance_filters(symbol)
(price <- binance_ticker_price(symbol)[, price])

# Compare prices (current vs first in queu to sell and current vs first in queu to buy)
(sell <- binance_depth(symbol)[["asks"]][1, price])
(buy <- binance_depth(symbol)[["bids"]][1, price])
(sell - price)/price
(price - buy)/price
# To ensure the order, make the price the first price in queu:
# buy match with ask, sell match with bid (it is crossed to ensure the match price)
# Si se va a comprar, buscar que la diferencia sea menor al 0.1 %
# entre el precio actual y el valor más próximo de precio de venta
if((sell - price)/price < 0.001){ 
  price <- sell
}else if((price - buy)/price < 0){
  price <- buy
}


# To get minimum buy_quantity, consider the price of the stop loss (instead of the current price) 
# in order the order can be allowed 
stop_loss <- 5.098


# Get minimum buy_quantity
buy_quantity * stop_loss >= filters[filterType == "MIN_NOTIONAL", minNotional]
buy_quantity >= filters[filterType == "MIN_NOTIONAL", minNotional] / stop_loss
(buy_quantity <- filters[filterType == "MIN_NOTIONAL", minNotional] / price)
(buy_quantity <- filters[filterType == "MIN_NOTIONAL", minNotional] / stop_loss)

# Get decimals allowed
quantity <- filters[filterType == 'LOT_SIZE', minQty]
# t <- format(quantity, scientific = FALSE)
# dec <- decimalplaces(t)
# buy_quantity <- round(buy_quantity, digits = dec)
buy_quantity <- plyr::round_any(buy_quantity, accuracy = quantity, f = ceiling)

# Test it
buy_quantity * stop_loss >= filters[filterType == "MIN_NOTIONAL", minNotional]
quot <- (buy_quantity - filters[filterType == 'LOT_SIZE', minQty]) / filters[filterType == 'LOT_SIZE', stepSize]
stopifnot(abs(quot - round(quot)) < 1e-10)

buy_quantity
price
stop_loss

binance_new_order(symbol = symbol, side = "BUY", type = "LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = buy_quantity, price = price, 
                  test = T
                  ) 
# binance_new_order(symbol = symbol, side = "SELL", type = "LIMIT",
#                   time_in_force = "GTC", # Good til cancelled
#                   quantity = buy_quantity, price = price,
#                   test = T
#                   )
binance_open_orders(symbol)
binance_query_order(symbol, order_id = 86466952) # Only works with real order_id
binance_cancel_order(symbol, order_id = 86466952)



# stop_loss <- 5.098
binance_new_order(symbol = symbol, side = "SELL", type = "STOP_LOSS_LIMIT", 
                  time_in_force = "GTC", # Good til cancelled
                  quantity = buy_quantity, price = stop_loss, 
                  stop_price = stop_loss,
                  test = F
                  ) -> orderSL
openOrders <- binance_open_orders(symbol)
openOrders[symbol == symbol & 
             price == stop_loss & 
             orig_qty == buy_quantity & 
             type == "STOP_LOSS_LIMIT" &
             stop_price == stop_loss]
binance_query_order(symbol, order_id = 86470026) # Only works with real order_id
binance_cancel_order(symbol, order_id = 86470026)


#### Get buy_quantity based on the minimum possible minNotional (functions) ####
symbol <- "BTCBUSD"

trades <- fread("~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/myTrades/myTrades.csv")
trades
# trades <- NULL

# If is the first time for the symbol
data.table(tradeId = 0,
           symbol = symbol,
           date = Sys.time(),
           side = "BUY",
           price = 0,
           quantity = 0,
           quote = 0,
           stopLoss = 0,
           idOrderBuy = 0,
           idOrderSL = 0,
           idOrderSell = 0,
           sell = 0,
           moneyEarned = 0,
           yield = 0,
           active = FALSE,
           test = TRUE,
           moneyCurrent = 10) -> trades


# Initial data
filters <- binance_filters(symbol)
stop_loss <- 28919
test <- T

buyTrade <- function(symbol, filters, stop_loss, test = T){
  # Get data
  avail_busd <- binance_balances()[asset == "BUSD", as.numeric(free)]
  price <- binance_ticker_price(symbol)[, price]
  
  # Compare prices (current vs first in queu to sell and current vs first in queu to buy)
  sell <- binance_depth(symbol)[["asks"]][1, price]
  buy <- binance_depth(symbol)[["bids"]][1, price]

  # To ensure the order, make the price the first price in queu:
  # buy match with ask, sell match with bid (it is crossed to ensure the match price)
  # Si se va a comprar, buscar que la diferencia sea menor al 0.1 %
  # entre el precio actual y el valor más próximo de precio de venta
  if((sell - price)/price < 0.001){ 
    price <- sell
  }else if((price - buy)/price < 0){
    price <- buy
  }
  
  # To get minimum buy_quantity, consider the price of the stop loss (instead of the current price) 
  # in order the order can be allowed 
  # Get minimum buy_quantity
  buy_quantity <- filters[filterType == "MIN_NOTIONAL", minNotional] / stop_loss
  
  # Get decimals allowed
  quantity <- filters[filterType == 'LOT_SIZE', minQty]
  buy_quantity <- plyr::round_any(buy_quantity, accuracy = quantity, f = ceiling)
  
  # If there aren't enough founds
  if(buy_quantity * price > avail_busd){
    test <- TRUE
  }
  
  # Orders
  binance_new_order(symbol = symbol, side = "BUY", type = "LIMIT", 
                    time_in_force = "GTC", # Good til cancelled
                    quantity = buy_quantity, price = price, 
                    test = test
                    ) -> orderBuy
  

  binance_new_order(symbol = symbol, side = "SELL", type = "STOP_LOSS_LIMIT", 
                    time_in_force = "GTC", # Good til cancelled
                    quantity = buy_quantity, price = stop_loss, 
                    stop_price = stop_loss,
                    test = test
                    ) -> orderSL
  
  # Test
  if(test){
    orderBuy <- data.table(symbol = symbol,
                           transact_time = Sys.time(),
                           side = "BUY",
                           price = price,
                           orig_qty = buy_quantity,
                           cummulative_quote_qty = price * buy_quantity,
                           order_id = 0
                           )
    orderSL2 <- data.table(stop_price = stop_loss,
                           order_id = 0)
  }else{
    # Getting stop loss data
    openOrders <- binance_open_orders(symbol)
    openOrders[symbol == symbol & 
                 price == stop_loss & 
                 orig_qty == buy_quantity & 
                 type == "STOP_LOSS_LIMIT" &
                 stop_price == stop_loss] -> orderSL2
  }
  
  results <- list(orderBuy = orderBuy,
                  orderSL2 = orderSL2,
                  test = test
                  )
  
  return(results)
}


tmp <- data.table(tradeId = trades[.N, tradeId] + 1,
                  symbol = results[["orderBuy"]][, symbol],
                  date = results[["orderBuy"]][, transact_time],
                  side = results[["orderBuy"]][, side],
                  price = results[["orderBuy"]][, price],
                  quantity = results[["orderBuy"]][, orig_qty],
                  quote = results[["orderBuy"]][, cummulative_quote_qty],
                  stopLoss = results[["orderSL2"]][, stop_price],
                  idOrderBuy = results[["orderBuy"]][, order_id],
                  idOrderSL = results[["orderSL2"]][, order_id],
                  idOrderSell = 0,
                  sell = 0,
                  moneyEarned = 0,
                  yield = 0,
                  active = TRUE,
                  test = results[["test"]],
                  moneyCurrent = trades[.N, moneyCurrent]
                  )
print(tmp)
trades <- rbind(trades,tmp)
# trades[, moneyCurrentLag := c(NA, moneyCurrent[-.N])]

fwrite(trades, "~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/myTrades/myTrades.csv")


# sell
(currentPrice <- binance_ticker_price(symbol)[, price])
# currentPrice <- 30290
sellTrades <- trades[currentPrice > price & active == TRUE]
if(sellTrades %>% nrow() > 0){
  numberTrades <- sellTrades %>% nrow()
  for(trade in 1:numberTrades){
    # Cancelling stop loss if it is not a TEST
    if(sellTrades[, test] == FALSE){
      binance_cancel_order(sellTrades[trade, symbol], 
                           order_id = sellTrades[trade, idOrderSL]
                           ) 
    }
    
    # Selling order
    binance_new_order(symbol = sellTrades[trade, symbol], 
                      side = "SELL", type = "LIMIT",
                      time_in_force = "GTC", # Good til cancelled
                      quantity = sellTrades[trade, quantity], 
                      price = currentPrice,
                      test = sellTrades[trade, test]
                      ) -> idOrderSell
    
    sellTrades[trade, 
               .(tradeId, symbol, date, side, price, quantity, quote, stopLoss, idOrderBuy, idOrderSL,
                 idOrderSell = ifelse(test == FALSE, idOrderSell[, order_id], 0),
                 sell = currentPrice,
                 moneyEarned = (currentPrice - price) * quantity,
                 yield = (currentPrice - price)/price,
                 active = FALSE,
                 test, 
                 moneyCurrent = ifelse(test == FALSE, 
                                       ifelse(moneyCurrent + (currentPrice - price) * quantity > 10,
                                              moneyCurrent + (currentPrice - price) * quantity,
                                              10), 
                                       moneyCurrent)
                 # moneyCurrentLag
                 )] -> sellTrades2
    
    trades <- rbind(trades[tradeId != sellTrades[trade, tradeId]], 
                    sellTrades2
                    )[order(tradeId)]
  }
}
# trades[, moneyCurrentLag := c(NA, moneyCurrent[-.N])]


# Stop Loss
trades[currentPrice < stopLoss & active == TRUE, 
       ":="(sell = stopLoss,
            moneyEarned = (stopLoss - price) * quantity,
            yield = (stopLoss - price)/price,
            active = FALSE,
            moneyCurrent = ifelse(test == FALSE, 
                                  ifelse(moneyCurrent + (stopLoss - price) * quantity > 10,
                                         moneyCurrent + (stopLoss - price) * quantity,
                                         10), 
                                  moneyCurrent)
            )]# -> trades

# trades[, moneyCurrentLag := c(NA, moneyCurrent[-.N])]



# Solo falta:
# - agregar comisiones
# - agregarlo al productivo
