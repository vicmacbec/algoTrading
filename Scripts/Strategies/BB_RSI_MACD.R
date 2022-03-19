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


#### Load data ####
print("#### Load data ####")
binance_coins()
binance_coins_prices() #%>% View
binance_symbols() %>% grep(pattern = "^BUSD|*BUSD", value = TRUE) -> BUSDpairs


#### Initial parameters ####
print("#### Initial parameters ####")


#### Strategy ####
print("#### Strategy ####")


#### Testing strategy ####
print("#### Testing strategy ####")


#### Plots ####
print("#### Plots ####")


#### Saving data ####
print("#### Saving data ####")


#### Tests ####

