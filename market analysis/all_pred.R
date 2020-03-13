
# local address for saving
if(Sys.info()[1]=="Windows"){
loc_add<-c("C:/Users/hahadydolatsara/OneDrive - Clark University/Research topics/blockchain/data/")}
if(Sys.info()[1]=="Darwin"){
loc_add<-c("/Users/hamid/OneDrive - Clark University/Research topics/blockchain/data/")}

source(paste0(loc_add,"blockchain_funcs.R"))
# creating a dataset that records all the info
data<-list()

end_date<-Sys.Date()
end_date<-c("2019-12-28")
anal_start<-c("2017-08-02")

##================================================================================================
suppressMessages(getSymbols("^DJI", from = "2007-01-01", to =  end_date))

#calculate the return
#https://www.r-bloggers.com/dow-jones-stock-market-index-1-4-log-returns-exploratory-analysis/
#https://www.codecogs.com/latex/eqneditor.php
# R_{t}\ :=\ \frac{P_{t}}{P_{t-1}}\ \ -\ 1
# r_{t}\ :=\ ln\frac{P_{t}}{P_{t-1}}\ =\ ln(1+R_{t})
# 

data$dji<-as.data.frame(DJI)
data$dji$date<-rownames(data$dji)

data$dji$dji_open_diff<-c(NA,diff(data$dji$DJI.Open)) 
data$dji$dji_close_diff<-c(NA,diff(data$dji$DJI.Close))
data$dji$dji_open_rsi <- RSI(data$dji$DJI.Open)
data$dji$dji_close_rsi <- RSI(data$dji$DJI.Close)

data$data_all<-data$dji

write.csv(data$dji,paste0(loc_add,"dji.csv"))
getSymbols("GOLD",src="yahoo", from="2016-01-01",to=end_date)
data$gold<-as.data.frame(`GOLD`)
names(data$gold)<-c("gold_Open", "gold_High", "gold_Low","gold_Close","gold_Volume","gold_Adjusted")
data$gold$date<-rownames(data$gold)
data$gold$gold_open_diff<-c(NA,diff(data$gold$gold_Open)) 
data$gold$gold_close_diff<-c(NA,diff(data$gold$gold_Close)) 
data$gold$gold_open_rsi <- RSI(data$gold$gold_Open)
data$gold$gold_close_rsi <- RSI(data$gold$gold_Close)
data$gold$gold_open_move<-func_move(data$gold$gold_open_diff)
data$gold$gold_close_move<-func_move(data$gold$gold_close_diff)

write.csv(data$gold,paste0(loc_add,"gold.csv"))

data$data_all<-merge(data$data_all,data$gold, by="date", all = TRUE)
stock<-"AAPL"
# stock<-"AMZN"
getSymbols(stock,src="yahoo", from="2016-01-01",to=end_date)
data$stock<-as.data.frame(eval(as.name(stock)))
names(data$stock)<-c("stock_Open", "stock_High", "stock_Low","stock_Close","stock_Volume","stock_Adjusted")
data$stock$date<-rownames(data$stock)

data$stock$stock_open_diff<-c(NA,diff(data$stock$stock_Open))
data$stock$stock_close_diff<-c(NA,diff(data$stock$stock_Close))
data$stock$stock_open_RSI<-RSI(data$stock$stock_Open)
data$stock$stock_close_RSI<-RSI(data$stock$stock_Close)
data$stock$stock_open_move<-func_move(data$stock$stock_open_diff)
data$stock$stock_close_move<-func_move(data$stock$stock_close_diff)


write.csv(data$stock,paste0(loc_add,"stock_stock.csv"))
data$data_all<-merge(data$data_all,data$stock, by="date", all = TRUE)
 
##============================================silver price========================================
##================================================================================================

# iShares Silver Trust (SLV) - NYSEArca - NYSEArca Delayed Price
getSymbols("SLV",src="yahoo", from="2016-01-01",to=end_date)
data$silver<-as.data.frame(`SLV`)
names(data$silver)<-c("silver_Open", "silver_High", "silver_Low","silver_Close","silver_Volume","silver_Adjusted")

data$silver$date<-rownames(data$silver)
data$silver$silver_open_diff<-c(NA,diff(data$silver$silver_Open)) 
data$silver$silver_close_diff<-c(NA,diff(data$silver$silver_Close)) 
data$silver$silver_open_rsi <- RSI(data$silver$silver_Open)
data$silver$silver_close_rsi <- RSI(data$silver$silver_Close)
data$silver$silver_open_move<-func_move(data$silver$silver_open_diff)
data$silver$silver_close_move<-func_move(data$silver$silver_close_diff)


# write.csv(data$gold_xauusd,paste0(loc_add,"gold_xauusd.csv"))
write.csv(data$silver,paste0(loc_add,"silver.csv"))

data$data_all<-merge(data$data_all,data$silver, by="date", all = TRUE)

# Federal Reserve Economic Data , Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
Quandl.api_key("S8ZBsDHHQ6pwPerVvvsr")

data$oil <- Quandl("FRED/DCOILWTICO", 
                    type = "raw", 
                    collapse = "daily",  
                    start_date = "2016-01-01", 
                   end_date = end_date)

names(data$oil)<-c("date","oil")
data$oil$date<-as.character(data$oil$date)

data$oil$oil_diff<-c(NA,diff(data$oil$oil)) 
data$oil$oil_rsi <- RSI(data$oil$oil)
data$oil$oil_move<-func_move(data$oil$oil_diff)

write.csv(data$oil,paste0(loc_add,"oil_raw.csv"))
data$data_all<-merge(data$data_all,data$oil, by="date", all = TRUE)

terms <- c("Bitcoin","Ethereum","Litecoin","Tether_(cryptocurrency)","Bitcoin_Cash","Cryptocurrency")

data$wiki<-list()


data$wiki$btc <- as.data.frame(wp_trend(page = "Bitcoin",
                             from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                             to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y")))

data$wiki$btc<-data$wiki$btc[c("date", "views")]
names(data$wiki$btc)<-c("date", "views_btc")
data$wiki$btc$date<-as.character(data$wiki$btc$date)

data$wiki$btc$btc_sma3<-SMA(data$wiki$btc$views,3)
data$wiki$btc$btc_sma3_diff<-c(NA,diff(data$wiki$btc$btc_sma3))
data$wiki$btc$btc_sma3_move<-func_move(data$wiki$btc$btc_sma3_diff)
data$wiki$btc$btc_ema5<-EMA(data$wiki$btc$views,5)
data$wiki$btc$btc_ema5_diff<-c(NA,diff(data$wiki$btc$btc_ema5))
data$wiki$btc$btc_ema5_move<-func_move(data$wiki$btc$btc_ema5_diff)
data$wiki$btc$btc_RSI<-RSI(data$wiki$btc$views_btc)
#new
data$wiki$btc$views_btc_diff<-c(NA,diff(data$wiki$btc$views_btc)) 
data$wiki$btc$views_btc_move<-func_move(data$wiki$btc$views_btc_diff)
#new

data$wiki$eth <- as.data.frame(wp_trend(page = "Ethereum",
                                        from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                                        to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y")))
data$wiki$eth<-data$wiki$eth[c("date", "views")]
names(data$wiki$eth)<-c("date", "views_eth")
data$wiki$eth$date<-as.character(data$wiki$eth$date)

data$wiki$eth$eth_sma3<-SMA(data$wiki$eth$views,3)
data$wiki$eth$eth_sma3_diff<-c(NA,diff(data$wiki$eth$eth_sma3))
data$wiki$eth$eth_sma3_move<-func_move(data$wiki$eth$eth_sma3_diff)
data$wiki$eth$eth_ema5<-EMA(data$wiki$eth$views,5)
data$wiki$eth$eth_ema5_diff<-c(NA,diff(data$wiki$eth$eth_ema5))
data$wiki$eth$eth_ema5_move<-func_move(data$wiki$eth$eth_ema5_diff)
data$wiki$eth$eth_RSI<-RSI(data$wiki$eth$views_eth)
#new
data$wiki$eth$views_eth_diff<-c(NA,diff(data$wiki$eth$views_eth)) 
data$wiki$eth$views_eth_move<-func_move(data$wiki$eth$views_eth_diff)
#new

data$wiki$ltc <- as.data.frame(wp_trend(page = "Litecoin",
                                        from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                                        to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y")))  
data$wiki$ltc<-data$wiki$ltc[c("date", "views")]
names(data$wiki$ltc)<-c("date", "views_ltc")
data$wiki$ltc$date<-as.character(data$wiki$ltc$date)

data$wiki$ltc$ltc_sma3<-SMA(data$wiki$ltc$views,3)
data$wiki$ltc$ltc_sma3_diff<-c(NA,diff(data$wiki$ltc$ltc_sma3))
data$wiki$ltc$ltc_sma3_move<-func_move(data$wiki$ltc$ltc_sma3_diff)
data$wiki$ltc$ltc_ema5<-EMA(data$wiki$ltc$views,5)
data$wiki$ltc$ltc_ema5_diff<-c(NA,diff(data$wiki$ltc$ltc_ema5))
data$wiki$ltc$ltc_ema5_move<-func_move(data$wiki$ltc$ltc_ema5_diff)
data$wiki$ltc$ltc_RSI<-RSI(data$wiki$ltc$views_ltc)
#new
data$wiki$ltc$views_ltc_diff<-c(NA,diff(data$wiki$ltc$views_ltc)) 
data$wiki$ltc$views_ltc_move<-func_move(data$wiki$ltc$views_ltc_diff)
#new

data$wiki$usdt <- as.data.frame(wp_trend(page = "Tether_(cryptocurrency)",
                                        from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                                        to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y")))
data$wiki$usdt<-data$wiki$usdt[c("date", "views")]
names(data$wiki$usdt)<-c("date", "views_usdt")
data$wiki$usdt$date<-as.character(data$wiki$usdt$date)

data$wiki$usdt$usdt_sma3<-SMA(data$wiki$usdt$views,3)
data$wiki$usdt$usdt_sma3_diff<-c(NA,diff(data$wiki$usdt$usdt_sma3))
data$wiki$usdt$usdt_sma3_move<-func_move(data$wiki$usdt$usdt_sma3_diff)
data$wiki$usdt$usdt_ema5<-EMA(data$wiki$usdt$views,5)
data$wiki$usdt$usdt_ema5_diff<-c(NA,diff(data$wiki$usdt$usdt_ema5))
data$wiki$usdt$usdt_ema5_move<-func_move(data$wiki$usdt$usdt_ema5_diff)
data$wiki$usdt$usdt_RSI<-RSI(data$wiki$usdt$views_usdt)
#new
data$wiki$usdt$views_usdt_diff<-c(NA,diff(data$wiki$usdt$views_usdt)) 
data$wiki$usdt$views_usdt_move<-func_move(data$wiki$usdt$views_usdt_diff)
#new

data$wiki$bch <- as.data.frame(wp_trend(page = "Bitcoin_Cash",
                                        from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                                        to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y"))) 
data$wiki$bch<-data$wiki$bch[c("date", "views")]
names(data$wiki$bch)<-c("date", "views_bch")
data$wiki$bch$date<-as.character(data$wiki$bch$date)

data$wiki$bch$bch_sma3<-SMA(data$wiki$bch$views,3)
data$wiki$bch$bch_sma3_diff<-c(NA,diff(data$wiki$bch$bch_sma3))
data$wiki$bch$bch_sma3_move<-func_move(data$wiki$bch$bch_sma3_diff)
data$wiki$bch$bch_ema5<-EMA(data$wiki$bch$views,5)
data$wiki$bch$bch_ema5_diff<-c(NA,diff(data$wiki$bch$bch_ema5))
data$wiki$bch$bch_ema5_move<-func_move(data$wiki$bch$bch_ema5_diff)
data$wiki$bch$bch_RSI<-RSI(data$wiki$bch$views_bch)
#new
data$wiki$bch$views_bch_diff<-c(NA,diff(data$wiki$bch$views_bch)) 
data$wiki$bch$views_bch_move<-func_move(data$wiki$bch$views_bch_diff)
#new

data$wiki$crypto <- as.data.frame(wp_trend(page = "Cryptocurrency",
                                        from = strptime(as.character("1/1/2016"), "%m/%d/%Y"),
                                        to= strptime(format(as.Date(end_date)-1, "%m/ %d/20%y"), "%m/%d/%Y")))
data$wiki$crypto<-data$wiki$crypto[c("date", "views")]
names(data$wiki$crypto)<-c("date", "views_crypto")
data$wiki$crypto$date<-as.character(data$wiki$crypto$date)

data$wiki$crypto$crypto_sma3<-SMA(data$wiki$crypto$views,3)
data$wiki$crypto$crypto_sma3_diff<-c(NA,diff(data$wiki$crypto$crypto_sma3))
data$wiki$crypto$crypto_sma3_move<-func_move(data$wiki$crypto$crypto_sma3_diff)
data$wiki$crypto$crypto_ema5<-EMA(data$wiki$crypto$views,5)
data$wiki$crypto$crypto_ema5_diff<-c(NA,diff(data$wiki$crypto$crypto_ema5))
data$wiki$crypto$crypto_ema5_move<-func_move(data$wiki$crypto$crypto_ema5_diff)
data$wiki$crypto$crypto_RSI<-RSI(data$wiki$crypto$views_crypto)
#new
data$wiki$crypto$views_crypto_diff<-c(NA,diff(data$wiki$crypto$views_crypto)) 
data$wiki$crypto$views_crypto_move<-func_move(data$wiki$crypto$views_crypto_diff)
#new

write.csv(data$wiki$btc,paste0(loc_add,"wiki_btc.csv"))
write.csv(data$wiki$eth,paste0(loc_add,"wiki_eth.csv"))
write.csv(data$wiki$ltc,paste0(loc_add,"wiki_ltc.csv"))
write.csv(data$wiki$usdt,paste0(loc_add,"wiki_usdt.csv"))
write.csv(data$wiki$bch,paste0(loc_add,"wiki_bch.csv"))
write.csv(data$wiki$crypto,paste0(loc_add,"wiki_crypto.csv"))

data$data_all<-merge(data$data_all,data$wiki$btc, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$wiki$eth, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$wiki$ltc, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$wiki$usdt, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$wiki$bch, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$wiki$crypto, by="date", all = TRUE)
nrow(data$data_all)
##############################################wiki trend##########################################
##################################################################################################

##============================================Crypto Currencies===================================
##================================================================================================

# 5 top traded cryptocurrencies:
# https://www.bloomberg.com/news/articles/2019-10-01/tether-not-bitcoin-likely-the-world-s-most-used-cryptocurrency

# it has data of all crypto currencies
crypto<-list()
crypto$crypto5 <-c("BTC-USD","ETH-USD","LTC-USD","USDT-USD","BCH-USD")
crypto$names<-c("Bitcoin_Price","Ethereum_Price","Litecoin_Price","Tether_Price","BitcoinCash_Price")
getSymbols(crypto$crypto5,src="yahoo", from="2016-01-01",to=as.Date(end_date)-1)

crypto$price<-list()
crypto$price_xts<-list()

for( i in 1:5){
  name_crypto<-crypto$crypto5[i]
  j<-str_replace(name_crypto, "-", "_")
  j<-tolower(j)
  crypto$price[[j]]<-as.data.frame(get(name_crypto))
  # convert to xts format and in 1000 (k) unit for visulization
  crypto$price_xts[[crypto$names[i]]]<-as.xts(crypto$price[[j]]/1000)
  rm(list = crypto$crypto5[i])
}
names(crypto$price_xts)

lapply(crypto$price_xts, function (x) storage.mode(x) <- "numeric")

Bitcoin_Price<-crypto$price_xts$Bitcoin_Price
Ethereum_Price<-crypto$price_xts$Ethereum_Price
Litecoin_Price<-crypto$price_xts$Litecoin_Price
Tether_Price<-crypto$price_xts$Tether_Price
BitcoinCash_Price<-crypto$price_xts$BitcoinCash_Price


chartSeries(`Bitcoin_Price`,log.scale=TRUE,theme='white.mono',bar.type='hlc')
chartSeries(`Ethereum_Price`,log.scale=TRUE,theme='white.mono',bar.type='hlc')
chartSeries(`Litecoin_Price`,log.scale=TRUE,theme='white.mono',bar.type='hlc')
chartSeries(`Tether_Price`,log.scale=TRUE,theme='white.mono',bar.type='hlc')
chartSeries(`BitcoinCash_Price`,log.scale=TRUE,theme='white.mono',bar.type='hlc')


crypto$price$btc_usd$date<-row.names(crypto$price$btc_usd)
names(crypto$price$btc_usd)<-c("BTC_USD_Open","BTC_USD_High","BTC_USD_Low","BTC_USD_Close",
                                    "BTC_USD_Volume","BTC_USD_Adjusted","date")
crypto$price$btc_usd$btc_usd_open_rsi <- RSI(crypto$price$btc_usd$BTC_USD_Open)  
crypto$price$btc_usd$btc_usd_close_rsi <- RSI(crypto$price$btc_usd$BTC_USD_Close)
crypto$price$btc_usd$btc_close_rsi_diff <- c(NA,diff(crypto$price$btc_usd$btc_usd_close_rsi)) 
crypto$price$btc_usd$btc_close_rsi_move<-func_move(crypto$price$btc_usd$btc_close_rsi_diff)
crypto$price$btc_usd$btc_open_rsi_diff <- c(NA,diff(crypto$price$btc_usd$btc_usd_open_rsi))   
crypto$price$btc_usd$btc_open_rsi_move<-func_move(crypto$price$btc_usd$btc_open_rsi_diff)
#new
crypto$price$btc_usd$btc_open_diff <- c(NA,diff(crypto$price$btc_usd$BTC_USD_Open))
crypto$price$btc_usd$btc_close_diff <- c(NA,diff(crypto$price$btc_usd$BTC_USD_Close))
#new
crypto$price$btc_usd$btc_open_move <- func_move(c(NA,diff(crypto$price$btc_usd$BTC_USD_Open)))
crypto$price$btc_usd$btc_close_move <- func_move(c(NA,diff(crypto$price$btc_usd$BTC_USD_Close)))


crypto$price$eth_usd$date<-row.names(crypto$price$eth_usd)
names(crypto$price$eth_usd)<-c("ETH_USD_Open","ETH_USD_High","ETH_USD_Low","ETH_USD_Close",
                               "ETH_USD_Volume","ETH_USD_Adjusted","date")
crypto$price$eth_usd$eth_usd_open_rsi <- RSI(crypto$price$eth_usd$ETH_USD_Open)  
crypto$price$eth_usd$eth_usd_close_rsi <- RSI(crypto$price$eth_usd$ETH_USD_Close)
crypto$price$eth_usd$eth_close_rsi_diff <- c(NA,diff(crypto$price$eth_usd$eth_usd_close_rsi)) 
crypto$price$eth_usd$eth_close_rsi_move<-func_move(crypto$price$eth_usd$eth_close_rsi_diff)
crypto$price$eth_usd$eth_open_rsi_diff <- c(NA,diff(crypto$price$eth_usd$eth_usd_open_rsi))   
crypto$price$eth_usd$eth_open_rsi_move<-func_move(crypto$price$eth_usd$eth_open_rsi_diff)
#new
crypto$price$eth_usd$eth_open_diff <- c(NA,diff(crypto$price$eth_usd$ETH_USD_Open))
crypto$price$eth_usd$eth_close_diff <- c(NA,diff(crypto$price$eth_usd$ETH_USD_Close))
#new
crypto$price$eth_usd$eth_open_move <- func_move(c(NA,diff(crypto$price$eth_usd$ETH_USD_Open)))
crypto$price$eth_usd$eth_close_move <- func_move(c(NA,diff(crypto$price$eth_usd$ETH_USD_Close)))


crypto$price$ltc_usd$date<-row.names(crypto$price$ltc_usd)
names(crypto$price$ltc_usd)<-c("LTC_USD_Open","LTC_USD_High","LTC_USD_Low","LTC_USD_Close",
                               "LTC_USD_Volume","LTC_USD_Adjusted","date")
crypto$price$ltc_usd$ltc_usd_open_rsi <- RSI(crypto$price$ltc_usd$LTC_USD_Open)  
crypto$price$ltc_usd$ltc_usd_close_rsi <- RSI(crypto$price$ltc_usd$LTC_USD_Close)
crypto$price$ltc_usd$ltc_close_rsi_diff <- c(NA,diff(crypto$price$ltc_usd$ltc_usd_close_rsi)) 
crypto$price$ltc_usd$ltc_close_rsi_move<-func_move(crypto$price$ltc_usd$ltc_close_rsi_diff)
crypto$price$ltc_usd$ltc_open_rsi_diff <- c(NA,diff(crypto$price$ltc_usd$ltc_usd_open_rsi))   
crypto$price$ltc_usd$ltc_open_rsi_move<-func_move(crypto$price$ltc_usd$ltc_open_rsi_diff)
#new
crypto$price$ltc_usd$ltc_open_diff <- c(NA,diff(crypto$price$ltc_usd$LTC_USD_Open))
crypto$price$ltc_usd$ltc_close_diff <- c(NA,diff(crypto$price$ltc_usd$LTC_USD_Close))
#new
crypto$price$ltc_usd$ltc_open_move <- func_move(c(NA,diff(crypto$price$ltc_usd$LTC_USD_Open)))
crypto$price$ltc_usd$ltc_close_move <- func_move(c(NA,diff(crypto$price$ltc_usd$LTC_USD_Close)))


crypto$price$usdt_usd$date<-row.names(crypto$price$usdt_usd)
names(crypto$price$usdt_usd)<-c("USDT_USD_Open","USDT_USD_High","USDT_USD_Low","USDT_USD_Close",
                               "USDT_USD_Volume","USDT_USD_Adjusted","date")
crypto$price$usdt_usd$usdt_usd_open_rsi <- RSI(crypto$price$usdt_usd$USDT_USD_Open)  
crypto$price$usdt_usd$usdt_usd_close_rsi <- RSI(crypto$price$usdt_usd$USDT_USD_Close)
crypto$price$usdt_usd$usdt_close_rsi_diff <- c(NA,diff(crypto$price$usdt_usd$usdt_usd_close_rsi)) 
crypto$price$usdt_usd$usdt_close_rsi_move<-func_move(crypto$price$usdt_usd$usdt_close_rsi_diff)
crypto$price$usdt_usd$usdt_open_rsi_diff <- c(NA,diff(crypto$price$usdt_usd$usdt_usd_open_rsi))   
crypto$price$usdt_usd$usdt_open_rsi_move<-func_move(crypto$price$usdt_usd$usdt_open_rsi_diff)
#new
crypto$price$usdt_usd$usdt_open_diff <- c(NA,diff(crypto$price$usdt_usd$USDT_USD_Open))
crypto$price$usdt_usd$usdt_close_diff <- c(NA,diff(crypto$price$usdt_usd$USDT_USD_Close))
#new
crypto$price$usdt_usd$usdt_open_move <- func_move(c(NA,diff(crypto$price$usdt_usd$USDT_USD_Open)))
crypto$price$usdt_usd$usdt_close_move <- func_move(c(NA,diff(crypto$price$usdt_usd$USDT_USD_Close)))


crypto$price$bch_usd$date<-row.names(crypto$price$bch_usd)
names(crypto$price$bch_usd)<-c("BCH_USD_Open","BCH_USD_High","BCH_USD_Low","BCH_USD_Close",
                                "BCH_USD_Volume","BCH_USD_Adjusted","date")
crypto$price$bch_usd$bch_usd_open_rsi <- RSI(crypto$price$bch_usd$BCH_USD_Open)  
crypto$price$bch_usd$bch_usd_close_rsi <- RSI(crypto$price$bch_usd$BCH_USD_Close)
crypto$price$bch_usd$bch_close_rsi_diff <- c(NA,diff(crypto$price$bch_usd$bch_usd_close_rsi)) 
crypto$price$bch_usd$bch_close_rsi_move<-func_move(crypto$price$bch_usd$bch_close_rsi_diff)
crypto$price$bch_usd$bch_open_rsi_diff <- c(NA,diff(crypto$price$bch_usd$bch_usd_open_rsi))   
crypto$price$bch_usd$bch_open_rsi_move<-func_move(crypto$price$bch_usd$bch_open_rsi_diff)
#new
crypto$price$bch_usd$bch_open_diff <- c(NA,diff(crypto$price$bch_usd$BCH_USD_Open))
crypto$price$bch_usd$bch_close_diff <- c(NA,diff(crypto$price$bch_usd$BCH_USD_Close))
#new
crypto$price$bch_usd$bch_open_move <- func_move(c(NA,diff(crypto$price$bch_usd$BCH_USD_Open)))
crypto$price$bch_usd$bch_close_move <- func_move(c(NA,diff(crypto$price$bch_usd$BCH_USD_Close)))
temp<-crypto$price$bch_usd

data$crypto<-crypto
rm(list = c("crypto","DJI","SLV","GOLD"))

write.csv(data$crypto$price$btc_usd,paste0(loc_add,"price_btc_usd.csv"))
write.csv(data$crypto$price$eth_usd,paste0(loc_add,"price_eth_usd.csv"))
write.csv(data$crypto$price$ltc_usd,paste0(loc_add,"price_ltc_usd.csv"))
write.csv(data$crypto$price$usdt_usd,paste0(loc_add,"price_usdt_usd.csv"))
write.csv(data$crypto$price$bch_usd,paste0(loc_add,"price_bch_usd.csv"))

data$data_all<-merge(data$data_all,data$crypto$price$btc_usd, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$crypto$price$eth_usd, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$crypto$price$ltc_usd, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$crypto$price$usdt_usd, by="date", all = TRUE)
data$data_all<-merge(data$data_all,data$crypto$price$bch_usd, by="date", all = TRUE)


##############################################Crypto Currencies###################################
##################################################################################################

# # local address for loading
# if(Sys.info()[1]=="Windows"){
#   saved<-readRDS("C:/Users/hahadydolatsara/OneDrive - Clark University/Research topics/blockchain/binweng/oneday/goodones/all_perf-AAPL12-29glmnone.rds")}
# if(Sys.info()[1]=="Darwin"){
#   saved<-readRDS("/Users/hamid/OneDrive - Clark University/Research topics/blockchain/binweng/oneday/goodones/all_perf-AAPL12-29glmnone.rds")}
# 
# data<-saved$data


# stock stock as a target could be found in here:
# data$stock$stock_open_move   and   data$stock$stock_close_move

# here I try to predict stock stock using other information


data201781<-data$data_all[which(data$data_all$date==anal_start):nrow(data$data_all),]


data201781$dateorder<-1:nrow(data201781)
data201781$dateweek<-wday(ymd(data201781$date, tz = "America/New_York"), label=TRUE)
data201781 <- encode_cat(data201781,"dateweek","factor")


stock201781<-data201781[complete.cases(data201781$stock_open_move), ]
gold201781<-data201781[complete.cases(data201781$gold_open_move), ]
silver201781<-data201781[complete.cases(data201781$silver_open_move), ]
oil201781<-data201781[complete.cases(data201781$oil_move), ]
btc201781<-data201781[complete.cases(data201781$btc_open_move), ]
eth201781<-data201781[complete.cases(data201781$eth_open_move), ]
ltc201781<-data201781[complete.cases(data201781$ltc_open_move), ]
usdt201781<-data201781[complete.cases(data201781$usdt_open_move), ]
bch201781<-data201781[complete.cases(data201781$bch_open_move), ]


# I shift the dependent variable for a row up because it's a future event to predict
stock201781$stock_open_move<-c(stock201781$stock_open_move[2:(length(stock201781$stock_open_move))],NA)
gold201781$gold_open_move<-c(gold201781$gold_open_move[2:(length(gold201781$gold_open_move))],NA)
silver201781$gold_open_move<-c(silver201781$silver_open_move[2:(length(silver201781$silver_open_move))],NA)
oil201781$oil_move<-c(oil201781$oil_move[2:(length(oil201781$oil_move))],NA)
btc201781$btc_open_move<-c(btc201781$btc_open_move[2:(length(btc201781$btc_open_move))],NA)
eth201781$eth_open_move<-c(eth201781$eth_open_move[2:(length(eth201781$eth_open_move))],NA)
ltc201781$ltc_open_move<-c(ltc201781$ltc_open_move[2:(length(ltc201781$ltc_open_move))],NA)
usdt201781$usdt_open_move<-c(usdt201781$usdt_open_move[2:(length(usdt201781$usdt_open_move))],NA)
bch201781$bch_open_move<-c(bch201781$bch_open_move[2:(length(bch201781$bch_open_move))],NA)

stock201781<-stock201781[complete.cases(stock201781$stock_open_move), ]
gold201781<-gold201781[complete.cases(gold201781$gold_open_move), ]
silver201781<-silver201781[complete.cases(silver201781$silver_open_move), ]
oil201781<-oil201781[complete.cases(oil201781$oil_move), ]
btc201781<-btc201781[complete.cases(btc201781$btc_open_move), ]
eth201781<-eth201781[complete.cases(eth201781$eth_open_move), ]
ltc201781<-ltc201781[complete.cases(ltc201781$ltc_open_move), ]
usdt201781<-usdt201781[complete.cases(usdt201781$usdt_open_move), ]
bch201781<-bch201781[complete.cases(bch201781$bch_open_move), ]




# interpolating
stock201781=ts_interpolate(stock201781,method="nocb")  
gold201781=ts_interpolate(gold201781,method="nocb") 
silver201781=ts_interpolate(silver201781,method="nocb") 
oil201781=ts_interpolate(oil201781,method="nocb") 
btc201781=ts_interpolate(btc201781,method="nocb") 
eth201781=ts_interpolate(eth201781,method="nocb") 
ltc201781=ts_interpolate(ltc201781,method="nocb") 
usdt201781=ts_interpolate(usdt201781,method="nocb") 
bch201781=ts_interpolate(bch201781,method="nocb") 

#$$$$$$$$$$$$$$$$$$$$$$$

data$var_num<-c("dji_open_diff","dji_close_diff","dji_open_rsi","dji_close_rsi","gold_open_diff","gold_close_diff",
"gold_open_rsi","gold_close_rsi","stock_open_diff","stock_close_diff","stock_open_RSI","stock_close_RSI",
"silver_open_diff","silver_close_diff","silver_open_rsi","silver_close_rsi","oil_diff","oil_rsi",
"btc_sma3","btc_sma3_diff","btc_ema5","btc_ema5_diff","btc_RSI","eth_sma3","eth_sma3_diff","eth_ema5","eth_ema5_diff",
"eth_RSI","ltc_sma3","ltc_sma3_diff","ltc_ema5","ltc_ema5_diff","ltc_RSI","usdt_sma3","usdt_sma3_diff",
"usdt_ema5","usdt_ema5_diff","usdt_RSI","bch_sma3","bch_sma3_diff","bch_ema5","bch_ema5_diff",
"bch_RSI","crypto_sma3","crypto_sma3_diff","crypto_ema5","crypto_ema5_diff","crypto_RSI",
"views_btc_diff","views_eth_diff","views_ltc_diff","views_usdt_diff","views_bch_diff",
"views_crypto_diff","btc_usd_open_rsi","btc_usd_close_rsi","btc_close_rsi_diff","btc_open_rsi_diff",
"btc_open_diff","btc_close_diff","eth_usd_open_rsi","eth_usd_close_rsi","eth_close_rsi_diff","eth_open_rsi_diff",
"eth_open_diff","eth_close_diff","ltc_usd_open_rsi","ltc_usd_close_rsi","ltc_close_rsi_diff","ltc_open_rsi_diff",
"ltc_open_diff","ltc_close_diff","usdt_usd_open_rsi","usdt_usd_close_rsi","usdt_close_rsi_diff","usdt_open_rsi_diff",
"usdt_open_diff","usdt_close_diff","bch_usd_open_rsi","bch_usd_close_rsi","bch_close_rsi_diff","bch_open_rsi_diff",
"bch_open_diff","bch_close_diff",
"DJI.Open" ,"DJI.High", "DJI.Low",  "DJI.Close","DJI.Volume","DJI.Adjusted" , "gold_Open" , "gold_High","gold_Low",    
"gold_Close" ,"gold_Volume", "gold_Adjusted" , "stock_Open", "stock_High",   "stock_Low" , "stock_Close", 
"stock_Volume" , "stock_Adjusted","silver_Open",  "silver_High" , "silver_Low","silver_Close","silver_Volume",  
"silver_Adjusted"  ,"oil" , "views_btc" , "views_eth" ,"views_ltc","views_usdt",       
"views_bch" , "views_crypto", "BTC_USD_Open", "BTC_USD_High", "BTC_USD_Low" ,     
"BTC_USD_Close", "BTC_USD_Volume" , "BTC_USD_Adjusted",  "ETH_USD_Open","ETH_USD_High" ,    
"ETH_USD_Low" , "ETH_USD_Close" , "ETH_USD_Volume","ETH_USD_Adjusted", "LTC_USD_Open", 
"LTC_USD_High", "LTC_USD_Low", "LTC_USD_Close" ,"LTC_USD_Volume" ,  "LTC_USD_Adjusted" ,
"USDT_USD_Open", "USDT_USD_High", "USDT_USD_Low" , "USDT_USD_Close" , "USDT_USD_Volume"  ,
"USDT_USD_Adjusted", "BCH_USD_Open" , "BCH_USD_High" , "BCH_USD_Low" ,  "BCH_USD_Close",
"BCH_USD_Volume","BCH_USD_Adjusted","dateorder" )

data$var_cat<-c("gold_close_move",
"silver_close_move","btc_sma3_move","btc_ema5_move",
"eth_sma3_move","eth_ema5_move","ltc_sma3_move","ltc_ema5_move","usdt_sma3_move","usdt_ema5_move","bch_sma3_move",
"bch_ema5_move","crypto_sma3_move","crypto_ema5_move","views_btc_move","views_eth_move","views_ltc_move",
"views_usdt_move","views_bch_move","views_crypto_move","btc_close_rsi_move","btc_open_rsi_move",
"btc_close_move","eth_close_rsi_move","eth_open_rsi_move",
"eth_close_move","ltc_close_rsi_move","ltc_open_rsi_move","ltc_close_move",
"usdt_close_rsi_move","usdt_open_rsi_move","usdt_close_move","bch_close_rsi_move",
"bch_open_rsi_move","bch_close_move", "dateweek_Mon","dateweek_Tue","dateweek_Wed","dateweek_Thu","dateweek_Fri",
"stock_close_move")

data$dep_var<-c("bch_open_move" , "usdt_open_move" , "ltc_open_move" , "eth_open_move" , "btc_open_move" ,
                "oil_move" ,"silver_open_move" , "gold_open_move" , "stock_open_move")

stock201781<-encode_cat(stock201781,data$var_cat,"factor")
stock201781<-encode_cat(stock201781,data$dep_var[!data$dep_var %in% c("stock_open_move")],"factor")
gold201781<-encode_cat(gold201781,data$var_cat,"factor")
gold201781<-encode_cat(gold201781,data$dep_var[!data$dep_var %in% c("gold_open_move")],"factor")
silver201781<-encode_cat(silver201781,data$var_cat,"factor")
silver201781<-encode_cat(silver201781,data$dep_var[!data$dep_var %in% c("silver_open_move")],"factor")
oil201781<-encode_cat(oil201781,data$var_cat,"factor")
oil201781<-encode_cat(oil201781,data$dep_var[!data$dep_var %in% c("oil_move")],"factor")
btc201781<-encode_cat(btc201781,data$var_cat,"factor")
btc201781<-encode_cat(btc201781,data$dep_var[!data$dep_var %in% c("btc_open_move")],"factor")
eth201781<-encode_cat(eth201781,data$var_cat,"factor")
eth201781<-encode_cat(eth201781,data$dep_var[!data$dep_var %in% c("eth_open_move")],"factor")
ltc201781<-encode_cat(ltc201781,data$var_cat,"factor")
ltc201781<-encode_cat(ltc201781,data$dep_var[!data$dep_var %in% c("ltc_open_move")],"factor")
usdt201781<-encode_cat(usdt201781,data$var_cat,"factor")
usdt201781<-encode_cat(usdt201781,data$dep_var[!data$dep_var %in% c("usdt_open_move")],"factor")
bch201781<-encode_cat(bch201781,data$var_cat,"factor")
bch201781<-encode_cat(bch201781,data$dep_var[!data$dep_var %in% c("bch_open_move")],"factor")


stock201781<-encode_cat(stock201781,data$var_num,"numeric")
gold201781<-encode_cat(gold201781,data$var_num,"numeric")
silver201781<-encode_cat(silver201781,data$var_num,"numeric")
oil201781<-encode_cat(oil201781,data$var_num,"numeric")
btc201781<-encode_cat(btc201781,data$var_num,"numeric")
eth201781<-encode_cat(eth201781,data$var_num,"numeric")
ltc201781<-encode_cat(ltc201781,data$var_num,"numeric")
usdt201781<-encode_cat(usdt201781,data$var_num,"numeric")
bch201781<-encode_cat(bch201781,data$var_num,"numeric")

# identifying training and testing sets


set.seed(110)
data$stock201781<-list()
data$gold201781<-list()
data$silver201781<-list()
data$oil201781<-list()
data$btc201781<-list()
data$eth201781<-list()
data$ltc201781<-list()
data$usdt201781<-list()
data$bch201781<-list()
data$data201781<-data201781


data$stock201781$splitIndex<-createDataPartition(stock201781$stock_open_move, p = .8, list = FALSE, times = 1)
data$gold201781$splitIndex<-createDataPartition(gold201781$gold_open_move, p = .8, list = FALSE, times = 1)
data$silver201781$splitIndex<-createDataPartition(silver201781$silver_open_move, p = .8, list = FALSE, times = 1)
data$oil201781$splitIndex<-createDataPartition(oil201781$oil_move, p = .8, list = FALSE, times = 1)
data$btc201781$splitIndex<-createDataPartition(btc201781$btc_open_move, p = .8, list = FALSE, times = 1)
data$eth201781$splitIndex<-createDataPartition(eth201781$eth_open_move, p = .8, list = FALSE, times = 1)
data$ltc201781$splitIndex<-createDataPartition(ltc201781$ltc_open_move, p = .8, list = FALSE, times = 1)
data$usdt201781$splitIndex<-createDataPartition(usdt201781$usdt_open_move, p = .8, list = FALSE, times = 1)
data$bch201781$splitIndex<-createDataPartition(bch201781$bch_open_move, p = .8, list = FALSE, times = 1)

data$stock201781$data<-stock201781
data$gold201781$data<-gold201781
data$silver201781$data<-silver201781
data$oil201781$data<-oil201781
data$btc201781$data<-btc201781
data$eth201781$data<-eth201781
data$ltc201781$data<-ltc201781
data$usdt201781$data<-usdt201781
data$bch201781$data<-bch201781

# 1: up; 0: down
data$stock201781$data[,c("stock_open_move")] <- as.character(data$stock201781$data[,c("stock_open_move")])
data$stock201781$data[,c("stock_open_move")] <- as.factor(ifelse(data$stock201781$data[,c("stock_open_move")]== 1,"down", "up"))
data$gold201781$data[,c("gold_open_move")] <- as.character(data$gold201781$data[,c("gold_open_move")])
data$gold201781$data[,c("gold_open_move")] <- as.factor(ifelse(data$gold201781$data[,c("gold_open_move")]== 1,"down", "up"))
data$silver201781$data[,c("silver_open_move")] <- as.character(data$silver201781$data[,c("silver_open_move")])
data$silver201781$data[,c("silver_open_move")] <- as.factor(ifelse(data$silver201781$data[,c("silver_open_move")]== 1,"down", "up"))
data$oil201781$data[,c("oil_move")] <- as.character(data$oil201781$data[,c("oil_move")])
data$oil201781$data[,c("oil_move")] <- as.factor(ifelse(data$oil201781$data[,c("oil_move")]== 1,"down", "up"))
data$btc201781$data[,c("btc_open_move")] <- as.character(data$btc201781$data[,c("btc_open_move")])
data$btc201781$data[,c("btc_open_move")] <- as.factor(ifelse(data$btc201781$data[,c("btc_open_move")]== 1,"down", "up"))
data$eth201781$data[,c("eth_open_move")] <- as.character(data$eth201781$data[,c("eth_open_move")])
data$eth201781$data[,c("eth_open_move")] <- as.factor(ifelse(data$eth201781$data[,c("eth_open_move")]== 1,"down", "up"))
data$ltc201781$data[,c("ltc_open_move")] <- as.character(data$ltc201781$data[,c("ltc_open_move")])
data$ltc201781$data[,c("ltc_open_move")] <- as.factor(ifelse(data$ltc201781$data[,c("ltc_open_move")]== 1,"down", "up"))
data$usdt201781$data[,c("usdt_open_move")] <- as.character(data$usdt201781$data[,c("usdt_open_move")])
data$usdt201781$data[,c("usdt_open_move")] <- as.factor(ifelse(data$usdt201781$data[,c("usdt_open_move")]== 1,"down", "up"))
data$bch201781$data[,c("bch_open_move")] <- as.character(data$bch201781$data[,c("bch_open_move")])
data$bch201781$data[,c("bch_open_move")] <- as.factor(ifelse(data$bch201781$data[,c("bch_open_move")]== 1,"down", "up"))

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))

data$features$wiki<-c(
  "views_btc", "views_eth" ,"views_ltc" ,"views_usdt"  ,"views_bch" , "views_crypto", 
  "btc_sma3", "btc_sma3_diff" , "btc_sma3_move_0" ,"btc_ema5", "btc_ema5_diff","btc_ema5_move_0","btc_RSI","views_btc_diff","views_btc_move_0",
  "eth_sma3", "eth_sma3_diff",  "eth_sma3_move_0","eth_ema5", "eth_ema5_diff","eth_ema5_move_0","eth_RSI","views_eth_diff","views_eth_move_0",
  "ltc_sma3" ,"ltc_sma3_diff" , "ltc_sma3_move_0", "ltc_ema5","ltc_ema5_diff", "ltc_ema5_move_0","ltc_RSI","views_ltc_diff","views_ltc_move_0",
  "usdt_sma3" ,"usdt_sma3_diff" ,"usdt_sma3_move_1" ,"usdt_ema5","usdt_ema5_diff","usdt_ema5_move_1", "usdt_RSI","views_usdt_diff","views_usdt_move_1",
  "bch_sma3" ,"bch_sma3_diff",  "bch_sma3_move_0" , "bch_ema5", "bch_ema5_diff","bch_ema5_move_0","bch_RSI","views_bch_diff","views_bch_move_0",
  "crypto_sma3","crypto_sma3_diff",  "crypto_sma3_move_0" ,"crypto_ema5" ,"crypto_ema5_diff","crypto_ema5_move_0","crypto_RSI",
  "views_crypto_diff","views_crypto_move_0")

#==============================feature selection=======================

data$features=list()


data$features$feat_stock=fsel_FFS_LASSO_RF(tr_index=data$stock201781$splitIndex,exclu= c("date"),df= data$stock201781$data,
                             dep_var= "stock_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_gold=fsel_FFS_LASSO_RF(tr_index=data$gold201781$splitIndex,exclu= c("date"),df= data$gold201781$data,
                             dep_var= "gold_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_silver=fsel_FFS_LASSO_RF(tr_index=data$silver201781$splitIndex,exclu= c("date"),df= data$silver201781$data,
                             dep_var= "silver_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_oil=fsel_FFS_LASSO_RF(tr_index=data$oil201781$splitIndex,exclu= c("date"),df= data$oil201781$data,
                             dep_var= "oil_move",methods=c("FFS","LASSO","RF"),seed0=201905)


data$features$feat_btc=fsel_FFS_LASSO_RF(tr_index=data$btc201781$splitIndex,exclu= c("date"),df= data$btc201781$data,
                                           dep_var= "btc_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_eth=fsel_FFS_LASSO_RF(tr_index=data$eth201781$splitIndex,exclu= c("date"),df= data$eth201781$data,
                                           dep_var= "eth_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_ltc=fsel_FFS_LASSO_RF(tr_index=data$ltc201781$splitIndex,exclu= c("date"),df= data$ltc201781$data,
                                           dep_var= "ltc_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_usdt=fsel_FFS_LASSO_RF(tr_index=data$usdt201781$splitIndex,exclu= c("date"),df= data$usdt201781$data,
                                           dep_var= "usdt_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$feat_bch=fsel_FFS_LASSO_RF(tr_index=data$bch201781$splitIndex,exclu= c("date"),df= data$bch201781$data,
                                           dep_var= "bch_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))


data$features$nonwiki_feat_stock=fsel_FFS_LASSO_RF(tr_index=data$stock201781$splitIndex,exclu= c("date"),
                                           df= data$stock201781$data[!names(data$stock201781$data)%in% data$features$wiki],
                                           dep_var= "stock_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_gold=fsel_FFS_LASSO_RF(tr_index=data$gold201781$splitIndex,exclu= c("date"),
                                          df= data$gold201781$data[!names(data$gold201781$data)%in% data$features$wiki],
                                          dep_var= "gold_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_silver=fsel_FFS_LASSO_RF(tr_index=data$silver201781$splitIndex,exclu= c("date"),
                                            df= data$silver201781$data[!names(data$silver201781$data)%in% data$features$wiki],
                                            dep_var= "silver_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_oil=fsel_FFS_LASSO_RF(tr_index=data$oil201781$splitIndex,exclu= c("date"),
                                            df= data$oil201781$data[!names(data$oil201781$data)%in% data$features$wiki],
                                         dep_var= "oil_move",methods=c("FFS","LASSO","RF"),seed0=201905)


data$features$nonwiki_feat_btc=fsel_FFS_LASSO_RF(tr_index=data$btc201781$splitIndex,exclu= c("date"),
                                         df= data$btc201781$data[!names(data$btc201781$data)%in% data$features$wiki],
                                         dep_var= "btc_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_eth=fsel_FFS_LASSO_RF(tr_index=data$eth201781$splitIndex,exclu= c("date"),
                                         df= data$eth201781$data[!names(data$eth201781$data)%in% data$features$wiki],
                                         dep_var= "eth_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_ltc=fsel_FFS_LASSO_RF(tr_index=data$ltc201781$splitIndex,exclu= c("date"),
                                         df= data$ltc201781$data[!names(data$ltc201781$data)%in% data$features$wiki],
                                         dep_var= "ltc_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_usdt=fsel_FFS_LASSO_RF(tr_index=data$usdt201781$splitIndex,exclu= c("date"),
                                          df= data$usdt201781$data[!names(data$usdt201781$data)%in% data$features$wiki],
                                          dep_var= "usdt_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)
data$features$nonwiki_feat_bch=fsel_FFS_LASSO_RF(tr_index=data$bch201781$splitIndex,exclu= c("date"),
                                         df= data$bch201781$data[!names(data$bch201781$data)%in% data$features$wiki],
                                         dep_var= "bch_open_move",methods=c("FFS","LASSO","RF"),seed0=201905)

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))

#########################################feature selection with genetic algorithm################################

registerDoParallel(4) # parallel processing
getDoParWorkers() # checking no. of workers

ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       number=5,
                       method = "cv",    # 10 fold cross validation
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE)

set.seed(110)
lev <- c("down", "up")  # Set the levels

exclud<- c("date","stock_open_move")
x_features<-data$stock201781$data[data$stock201781$splitIndex,!names(data$stock201781$data)%in%exclud]
y_stock_open_move<-data$stock201781$data[data$stock201781$splitIndex,"stock_open_move"]
system.time(y_stock_open_move_ga <- gafs(x = x_features, y = y_stock_open_move,
                           iters = 100, # 100 generations of algorithm
                           popSize = 20, # population size for each generation
                           levels = lev,
                           gafsControl = ga_ctrl))

exclud<- c("date","gold_open_move")
x_features<-data$gold201781$data[data$gold201781$splitIndex,!names(data$gold201781$data)%in%exclud]
y_gold_open_move<-data$gold201781$data[data$gold201781$splitIndex,"gold_open_move"]
system.time(y_gold_open_move_ga <- gafs(x = x_features, y = y_gold_open_move,
                                         iters = 100, # 100 generations of algorithm
                                         popSize = 20, # population size for each generation
                                         levels = lev,
                                         gafsControl = ga_ctrl))

exclud<- c("date","silver_open_move")
x_features<-data$silver201781$data[data$silver201781$splitIndex,!names(data$silver201781$data)%in%exclud]
y_silver_open_move<-data$silver201781$data[data$silver201781$splitIndex,"silver_open_move"]
system.time(y_silver_open_move_ga <- gafs(x = x_features, y = y_silver_open_move,
                                          iters = 100, # 100 generations of algorithm
                                          popSize = 20, # population size for each generation
                                          levels = lev,
                                          gafsControl = ga_ctrl))

exclud<- c("date","oil_move")
x_features<-data$oil201781$data[data$oil201781$splitIndex,!names(data$oil201781$data)%in%exclud]
y_oil_move<-data$oil201781$data[data$oil201781$splitIndex,"oil_move"]
system.time(y_oil_move_ga <- gafs(x = x_features, y = y_oil_move,
                                  iters = 100, # 100 generations of algorithm
                                  popSize = 20, # population size for each generation
                                  levels = lev,
                                  gafsControl = ga_ctrl))

exclud<- c("date","btc_open_move")
x_features<-data$btc201781$data[data$btc201781$splitIndex,!names(data$btc201781$data)%in%exclud]
y_btc_open_move<-data$btc201781$data[data$btc201781$splitIndex,"btc_open_move"]
system.time(y_btc_open_move_ga <- gafs(x = x_features, y = y_btc_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","eth_open_move")
x_features<-data$eth201781$data[data$eth201781$splitIndex,!names(data$eth201781$data)%in%exclud]
y_eth_open_move<-data$eth201781$data[data$eth201781$splitIndex,"eth_open_move"]
system.time(y_eth_open_move_ga <- gafs(x = x_features, y = y_eth_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","ltc_open_move")
x_features<-data$ltc201781$data[data$ltc201781$splitIndex,!names(data$ltc201781$data)%in%exclud]
y_ltc_open_move<-data$ltc201781$data[data$ltc201781$splitIndex,"ltc_open_move"]
system.time(y_ltc_open_move_ga <- gafs(x = x_features, y = y_ltc_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","usdt_open_move")
x_features<-data$usdt201781$data[data$usdt201781$splitIndex,!names(data$usdt201781$data)%in%exclud]
y_usdt_open_move<-data$usdt201781$data[data$usdt201781$splitIndex,"usdt_open_move"]
system.time(y_usdt_open_move_ga <- gafs(x = x_features, y = y_usdt_open_move,
                                        iters = 100, # 100 generations of algorithm
                                        popSize = 20, # population size for each generation
                                        levels = lev,
                                        gafsControl = ga_ctrl))

exclud<- c("date","bch_open_move")
x_features<-data$bch201781$data[data$bch201781$splitIndex,!names(data$bch201781$data)%in%exclud]
y_bch_open_move<-data$bch201781$data[data$bch201781$splitIndex,"bch_open_move"]
system.time(y_bch_open_move_ga <- gafs(x = x_features, y = y_bch_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

data$GAs<-list()
data$GAs$y_stock_open_move_ga<-y_stock_open_move_ga
data$GAs$y_gold_open_move_ga<-y_gold_open_move_ga
data$GAs$y_silver_open_move_ga<-y_silver_open_move_ga
data$GAs$y_oil_move_ga<-y_oil_move_ga
data$GAs$y_btc_open_move_ga<-y_btc_open_move_ga
data$GAs$y_eth_open_move_ga<-y_eth_open_move_ga
data$GAs$y_ltc_open_move_ga<-y_ltc_open_move_ga
data$GAs$y_usdt_open_move_ga<-y_usdt_open_move_ga
data$GAs$y_bch_open_move_ga<-y_bch_open_move_ga

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))

########################################non-wiki GA features###########################################


ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       number=5,
                       method = "cv",    # 10 fold cross validation
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE)

set.seed(110)
lev <- c("down", "up")  # Set the levels

exclud<- c("date","stock_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$stock201781$data[data$stock201781$splitIndex,!names(data$stock201781$data)%in%exclud]
y_stock_open_move<-data$stock201781$data[data$stock201781$splitIndex,"stock_open_move"]
system.time(y_stock_open_move_nonwiki_ga <- gafs(x = x_features, y = y_stock_open_move,
                                         iters = 100, # 100 generations of algorithm
                                         popSize = 20, # population size for each generation
                                         levels = lev,
                                         gafsControl = ga_ctrl))

exclud<- c("date","gold_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$gold201781$data[data$gold201781$splitIndex,!names(data$gold201781$data)%in%exclud]
y_gold_open_move<-data$gold201781$data[data$gold201781$splitIndex,"gold_open_move"]
system.time(y_gold_open_move_nonwiki_ga <- gafs(x = x_features, y = y_gold_open_move,
                                        iters = 100, # 100 generations of algorithm
                                        popSize = 20, # population size for each generation
                                        levels = lev,
                                        gafsControl = ga_ctrl))

exclud<- c("date","silver_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$silver201781$data[data$silver201781$splitIndex,!names(data$silver201781$data)%in%exclud]
y_silver_open_move<-data$silver201781$data[data$silver201781$splitIndex,"silver_open_move"]
system.time(y_silver_open_move_nonwiki_ga <- gafs(x = x_features, y = y_silver_open_move,
                                          iters = 100, # 100 generations of algorithm
                                          popSize = 20, # population size for each generation
                                          levels = lev,
                                          gafsControl = ga_ctrl))

exclud<- c("date","oil_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$oil201781$data[data$oil201781$splitIndex,!names(data$oil201781$data)%in%exclud]
y_oil_move<-data$oil201781$data[data$oil201781$splitIndex,"oil_move"]
system.time(y_oil_move_nonwiki_ga <- gafs(x = x_features, y = y_oil_move,
                                  iters = 100, # 100 generations of algorithm
                                  popSize = 20, # population size for each generation
                                  levels = lev,
                                  gafsControl = ga_ctrl))

exclud<- c("date","btc_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$btc201781$data[data$btc201781$splitIndex,!names(data$btc201781$data)%in%exclud]
y_btc_open_move<-data$btc201781$data[data$btc201781$splitIndex,"btc_open_move"]
system.time(y_btc_open_move_nonwiki_ga <- gafs(x = x_features, y = y_btc_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","eth_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$eth201781$data[data$eth201781$splitIndex,!names(data$eth201781$data)%in%exclud]
y_eth_open_move<-data$eth201781$data[data$eth201781$splitIndex,"eth_open_move"]
system.time(y_eth_open_move_nonwiki_ga <- gafs(x = x_features, y = y_eth_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","ltc_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$ltc201781$data[data$ltc201781$splitIndex,!names(data$ltc201781$data)%in%exclud]
y_ltc_open_move<-data$ltc201781$data[data$ltc201781$splitIndex,"ltc_open_move"]
system.time(y_ltc_open_move_nonwiki_ga <- gafs(x = x_features, y = y_ltc_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

exclud<- c("date","usdt_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$usdt201781$data[data$usdt201781$splitIndex,!names(data$usdt201781$data)%in%exclud]
y_usdt_open_move<-data$usdt201781$data[data$usdt201781$splitIndex,"usdt_open_move"]
system.time(y_usdt_open_move_nonwiki_ga <- gafs(x = x_features, y = y_usdt_open_move,
                                        iters = 100, # 100 generations of algorithm
                                        popSize = 20, # population size for each generation
                                        levels = lev,
                                        gafsControl = ga_ctrl))

exclud<- c("date","bch_open_move")
exclud<-c(data$features$wiki,exclud)
x_features<-data$bch201781$data[data$bch201781$splitIndex,!names(data$bch201781$data)%in%exclud]
y_bch_open_move<-data$bch201781$data[data$bch201781$splitIndex,"bch_open_move"]
system.time(y_bch_open_move_nonwiki_ga <- gafs(x = x_features, y = y_bch_open_move,
                                       iters = 100, # 100 generations of algorithm
                                       popSize = 20, # population size for each generation
                                       levels = lev,
                                       gafsControl = ga_ctrl))

data$GAs<-list()
data$GAs$y_stock_open_move_nonwiki_ga<-y_stock_open_move_nonwiki_ga
data$GAs$y_gold_open_move_nonwiki_ga<-y_gold_open_move_nonwiki_ga
data$GAs$y_silver_open_move_nonwiki_ga<-y_silver_open_move_nonwiki_ga
data$GAs$y_oil_move_nonwiki_ga<-y_oil_move_nonwiki_ga
data$GAs$y_btc_open_move_nonwiki_ga<-y_btc_open_move_nonwiki_ga
data$GAs$y_eth_open_move_nonwiki_ga<-y_eth_open_move_nonwiki_ga
data$GAs$y_ltc_open_move_nonwiki_ga<-y_ltc_open_move_nonwiki_ga
data$GAs$y_usdt_open_move_nonwiki_ga<-y_usdt_open_move_nonwiki_ga
data$GAs$y_bch_open_move_nonwiki_ga<-y_bch_open_move_nonwiki_ga

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))


data$features$crypto<-c("BTC_USD_Open",    "BTC_USD_High",    "BTC_USD_Low",    
"BTC_USD_Close",   "BTC_USD_Volume",  "BTC_USD_Adjusted","btc_usd_open_rsi","btc_usd_close_rsi" ,"btc_close_rsi_diff" ,
"btc_close_rsi_move_0","btc_open_rsi_diff","btc_open_rsi_move_0","btc_open_diff","btc_close_diff",
"btc_open_move_0",   "btc_close_move_0",  "ETH_USD_Open",   
"ETH_USD_High",    "ETH_USD_Low",     "ETH_USD_Close",   "ETH_USD_Volume",  "ETH_USD_Adjusted","eth_usd_open_rsi" ,  
"eth_usd_close_rsi","eth_close_rsi_diff" ,"eth_close_rsi_move_0" ,"eth_open_rsi_diff","eth_open_rsi_move_0", 
"eth_open_diff","eth_close_diff", "eth_open_move_0",  
"eth_close_move_0",  "LTC_USD_Open",    "LTC_USD_High",    "LTC_USD_Low",     "LTC_USD_Close",   "LTC_USD_Volume", 
"LTC_USD_Adjusted","ltc_usd_open_rsi","ltc_usd_close_rsi","ltc_close_rsi_diff","ltc_close_rsi_move_0", "ltc_open_rsi_diff",  
"ltc_open_rsi_move_0","ltc_open_diff","ltc_close_diff",
"ltc_open_move_0",   "ltc_close_move_0",  "USDT_USD_Open",   "USDT_USD_High",   "USDT_USD_Low",   
"USDT_USD_Close",  "USDT_USD_Volume", "USDT_USD_Adjusted","usdt_usd_open_rsi","usdt_usd_close_rsi" ,"usdt_close_rsi_diff",
"usdt_close_rsi_move_0","usdt_open_rsi_diff","usdt_open_rsi_move_0","usdt_open_diff","usdt_close_diff",
"usdt_open_move_0",  "usdt_close_move_0", "BCH_USD_Open",   
"BCH_USD_High",    "BCH_USD_Low",     "BCH_USD_Close",   "BCH_USD_Volume",  "BCH_USD_Adjusted","bch_usd_open_rsi",   
"bch_usd_close_rsi" ,  "bch_close_rsi_diff","bch_close_rsi_move_1", "bch_open_rsi_diff" ,"bch_open_rsi_move_1",
"bch_open_diff","bch_close_diff","bch_open_move_0","bch_close_move_0")  


data$features$DJI<-c(
  "DJI.Open" , "DJI.High" , "DJI.Low" , "DJI.Close" ,"DJI.Volume" ,    
  "DJI.Adjusted","dji_open_diff", "dji_close_diff","dji_open_rsi" , "dji_close_rsi"
)

data$features$gold<-c(
  "gold_Open","gold_High" ,"gold_Low" ,"gold_Close","gold_Volume","gold_Adjusted","gold_open_diff",   
  "gold_close_diff","gold_open_rsi","gold_close_rsi","gold_open_move_0","gold_close_move_0","gold_open_move_1","gold_close_move_1"
)

data$features$silver<-c(
  "silver_Open","silver_High","silver_Low","silver_Close","silver_Volume","silver_Adjusted","silver_open_diff",   
  "silver_close_diff","silver_open_rsi","silver_close_rsi","silver_open_move_0", "silver_close_move_0",
  "silver_open_move_1", "silver_close_move_1"
)

data$features$stock<-c(
  "stock_Open","stock_High","stock_Low","stock_Close","stock_Volume","stock_Adjusted","stock_open_diff",   
  "stock_close_diff","stock_open_RSI","stock_close_RSI","stock_open_move_0","stock_close_move_0",
  "stock_open_move_1","stock_close_move_1"
)

data$features$oil<-c(
  "oil","oil_diff" ,"oil_rsi" ,"oil_move_1"
)
data$features$date<-c(
  "dateorder"  , "dateweek_Mon_0","dateweek_Tue_0",
  "dateweek_Wed_0", "dateweek_Thu_0" ,"dateweek_Fri_0"  
)

data$features$wiki
data$features$all<-c(data$features$crypto,data$features$wiki,data$features$DJI,data$features$gold,data$features$silver,
                     data$features$stock,data$features$oil
                     ,data$features$date
                     )


saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))

###############################feature selection#######################


#==============================stock prediction=======================

# saved<-readRDS("C:/Users/hahadydolatsara/OneDrive - Clark University/Research topics/blockchain/binweng/oneday/goodones/all_perf-AAPL12-29glmnone_V_all.rds")
# data<-saved$data

#saved$data$features$stock_close_move_all

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
data$stock201781$data$ID<-rownames(data$stock201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds<-list()
data$preds$bayesglm<-list()
data$preds$svmRadial<-list()
data$preds$xgbDART<-list()

data$preds$pred_stock_open_move_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                     features0= data$features$feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_stock_open_move_nonwiki_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                     features0= data$features$nonwiki_feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_stock_open_move_crypto<-train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move",  
                     features0= data$features$crypto[data$features$crypto%in% names(data$stock201781$data)],
                     resample_fact=sam_bal,alg_fact=alg,cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_stock_open_move_all<-train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                     features0= data$features$all[data$features$all%in% names(data$stock201781$data)],
                     resample_fact= sam_bal,alg_fact=alg,
                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_stock_open_move_ga<-train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                     features0= data$GAs$y_stock_open_move_ga$ga$final,
                     resample_fact= sam_bal,alg_fact=alg,
                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_stock_open_move_nonwiki_ga<-train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                     features0= data$GAs$y_stock_open_move_nonwiki_ga$ga$final,
                     resample_fact= sam_bal,alg_fact=alg,
                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

#fda .875 none  bayesglm up 0.8791209
data$preds$pred_stock_open_move_f.select$Performance
data$preds$pred_stock_open_move_nonwiki_f.select$Performance
data$preds$pred_stock_open_move_crypto$Performance
data$preds$pred_stock_open_move_all$Performance
data$preds$pred_stock_open_move_ga$Performance
data$preds$pred_stock_open_move_nonwiki_ga$Performance

#===================================================
#########################open price stock prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
data$stock201781$data$ID<-rownames(data$stock201781$data)
data$preds$bayesglm$pred_stock_open_move_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_stock_open_move_nonwiki_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$nonwiki_feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

alg<-"svmRadial"
data$preds$svmRadial$pred_stock_open_move_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_stock_open_move_nonwiki_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$nonwiki_feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_stock_open_move_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_stock_open_move_nonwiki_f.select<- train_pred(data0=data$stock201781$data,splitIndex0=data$stock201781$splitIndex,TARGET0="stock_open_move", 
                                                      features0= data$features$nonwiki_feat_stock$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_stock_open_move_f.select$Performance
data$preds$bayesglm$pred_stock_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_stock_open_move_f.select$Performance
data$preds$svmRadial$pred_stock_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_stock_open_move_f.select$Performance
data$preds$xgbDART$pred_stock_open_move_nonwiki_f.select$Performance

#==============================gold prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="gold_open_move"
data$gold201781$data$ID<-rownames(data$gold201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_gold_open_move_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_gold_open_move_nonwiki_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_gold_open_move_crypto<-train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target,  
                                                  features0= data$features$crypto[data$features$crypto%in% names(data$gold201781$data)],
                                                  resample_fact=sam_bal,alg_fact=alg,
                                                  cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_gold_open_move_all<-train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                               features0= data$features$all[data$features$all%in% names(data$gold201781$data)],
                                               resample_fact= sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_gold_open_move_ga<-train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                              features0= data$GAs$y_gold_open_move_ga$ga$final,
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_gold_open_move_nonwiki_ga<-train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                              features0= data$GAs$y_gold_open_move_nonwiki_ga$ga$final,
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

#fda .875 none  bayesglm ga/all none auc  0.8387097
data$preds$pred_gold_open_move_f.select$Performance
data$preds$pred_gold_open_move_nonwiki_f.select$Performance
data$preds$pred_gold_open_move_crypto$Performance
data$preds$pred_gold_open_move_all$Performance
data$preds$pred_gold_open_move_ga$Performance
data$preds$pred_gold_open_move_nonwiki_ga$Performance

#===================================================
#########################open price gold prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="gold_open_move"
data$gold201781$data$ID<-rownames(data$gold201781$data)
data$preds$bayesglm$pred_gold_open_move_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_gold_open_move_nonwiki_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_gold_open_move_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_gold_open_move_nonwiki_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_gold_open_move_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_gold_open_move_nonwiki_f.select<- train_pred(data0=data$gold201781$data,splitIndex0=data$gold201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_gold$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_gold_open_move_f.select$Performance
data$preds$bayesglm$pred_gold_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_gold_open_move_f.select$Performance
data$preds$svmRadial$pred_gold_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_gold_open_move_f.select$Performance
data$preds$xgbDART$pred_gold_open_move_nonwiki_f.select$Performance

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))


#==============================silver prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="silver_open_move"
data$silver201781$data$ID<-rownames(data$silver201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_silver_open_move_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                  features0= data$features$feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                  cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_silver_open_move_nonwiki_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                  features0= data$features$nonwiki_feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                  cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_silver_open_move_crypto<-train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target,  
                                                  features0= data$features$crypto[data$features$crypto%in% names(data$silver201781$data)],
                                                  resample_fact=sam_bal,alg_fact=alg,
                                                  cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_silver_open_move_all<-train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                 features0= data$features$all[data$features$all%in% names(data$silver201781$data)],
                                                 resample_fact= sam_bal,alg_fact=alg,
                                                 cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_silver_open_move_ga<-train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                features0= data$GAs$y_silver_open_move_ga$ga$final,
                                                resample_fact= sam_bal,alg_fact=alg,
                                                cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_silver_open_move_nonwiki_ga<-train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                features0= data$GAs$y_silver_open_move_nonwiki_ga$ga$final,
                                                resample_fact= sam_bal,alg_fact=alg,
                                                cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


# bayesglm up 1
data$preds$pred_silver_open_move_f.select$Performance
data$preds$pred_silver_open_move_nonwiki_f.select$Performance
data$preds$pred_silver_open_move_crypto$Performance
data$preds$pred_silver_open_move_all$Performance
data$preds$pred_silver_open_move_ga$Performance
data$preds$pred_silver_open_move_nonwiki_ga$Performance

#===================================================
#########################open silver gold prediction
#===================================================

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="silver_open_move"
data$silver201781$data$ID<-rownames(data$silver201781$data)

data$preds$bayesglm$pred_silver_open_move_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                       features0= data$features$feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                       cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_silver_open_move_nonwiki_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                      features0= data$features$nonwiki_feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                      cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_silver_open_move_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                       features0= data$features$feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                       cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_silver_open_move_nonwiki_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                       features0= data$features$nonwiki_feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                       cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_silver_open_move_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                       features0= data$features$feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                       cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_silver_open_move_nonwiki_f.select<- train_pred(data0=data$silver201781$data,splitIndex0=data$silver201781$splitIndex,TARGET0=target, 
                                                       features0= data$features$nonwiki_feat_silver$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                       cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_silver_open_move_f.select$Performance
data$preds$bayesglm$pred_silver_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_silver_open_move_f.select$Performance
data$preds$svmRadial$pred_silver_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_silver_open_move_f.select$Performance
data$preds$xgbDART$pred_silver_open_move_nonwiki_f.select$Performance

#==============================oil prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"svmRadial"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="oil_move"
data$oil201781$data$ID<-rownames(data$oil201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_oil_move_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_oil_move_nonwiki_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$nonwiki_feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_oil_move_crypto<-train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target,  
                                            features0= data$features$crypto[data$features$crypto%in% names(data$oil201781$data)],
                                            resample_fact=sam_bal,alg_fact=alg,
                                            cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_oil_move_all<-train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                         features0= data$features$all[data$features$all%in% names(data$oil201781$data)],
                                         resample_fact= sam_bal,alg_fact=alg,
                                         cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_oil_move_ga<-train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                        features0= data$GAs$y_oil_move_ga$ga$final,
                                        resample_fact= sam_bal,alg_fact=alg,
                                        cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_oil_move_nonwiki_ga<-train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                        features0= data$GAs$y_oil_move_nonwiki_ga$ga$final,
                                        resample_fact= sam_bal,alg_fact=alg,
                                        cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


#fda .875 none  bayesglm up 0.8791209
data$preds$pred_oil_move_f.select$Performance
data$preds$pred_oil_move_nonwiki_f.select$Performance
data$preds$pred_oil_move_crypto$Performance
data$preds$pred_oil_move_all$Performance
data$preds$pred_oil_move_ga$Performance
data$preds$pred_oil_move_nonwiki_ga$Performance


alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="oil_move"
data$oil201781$data$ID<-rownames(data$oil201781$data)
data$preds$ens_pred_oil_move_all<-ense_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target,
                    TARGET0_upclass="up" ,features0= data$features$feat_oil$vars_all,
                    resample_fact=sam_bal, cent_sclae=center_scaling,folds=5, seed0=2019)
ens_alg="bayesglm"
sam_bal="up"
data$preds$ensx_open_move_all<-excl_fun(cor_df=data$preds$ens_pred_oil_move_all$results$models_cor,stack_alg=ens_alg,
                             perf_df=data$preds$ens_pred_oil_move_all$results$resul_pred_perf, 
                             preds=data$preds$ens_pred_oil_move_all$results$model_preds,
                             ens_models=data$preds$ens_pred_oil_move_all$results$models_all,metric="gmean",cor_thresh=0.6,
                             data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target)

#===================================================
#########################oil prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="oil_move"
data$oil201781$data$ID<-rownames(data$oil201781$data)

data$preds$bayesglm$pred_oil_move_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_oil_move_nonwiki_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$nonwiki_feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_oil_move_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_oil_move_nonwiki_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$nonwiki_feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_oil_move_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_oil_move_nonwiki_f.select<- train_pred(data0=data$oil201781$data,splitIndex0=data$oil201781$splitIndex,TARGET0=target, 
                                               features0= data$features$nonwiki_feat_oil$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_oil_move_f.select$Performance
data$preds$bayesglm$pred_oil_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_oil_move_f.select$Performance
data$preds$svmRadial$pred_oil_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_oil_move_f.select$Performance
data$preds$xgbDART$pred_oil_move_nonwiki_f.select$Performance

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))
  

#==============================btc prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="btc_open_move"
data$btc201781$data$ID<-rownames(data$btc201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_btc_open_move_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_btc_open_move_nonwiki_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_btc_open_move_crypto<-train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target,  
                                                 features0= data$features$crypto[data$features$crypto%in% names(data$btc201781$data)],
                                                 resample_fact=sam_bal,alg_fact=alg,
                                                 cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_btc_open_move_all<-train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                              features0= data$features$all[data$features$all%in% names(data$btc201781$data)],
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_btc_open_move_ga<-train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_btc_open_move_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_btc_open_move_nonwiki_ga<-train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_btc_open_move_nonwiki_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


#fda .875 none  bayesglm up 0.8791209
data$preds$pred_btc_open_move_f.select$Performance
data$preds$pred_btc_open_move_nonwiki_f.select$Performance
data$preds$pred_btc_open_move_crypto$Performance
data$preds$pred_btc_open_move_all$Performance
data$preds$pred_btc_open_move_ga$Performance
data$preds$pred_btc_open_move_nonwiki_ga$Performance

#===================================================
#########################open BTC prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="btc_open_move"
data$btc201781$data$ID<-rownames(data$btc201781$data)
data$preds$bayesglm$pred_btc_open_move_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_btc_open_move_nonwiki_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_btc_open_move_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_btc_open_move_nonwiki_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                   features0= data$features$nonwiki_feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                   cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_btc_open_move_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_btc_open_move_nonwiki_f.select<- train_pred(data0=data$btc201781$data,splitIndex0=data$btc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_btc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_btc_open_move_f.select$Performance
data$preds$bayesglm$pred_btc_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_btc_open_move_f.select$Performance
data$preds$svmRadial$pred_btc_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_btc_open_move_f.select$Performance
data$preds$xgbDART$pred_btc_open_move_nonwiki_f.select$Performance


#==============================eth prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="eth_open_move"
data$eth201781$data$ID<-rownames(data$eth201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_eth_open_move_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_eth_open_move_nonwiki_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_eth_open_move_crypto<-train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target,  
                                                 features0= data$features$crypto[data$features$crypto%in% names(data$eth201781$data)],
                                                 resample_fact=sam_bal,alg_fact=alg,
                                                 cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_eth_open_move_all<-train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                              features0= data$features$all[data$features$all%in% names(data$eth201781$data)],
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_eth_open_move_ga<-train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_eth_open_move_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_eth_open_move_nonwiki_ga<-train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_eth_open_move_nonwiki_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


#fda .875 none  bayesglm up 0.8791209
data$preds$pred_eth_open_move_f.select$Performance
data$preds$pred_eth_open_move_nonwiki_f.select$Performance
data$preds$pred_eth_open_move_crypto$Performance
data$preds$pred_eth_open_move_all$Performance
data$preds$pred_eth_open_move_ga$Performance
data$preds$pred_eth_open_move_nonwiki_ga$Performance

#===================================================
#########################open ETH prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="eth_open_move"
data$eth201781$data$ID<-rownames(data$eth201781$data)

data$preds$bayesglm$pred_eth_open_move_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_eth_open_move_nonwiki_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_eth_open_move_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_eth_open_move_nonwiki_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                   features0= data$features$nonwiki_feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                   cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_eth_open_move_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_eth_open_move_nonwiki_f.select<- train_pred(data0=data$eth201781$data,splitIndex0=data$eth201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_eth$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

data$preds$bayesglm$pred_eth_open_move_f.select$Performance
data$preds$bayesglm$pred_eth_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_eth_open_move_f.select$Performance
data$preds$svmRadial$pred_eth_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_eth_open_move_f.select$Performance
data$preds$xgbDART$pred_eth_open_move_nonwiki_f.select$Performance

#==============================ltc prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="ltc_open_move"
data$ltc201781$data$ID<-rownames(data$ltc201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_ltc_open_move_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_ltc_open_move_nonwiki_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_ltc_open_move_crypto<-train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target,  
                                                 features0= data$features$crypto[data$features$crypto%in% names(data$ltc201781$data)],
                                                 resample_fact=sam_bal,alg_fact=alg,
                                                 cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_ltc_open_move_all<-train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                              features0= data$features$all[data$features$all%in% names(data$ltc201781$data)],
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_ltc_open_move_ga<-train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_ltc_open_move_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_ltc_open_move_nonwiki_ga<-train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_ltc_open_move_nonwiki_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


#fda .875 none  bayesglm up 0.8791209
data$preds$pred_ltc_open_move_f.select$Performance
data$preds$pred_ltc_open_move_nonwiki_f.select$Performance
data$preds$pred_ltc_open_move_crypto$Performance
data$preds$pred_ltc_open_move_all$Performance
data$preds$pred_ltc_open_move_ga$Performance
data$preds$pred_ltc_open_move_nonwiki_ga$Performance

#===================================================
#########################open LTC prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="ltc_open_move"
data$ltc201781$data$ID<-rownames(data$ltc201781$data)

data$preds$bayesglm$pred_ltc_open_move_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_ltc_open_move_nonwiki_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_ltc_open_move_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_ltc_open_move_nonwiki_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_ltc_open_move_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_ltc_open_move_nonwiki_f.select<- train_pred(data0=data$ltc201781$data,splitIndex0=data$ltc201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_ltc$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


data$preds$bayesglm$pred_eth_open_move_f.select$Performance
data$preds$bayesglm$pred_ltc_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_eth_open_move_f.select$Performance
data$preds$svmRadial$pred_ltc_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_eth_open_move_f.select$Performance
data$preds$xgbDART$pred_ltc_open_move_nonwiki_f.select$Performance

#==============================usdt prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="usdt_open_move"
data$usdt201781$data$ID<-rownames(data$usdt201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$bayesglm$pred_usdt_open_move_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_usdt_open_move_nonwiki_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadia$pred_usdt_open_move_crypto<-train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target,  
                                                  features0= data$features$crypto[data$features$crypto%in% names(data$usdt201781$data)],
                                                  resample_fact=sam_bal,alg_fact=alg,
                                                  cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadia$pred_usdt_open_move_all<-train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                               features0= data$features$all[data$features$all%in% names(data$usdt201781$data)],
                                               resample_fact= sam_bal,alg_fact=alg,
                                               cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_usdt_open_move_ga<-train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                              features0= data$GAs$y_usdt_open_move_ga$ga$final,
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_usdt_open_move_nonwiki_ga<-train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                              features0= data$GAs$y_usdt_open_move_nonwiki_ga$ga$final,
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)

#fda .875 none  bayesglm up 0.8791209
data$preds$bayesglm$pred_usdt_open_move_f.select$Performance
data$preds$bayesglm$pred_usdt_open_move_nonwiki_f.select$Performance
data$preds$svmRadia$pred_usdt_open_move_crypto$Performance
data$preds$svmRadia$pred_usdt_open_move_all$Performance
data$preds$xgbDART$pred_usdt_open_move_ga$Performance
data$preds$xgbDART$pred_usdt_open_move_nonwiki_ga$Performance

#===================================================
#########################open USDT prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="usdt_open_move"
data$usdt201781$data$ID<-rownames(data$usdt201781$data)
data$preds$bayesglm$pred_usdt_open_move_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_usdt_open_move_nonwiki_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_usdt_open_move_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_usdt_open_move_nonwiki_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$nonwiki_feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_usdt_open_move_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                     features0= data$features$feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                     cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_usdt_open_move_nonwiki_f.select<- train_pred(data0=data$usdt201781$data,splitIndex0=data$usdt201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_usdt$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


data$preds$bayesglm$pred_usdt_open_move_f.select$Performance
data$preds$bayesglm$pred_usdt_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_usdt_open_move_f.select$Performance
data$preds$svmRadial$pred_usdt_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_usdt_open_move_f.select$Performance
data$preds$xgbDART$pred_usdt_open_move_nonwiki_f.select$Performance

#==============================bch prediction=======================

# glm,xgbDART,naive_bayes, nnet,svmRadial,ranger, rpart,lda

alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="gmean"
target="bch_open_move"
data$bch201781$data$ID<-rownames(data$bch201781$data)
# "lda", "lda2","stepLDA","lssvmLinear","hdda","hda","glmnet","fda",
# "LogitBoost","ada","bayesglm","bagEarth","bagEarthGCV"
# alg<-xgbDART,glm,xgbDART,naive_bayes
# alg<-"svmRadial"   YES
# none,smote,down,up,rose
# metric="ROC","gmean"
data$preds$pred_bch_open_move_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_bch_open_move_nonwiki_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_bch_open_move_crypto<-train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target,  
                                                 features0= data$features$crypto[data$features$crypto%in% names(data$bch201781$data)],
                                                 resample_fact=sam_bal,alg_fact=alg,
                                                 cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_bch_open_move_all<-train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                              features0= data$features$all[data$features$all%in% names(data$bch201781$data)],
                                              resample_fact= sam_bal,alg_fact=alg,
                                              cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_bch_open_move_ga<-train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_bch_open_move_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$pred_bch_open_move_nonwiki_ga<-train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                             features0= data$GAs$y_bch_open_move_nonwiki_ga$ga$final,
                                             resample_fact= sam_bal,alg_fact=alg,
                                             cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


#fda .875 none  bayesglm up 0.8791209
data$preds$pred_bch_open_move_f.select$Performance
data$preds$pred_bch_open_move_nonwiki_f.select$Performance
data$preds$pred_bch_open_move_crypto$Performance
data$preds$pred_bch_open_move_all$Performance
data$preds$pred_bch_open_move_ga$Performance
data$preds$pred_bch_open_move_nonwiki_ga$Performance

#===================================================
#########################open BCH prediction
#===================================================
alg<-"bayesglm"
sam_bal<-"up"
center_scaling<-"NO"
metrics="ROC"
target="bch_open_move"
data$bch201781$data$ID<-rownames(data$bch201781$data)
data$preds$bayesglm$pred_bch_open_move_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$bayesglm$pred_bch_open_move_nonwiki_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"svmRadial"
data$preds$svmRadial$pred_bch_open_move_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$svmRadial$pred_bch_open_move_nonwiki_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
alg<-"xgbDART"
data$preds$xgbDART$pred_bch_open_move_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)
data$preds$xgbDART$pred_bch_open_move_nonwiki_f.select<- train_pred(data0=data$bch201781$data,splitIndex0=data$bch201781$splitIndex,TARGET0=target, 
                                                    features0= data$features$nonwiki_feat_bch$vars_all,resample_fact=sam_bal,alg_fact=alg,
                                                    cent_sclae=center_scaling,folds=5,metric_eval=metrics,seed0=2019)


data$preds$bayesglm$pred_bch_open_move_f.select$Performance
data$preds$bayesglm$pred_bch_open_move_nonwiki_f.select$Performance
data$preds$svmRadial$pred_bch_open_move_f.select$Performance
data$preds$svmRadial$pred_bch_open_move_nonwiki_f.select$Performance
data$preds$xgbDART$pred_bch_open_move_f.select$Performance
data$preds$xgbDART$pred_bch_open_move_nonwiki_f.select$Performance

saveRDS(data,paste0(loc_add,"data.rds"))
data<-readRDS(paste0(loc_add,"data.rds"))

###############################prediction#######################

# deep learning with mxnet
library(mxnet) #call library first
library(caret)
# specify tuning parameters of the dnn with 3 layers
mlp_grid <- expand.grid(
  layer1 = 100,
  layer2 = 50,
  layer3 = 1,
  learning.rate = 0.05,
  momentum = 0.01,
  dropout = 0,
  activation = "relu"
)

gmeanfunction <- function(data, lev = NULL, model = NULL) {
  sub_sens<-caret::sensitivity(data$pred,data$obs)
  sub_spec<-caret::specificity(data$pred,data$obs)
  c(gmean = sqrt(sub_sens*sub_spec))
}

alg<-"glm"
sam_bal<-"up"
center_scaling<-"NO"
data0=data$data201781$data
TARGET0="stock_close_move"
#features0= data$features$all[!(data$features$all)%in%c("stock_close_move","stock_open_move")]
features0=  data$features$stock_open_move_all
splitIndex0=data$data201781$splitIndex
data0<-data0[,c(features0,TARGET0)]
traindata<-data0[splitIndex0,]
hold_out<-data0[-splitIndex0,]
resample_fact=sam_bal
alg_fact=alg
cent_sclae=center_scaling
folds=5
seed0=2019


control_setting <- caret::trainControl(method = "cv", number=folds, sampling=resample_fact , 
                                       #summaryFunction = twoClassSummary, 
                                       preProcOptions = list(thresh=.95), # threshold for pca preprocess
                                       search="random", classProbs = TRUE, selectionFunction="tolerance"
                                       #,summaryFunction = gmeanfunction
                                       )


# train DNN model
mlp_fit = train(
  x = data_train.x, 
  y = data_train.y, 
  method = "mxnet", 
  #tuneGrid = mlp_grid
  tuneLength=10
)



set.seed(seed0)


formul<-as.formula(paste0(as.character(TARGET0[1]),"~."))

result_model <- train(formul, data=traindata, method="mxnet", 
                      #family="binomial",
                      #trControl = control_setting #, metric="gmean"
                      tuneLength=10
                      )


preds = predict(result_model,hold_out, type="raw")

head(preds)

confusionMatrix(preds, hold_out$stock_close_move)


traindata$stock_close_move

############3mxnet2

traindata[!names(traindata) %in% "stock_close_move"]

data_train = data.matrix(traindata)
data_train.x = data_train[,!colnames(data_train) %in% TARGET0] #predictors
data_train.y = data_train[,TARGET0]#response var - label

data_test = data.matrix(hold_out)
data_test.x = data_test[,!colnames(data_test) %in% TARGET0] #predictors
data_test.y = data_test[,TARGET0]#response var - label

data <- mx.symbol.Variable("data")

### FIRST HIDDEN LAYER- 128 neurons
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")

### 2nd HIDDEN LAYER- 64 neurons
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")

### 3rd HIDDEN LAYER- 10 neurons
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")

devices<-mx.cpu() #uses CPU

model_dnn=mx.model.FeedForward.create(softmax,X=data_train.x,
                                      y=data_train.y,ctx=devices,num.round = 300,
                                      array.batch.size = 100,learning.rate=0.01,
                                      momentum=0.9,eval.metric = mx.metric.accuracy,
                                      initializer=mx.init.uniform(0.1),
                                      epoch.end.callback=mx.callback.log.train.metric(100))

graph.viz(model_dnn$symbol)

preds = predict(model_dnn,data_test.x)

## implement DNN model on 25% testing data predictors
prediction_dnn <- max.col(t(preds)) - 1

cm_dnn = table(data_test.y, prediction_dnn)
cm_dnn

# Keras

library(caret)
library(keras)

Train_Features = data.matrix(traindata[,!colnames(traindata) %in% TARGET0])
Train_Labels = data.matrix(traindata[,colnames(traindata) %in% TARGET0]) ##y

Test_Features = data.matrix(hold_out[,!colnames(hold_out) %in% TARGET0])
Test_Labels = data.matrix(hold_out[,colnames(hold_out) %in% TARGET0]) ##y

to_categorical(as.numeric(as.factor(Train_Labels)))[,c(-1)] -> Train_Labels
to_categorical(as.numeric(as.factor(Test_Labels)))[,c(-1)] -> Test_Labels

### set up Keras for MLP

model <- keras_model_sequential()

# Add layers to the model
# c(4), because we have 4 input features
# relu, tanh, elu,linear, selu
model %>% 
  layer_dense(units = 16, activation = 'selu', input_shape = c(ncol(Train_Features))) %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(ncol(Train_Features))) %>%
  layer_dense(units = 4, activation = 'relu', input_shape = c(ncol(Train_Features))) %>%
  layer_dense(units = 2, activation = 'selu') #output

summary(model)

model %>% compile(loss = "categorical_crossentropy",
                  optimizer = optimizer_adagrad(),
                  metrics = c('accuracy')
)

history <- model %>% fit(Train_Features,Train_Labels,
                         validation_split = 0.10,epochs=300,batch_size = 5,shuffle = T)

model %>% evaluate(Test_Features,Test_Labels)

model %>% predict(Test_Features,"raw")

predict_classes(model,Test_Features)

table(predict_classes(model,Test_Features),as.numeric(as.factor(hold_out[,colnames(hold_out) %in% TARGET0]))-1)


###########################correlation#################################

cor(data$data201781$oil,data$data201781$gold_Open)

data201781<-data$data_all[which(data$data_all$date==anal_start):nrow(data$data_all),]

data$stock201781$data$stock_Open
data$gold201781$data$gold_Open
data$silver201781$data$silver_Open
data$oil201781$data$oil
data$btc201781$data$BTC_USD_Open
data$eth201781$data$ETH_USD_Open
data$ltc201781$data$LTC_USD_Open
data$bch201781$data$BCH_USD_Open
data$usdt201781$data$USDT_USD_Open


data$data_cor=merge(data$stock201781$data[c("date","stock_Open")],data$gold201781$data[c("date","gold_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$silver201781$data[c("date","silver_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$oil201781$data[c("date","oil")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$btc201781$data[c("date","BTC_USD_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$eth201781$data[c("date","ETH_USD_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$ltc201781$data[c("date","LTC_USD_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$bch201781$data[c("date","BCH_USD_Open")], by="date", all = TRUE)
data$data_cor=merge(data$data_cor,data$usdt201781$data[c("date","USDT_USD_Open")], by="date", all = TRUE)

data$data_cor=data$data_cor[complete.cases(data$data_cor),!colnames(data$data_cor)%in%"date"]
cor(data$data_cor)


