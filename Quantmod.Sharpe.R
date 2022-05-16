library(quantmod)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod
getSymbols("BTC-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("ETH-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)



getSymbols("ADA-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("LINK-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("BNB-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)

getSymbols("XRP-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("SOL-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("LTC-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("LUNA-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
getSymbols("DOGE-USD", from = '2020-01-01',
           to = "2022-03-01",warnings = FALSE,
           auto.assign = TRUE)
merged.data <- merge(`BTC-USD`, `ETH-USD`, `ADA-USD`,`LINK-USD`,`BNB-USD`,`XRP-USD`,`SOL-USD`,`LTC-USD`,`LUNA-USD`,`DOGE-USD`)



#merged.data %>% select(BTC.USD-Close)
#merged.data<- dplyr::select ( merged.data,BTC-USD.Close,ETH-USD.Close)
merged.data7 <- merged.data[ , c("BTC.USD.Close","ETH.USD.Close","ADA.USD.Close","LINK.USD.Close","BNB.USD.Close","XRP.USD.Close","SOL.USD.Close","LTC.USD.Close","LUNA.USD.Close","DOGE.USD.Close")]
merged.data2 <- na.omit(merged.data7)


###
for (i in 1:ncol(merged.data2)) {
  # Price time series of the i-th asset
  prices = merged.data2[,i]
  
  # Price lagged by 1
  prices_prev = c(NA,prices[1:(length(prices)-1)])
  
  # Returns time series
  returns = (prices-prices_prev)/prices_prev
  
  # Replace the i-th column with returns
  merged.data2[,i] = returns
}



asset_returns = merged.data2[2:nrow(merged.data2),1:ncol(merged.data2)]


# creating function to split data into train and test



train_test <- function(asset_returns, size = 0.8, train = TRUE) {
 train_row = size * nrow(asset_returns)
  test_set <- 1: train_row
  if (test == TRUE) {
    return (asset_returns[train_set, ])
  } else {
    return (asset_returns[test_set, ])
  }
}





asset_returns_train <<- train(asset_returns, 0.8, train = TRUE)
asset_returns_test <<- test_row(asset_returns, 0.2, test  = TRUE)




#calculation Portfolio Returns



portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns_train[,i] * x[i]
  }
  
  return (port.returns)
}



#Objective function with penalty
sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}
#Now we have to write the code for the penalty function



constraint = function(x) {
  boundary_constr = (sum(x)-1)**2 # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr +
      max(c(0,x[i]-1))**2 + # "x <= 1" constraint
      max(c(0,-x[i]))**2 # "x >= 0" constraint
  }
  
  return (boundary_constr)
}



#coding of final sharpe ratio



obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}





