f = NULL
files = c("AAPL-5.csv","MSFT-5.csv","AMZN-5.csv","IBM-5.csv","INTC-5.csv","GOOG-5.csv","ORCL-5.csv")
for (i in 1:length(files)) {
  csv = read.csv(files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",files[i])
  if (i == 1) f = csv
  else f = merge(f,csv)
}

for (i in 2:ncol(f)) {
  # Price time series of the i-th asset
  prices = f[,i] 
  
  # Price lagged by 1
  prices_prev = c(NA,prices[1:(length(prices)-1)]) 
  
  # Returns time series
  returns = (prices-prices_prev)/prices_prev 
  
  
  
  
  # Replace the i-th column with returns
  f[,i] = returns 
}
# Remove the first row with NAs and the Date column
asset_returns = f[2:nrow(f),2:ncol(f)]



portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns[,i] * x[i]
  }
  
  return (port.returns)
}

sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-(-sharpe(x)+constraint(x)))
}






  
  
  
  
  





