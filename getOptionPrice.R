optionPriceEstimation = function( stock_price=20, maturity=0.25, steps=20,
                                  sigma=0.4, interest=0.03, iterations =1000,
                                  seed=12, callorput="call", strike=20, exotic)
{
  
  s0 = stock_price
  r = interest
  sig = sigma
  T = maturity
  m = steps
  t =  T/m
  
  getStPrice = getStockPrice(s0, T, m , sig, r , iterations, seed)  
  
  stock_price  = getStPrice[[1]] #extracting stock price matrix
  end_price = stock_price[,ncol(stock_price)]
  max_price = apply(stock_price, 1, max)
  min_price = apply(stock_price, 1, min)
  t = T/m #time interval per step
  
  fac = exp(-r*T)
  
  # float lookback option
  if (exotic == "floatlookback") {
    if (callorput == "call") {
      payoff1 = ifelse(end_price > min_price, end_price - min_price, 0)
      floatlookback = fac * payoff1
      option_val = mean(floatlookback)}
    
    else if (callorput == "put") {
      payoff1 = ifelse(end_price < max_price, max_price - end_price, 0)
      floatlookback = fac * payoff1
      option_val = mean(floatlookback)}
  } 
  # Fixed Lookback Option (fixed look back where the payoff depends on the difference between the max/min price and the exercise price)
  if (exotic == "fixedlookback") {
    if (callorput == "call") {
      payoff2 = ifelse(max_price > strike, max_price - strike, 0)
      fixedlookback = fac * payoff2
      option_val = mean(fixedlookback)}
    
    else if (callorput == "put") {
      payoff2 = ifelse(strike > min_price, strike - min_price, 0)
      fixedlookback = fac * payoff2
      option_val = mean(fixedlookback)}
  } 
  
  # Asian arithmetic option
  if (exotic == "asianarithmetic") {
    mean_stk = apply(stock_price, 1, mean)
    if (callorput == "call") {
      payoff3 = ifelse(mean_stk > strike, mean_stk - strike, 0)
      asian_arith = fac * payoff3                 
      option_val = mean(asian_arith)}
    
    else if (callorput == "put") {
      payoff3 = ifelse(strike > mean_stk, strike - mean_stk, 0)    
      asian_arith = fac * payoff3                 
      option_val = mean(asian_arith)}
  } 
  
  # Asian geometric option (the geometric mean is used in the payoff function)
  if (exotic == "asiangeometric") {
    geo_mean = function(x, na.rm=TRUE){
      exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))}
    geo_meanStk = apply(stock_price, 1, geo_mean)
    if (callorput == "call") {
      payoff4 = ifelse(geo_meanStk > strike, geo_meanStk - strike, 0)    
      asian_geo = fac * payoff4                 
      option_val = mean(asian_geo)}
    
    else if (callorput == "put") {
      fac4 = ifelse(strike > geo_meanStk, strike - geo_meanStk, 0)    
      asian_geo = fac * fac4                
      option_val = mean(asian_geo)}
  }
  
  # Asset or nothing option 
  
  if (exotic == "assetornothing") {
    if (callorput == "call") {
      d1 = (log(s0/strike) + (r + sigma^2/2)*T-t) / (sigma*sqrt(T-t))
      option_val = s0 * pnorm(d1)}
    
    else if (callorput == "put") {
      d1 = (log(s0/strike) + (r + sigma^2/2)*T-t) / (sigma*sqrt(T-t))
      option_val = s0 * pnorm(-d1)}
  } 
  
  
  returnlist2 = list(s0, T, steps, sigma, r , iterations, seed, callorput, 
                     strike, exotic, option_val)  
  return(returnlist2)
} 
# The opiton price estimation function will require the below input parameters. 
# We will exectue this to get the desired output.

callorput = list("floatlookback", "fixedlookback", "asianarithmetic", 
                 "asiangeometric", "assetornothing")
exotic = list("call","put")
result = data.frame("Stock_Price", "Time to Maturity", "Steps", 
                    "Standard Deviation", "Interest Rate", "Iterations", 
                    "Seed_Value", "Option", "Strike Price", "Category", 
                    "Option_Price")
for (i in callorput){
  for(j in exotic){
    output = optionPriceEstimation(, , , , , , , j, , i)
    result[nrow(result) + 1,] = output
  }
}
names(result) = NULL

# Final option value with differenct exotic values
head(result)
