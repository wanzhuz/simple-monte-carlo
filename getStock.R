getStockPrice = function(stock_price=20, maturity=0.25, steps=20, sigma=0.4,
                         interest=0.03, iterations=1000, seed=12){
  s0 = stock_price
  r = interest
  sig = sigma
  T = maturity
  m = steps
  t = T/m
  
  n = iterations
  nvars = m*n
  set.seed(seed)
  e = as.vector(rnorm(n=nvars, m=0, sd=1))
  E = matrix(e, nrow=n, ncol=m)
  head(E, n=2)
  
  f = exp((r-0.5*sig^2)*t + sig*sqrt(t)*E)
  head(f, n=1)
  
  # cumulates returns
  f1 = t(apply(f, 1, cumprod))
  head(f1, n=1)
  
  st = f1*s0 # returns * price
  head(st, n=1)
  
  mean_st = apply(st, 1, mean)
  end_price = st[, ncol(st)]
  end_mean = mean(end_price)
  
  max_price = apply(st, 1, max)
  max_mean = mean(max_price)
  
  min_price = apply(st, 1, min)
  min_mean = mean(min_price)
  
  final = list(st, end_mean, max_mean, min_mean)
  return(final)
}

# first 2 price paths
price_list = getStockPrice()
head(price_list[[1]], 2)

# end price
print(price_list[[2]])

# max mean
print(price_list[[3]])

# min mean
print(price_list[[4]])
