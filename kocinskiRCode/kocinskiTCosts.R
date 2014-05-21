# kocinskiTCosts.R 

nShares = function(fList,stockList) {
  # Compute number of shares in each post-rebalance portfolio
  f22 = fList$f22
  f21 = fList$f21
  f2 = fList$f2
  # t = 0
  stockPrices = stockList[[1]]
  nPrices     = length(stockPrices)
  nShares22   = rep(0,nPrices)
  for (i in 1:nPrices){
    stockPrice   = stockPrices[i]
    nShares22[i] = f22(stockPrice)/stockPrice
  }        
  # t = 1
  stockPrices = stockList[[2]]
  nPrices     = length(stockPrices)
  nShares21   = rep(0,nPrices)
  for (i in 1:nPrices) {
    stockPrice   = stockPrices[i]
    nShares21[i] = f21(stockPrice)/stockPrice
  }
  # t = 2
  stockPrices = stockList[[3]]
  nPrices     = length(stockPrices)
  nShares2    = rep(0,nPrices )
  for (i in 1:nPrices) {
    stockPrice   = stockPrices[i]
    nShares2[i]  = f2(stockPrice)/stockPrice
  }
  nShares = list(nShares22=nShares22,nShares21=nShares21,nShares2=nShares2)
  return(nShares)
} 

testNShares = function() {
  constants    = assignConstants()
  combinedList = fIterate(constants)
  fList        = combinedList$fList
  S0 = constants['S0']
  u  = 1 + constants['b']
  d  = 1 + constants['a']
  nFlips       = constants['nFlips']
  stockList    = stock(S0,u,d,nFlips)
  nSharesList  = nShares(fList,stockList)
  nTimes       = length(nSharesList)
  for (iTime in 1:nTimes) {
    stockPrices    = stockList[[iTime]]
    nSharesValues  = nSharesList[[iTime]]
    nPrices        = length(stockPrices)
    for (iPrice in 1:nPrices) {
      cat('\n stock price',stockPrices[iPrice],'nShares',nSharesValues[iPrice])
    }
    cat('\n\n')
  }
  invisible(list(stockList=stockList,nSharesList=nSharesList,constants=constants))
}

stockValueChange = function(nSharesList,stockList,lambda,mu) {
  # compute change in number of shares * stock price
  nShares2  = nSharesList$nShares2
  nShares21 = nSharesList$nShares21
  nShares22 = nSharesList$nShares22
  # t = 0
  stockPrices = stockList[[1]]
  nPrices = length(stockPrices)
  stopifnot(nPrices == 1)
  stockChanges1   = rep(0,1)
  nSharesChanges1 = rep(0,1)
  tCostValue1     = rep(0,1)
  for (i in 1:nPrices) {
    stockPrice = stockPrices[i]
    stockChanges1[i]   = nShares22[i]*stockPrice
    tCostValue1[i]     = tCost(stockChanges1[i],lambda,mu)
    nSharesChanges1[i] = nShares22[i]
  }
  # t = 1
  stockPrices = stockList[[2]]
  nPrices     = length(stockPrices)
  # Length of stockChanges goes up exponentially, e.g. length= 1,2,4 for t=0,1,2
  # There will be problems coding this for the general nFlips case.
  # The stockChanges is path dependent, so we will need a 
  # non-recombinant tree with all 2^n branches.
  stopifnot(nPrices == 2)
  stockChanges2   = rep(0,2)
  nSharesChanges2 = rep(0,2)
  tCostValue2     = rep(0,2)
  nSharesT0   = nShares22[1]
  for (i in 1:nPrices) {
    stockPrice         = stockPrices[i]
    nSharesChanges2[i] = nShares21[i] - nSharesT0
    stockChanges2[i]   = (nShares21[i] - nSharesT0) * stockPrice
    tCostValue2[i]     = tCost(stockChanges2[i],lambda,mu)
  }
  # t = 2
  stockPrices = stockList[[3]]
  nPrices = length(stockPrices)
  stopifnot(nPrices == 3)
  stockChanges3   = rep(0,4)
  nSharesChanges3 = rep(0,4)
  tCostValue3     = rep(0,4)
  # node (2,2)
  nSharesChanges3[1] = nShares2[1] - nShares21[1]
  stockChanges3[1]   = (nShares2[1] - nShares21[1]) * stockPrices[1]
  tCostValue3[1]     = tCost(stockChanges3[1],lambda,mu)
  # node (2,1)
  nSharesChanges3[2] = nShares2[2] - nShares21[1]
  stockChanges3[2]   = (nShares2[2] - nShares21[1]) * stockPrices[2]
  tCostValue3[2]     = tCost(stockChanges3[2],lambda,mu)
  nSharesChanges3[3] = nShares2[2] - nShares21[2]
  stockChanges3[3]   = (nShares2[2] - nShares21[2]) * stockPrices[2]
  tCostValue3[3]     = tCost(stockChanges3[3],lambda,mu)
  # node (2,0)
  nSharesChanges3[4] = nShares2[3] - nShares21[2]
  stockChanges3[4]   = (nShares2[3] - nShares21[2]) * stockPrices[3]
  tCostValue3[4]     = tCost(stockChanges3[4],lambda,mu)
  stockChangesList = list(stockChanges1=stockChanges1,
                          stockChanges2=stockChanges2,
                          stockChanges3=stockChanges3,
                          nSharesChanges1=nSharesChanges1,
                          nSharesChanges2=nSharesChanges2,
                          nSharesChanges3=nSharesChanges3,
                          tCostValue1=tCostValue1,
                          tCostValue2=tCostValue2,
                          tCostValue3=tCostValue3)
  return(stockChangesList)
}

tCost = function(x,lambda,mu) {
  if (x >= 0) ans = x*lambda
  if (x < 0 ) ans = - x*mu
  return(ans)
}

testStockChanges = function() {
  constants    = assignConstants()
  combinedList = fIterate(constants)
  fList        = combinedList$fList
  S0 = constants['S0']
  u  = 1 + constants['b']
  d  = 1 + constants['a']
  lambda = constants['lambda']
  mu     = constants['mu']
  nFlips       = constants['nFlips']
  stockList    = stock(S0,u,d,nFlips)
  nSharesList  = nShares(fList,stockList)
  stockVC      = stockValueChange(nSharesList,stockList,lambda,mu)
  stockChanges1=stockVC$stockChanges1
  stockChanges2=stockVC$stockChanges2
  stockChanges3=stockVC$stockChanges3
  nSharesChanges1=stockVC$nSharesChanges1
  nSharesChanges2=stockVC$nSharesChanges2
  nSharesChanges3=stockVC$nSharesChanges3
  tCostValue1=stockVC$tCostValue1
  tCostValue2=stockVC$tCostValue2
  tCostValue3=stockVC$tCostValue3
  cat('\n\n')
  cat("stock prices",stockList[[1]],'\n')
  cat("change in nShares ",nSharesChanges1,'\n')
  cat("change in stock value ",stockChanges1,'\n')
  cat("transaction costs ",tCostValue1)
  cat("\n\n")
  cat("stock prices",stockList[[2]],'\n')
  cat("change in nShares ",nSharesChanges2,'\n')
  cat("change in stock value ",stockChanges2,'\n')
  cat("transaction costs ",tCostValue2)
  cat("\n\n")
  cat("stock prices",stockList[[3]],'\n')
  cat("change in nShares ",nSharesChanges3,'\n')
  cat("change in stock value ",stockChanges3,'\n')
  cat("transaction costs ",tCostValue3)
#   for (iTime in 1:nTimes) {
#     stockPrices = stockList[[iTime]]
#     sChanges    = stockVC[c(iTime,iTime+3)]
#     cat(' stock Prices\n')
#     print(stockPrices)
#     cat("\n Changes in stock value and stock number of shares\n")
#     print(sChanges)
# #     nStates     = length(stockPrices)
# #     for (iState in 1:nStates) {
# #       stockPrice = stockPrices[iState]
# #       sChange    = sChanges[iState]
# #       cat('\n stock price',stockPrice,'stock changes',sChange)
# #     } # end loop iState
#     cat('\n')
#   }   # end loop iTime
}
