# crr-1.R  

# 6/20/2012 Created runAllTests()

# 6/21/2012 Added unname to riskNeutralProbability()

runAllTests = function() {
  cat('\n\n Running all tests in crr-1.R\n\n')
  testCallOptionPayoff()
  testRiskNeutralProbability()
  testOptionTree()
  teststockQtTree()
  testBonds()
  testPortfolioStockHoldingInDollars()
  cat('\n\n')
}

# call option payoff is max(ST-K,0)
callOptionPayoff = function(stockList,K) {
  ST = stockList[[length(stockList)]]
  payoff = pmax(ST-K,0)
  return(payoff)
}

testCallOptionPayoff = function() {
  stockList = stock(S0=100,u=1.3,d=0.9,nFlips=2)
  K = 100
  payoff = callOptionPayoff(stockList,K)
#  print(payoff)
  return(payoff)
}
  
riskNeutralProbability = function(r,u,d) {
  R = 1 + r
  pu = unname((R - d)/(u - d))
  pd = unname((u - R)/(u - d))
  prob = c(pu=pu,pd=pd)
  return(prob)
}  

testRiskNeutralProbability = function() {
  r=0
  u=1.3
  d=0.9
  print(riskNeutralProbability(r,u,d))
}
 
# stock() defined in rutkowski.R
stock = function(S0,u,d,nFlips) {
  stkList = list()
  for (i in 0:nFlips){
    uIndex = seq(i,0)
    dIndex = seq(0,i)
    stkList[[i+1]] = u^uIndex * d^dIndex * S0
  }
  return(stkList)
}


# payoff is vector computed by callOptionPayoff
optionTree = function(S0,K,u,d,r,nFlips) {
  discountFactor = 1/(1+r)
  stockList = stock(S0,u,d,nFlips)
  callTree = list()
  callTree[[1]] = callOptionPayoff(stockList,K)
  cValues = callTree[[1]]
  probability = riskNeutralProbability(r,u,d)
  pu = unname(probability['pu'])
  pd = unname(probability['pd'])
  for (j in 1:nFlips) {
    nCValues = length(cValues)
    cNew = discountFactor * (pu*cValues[-nCValues] + pd*cValues[-1])
    callTree[[j+1]] = cNew
    cValues = cNew
  }
  return(callTree)
}

testOptionTree = function(nFlips=2) {
  r  = 0
  u  = 1.3
  d  = 0.9
  S0 = 100
  K  = 100
  oTree = optionTree(S0,K,u,d,r,nFlips)
  cat("\n CRR Call option tree \n")
  print(oTree)
  invisible(oTree)
}

# stockQtTree is portfolio stock holding in stock shares
# At expiration, number of shares set to 1 if option value > 0 (in the money)
# If option value = 0, then number of shares set to 0.
stockQtTree = function(callTree,stockTree) {
  # stockTree is in time order; stockTree[[1]] is time 0 stock
  # We need stockTree in reverse order; stockTree[[1]] is time T stock
  # The reversal will then match stockTree with optionTree
  revStockTree = rev(stockTree)
  nFlips = length(callTree) - 1
  dTree = list()
  endValues = ifelse(callTree[[1]] > 0,1,0)
  dTree[[1]] = endValues
  for (j in 1:nFlips) {
    stockV = revStockTree[[j]]
    callV  = callTree[[j]]
    last = length(callV)
    top = callV[-last] - callV[-1]
    bot = stockV[-last] - stockV[-1]
    dTree[[j+1]] = top/bot
  }
  return(dTree)
}


teststockQtTree = function() {
  S0 = 100
  u = 1.3
  d = 0.9
  K = 100
  r = 0
  nFlips = 2
  sTree = stock(S0,u,d,nFlips)
  oTree = optionTree(S0,K,u,d,r,nFlips)
  dTree = stockQtTree(oTree,sTree)
  constants = c(S0=S0,u=u,d=d,K=K,nFlips=nFlips)
  cat("\n\n constants\n")
  print(constants)
  cat("\n\n stock prices (in reverse order) \n")
  print(rev(sTree))
  cat("\n\n option prices\n")
  print(oTree)
  cat("\n\n delta \n")
  for (i in 1:length(dTree)) {
    cat('\n i:',i, 'delta:',dTree[[i]])
  }
  return(dTree)
}

# bondTree is portfolio bond holding in dollars
bonds = function(optionTree,dTree,stockTree) {
  nTime  = length(optionTree)
#  nFlips = nTime - 1
  revStockTree = rev(stockTree)
  bondTree = list()
  for (j in 1:nTime) {
    bondTree[[j]] = optionTree[[j]] - dTree[[j]] * revStockTree[[j]]
  }
  return(bondTree)
}

testBonds = function() {
  S0 = 100
  u = 1.3
  d = 0.9
  K = 100
  r = 0.0
  nFlips = 2
  constants = c(S0=S0,u=u,d=d,K=K,r=r,nFlips=nFlips)
  sTree = stock(S0,u,d,nFlips)
  oTree = optionTree(S0,K,u,d,r,nFlips)
  dTree = stockQtTree(oTree,sTree)
  bTree = bonds(oTree,dTree,sTree)
  cat("\n\n bond tree \n")
  print(bTree)
  invisible(bTree)
}

portfolioStockHoldingInDollars = function(dTree,stockTree) {
  revStockTree = rev(stockTree)
  portfolioStockInDollars = list() 
  for (j in 1:length(dTree)) {
    portfolioStockInDollars[[j]] = dTree[[j]] * revStockTree[[j]]
  }
  return(portfolioStockInDollars)
}

testPortfolioStockHoldingInDollars = function() {
  dTree = teststockQtTree()
  S0 = 100
  u = 1.3
  d = 0.9
  K = 100
  r = 0.0
  nFlips = 2
  constants = c(S0=S0,u=u,d=d,K=K,r=r,nFlips=nFlips)
  sTree = stock(S0,u,d,nFlips)
  psd = portfolioStockHoldingInDollars(dTree=dTree,stockTree=sTree)
  cat("\n\n Portfolio stoch holdings in dollars \n")
  print(psd)
}
