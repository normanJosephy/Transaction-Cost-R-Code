# kocinski.R

# 6/10/2012 Modified pricing to use S_{t} rather than S_{T}

# Copied from Rutkowski.R
stock = function(S0=100,u=1.3,d=0.9,nFlips=2) {
  stkList = list()
  for (i in 0:nFlips){
    uIndex = seq(i,0)
    dIndex = seq(0,i)
    stkList[[i+1]] = u^uIndex * d^dIndex * S0
  }
#  names(stkList) = timeNames(nFlips+1)
  return(stkList)
}

f1init = function(s,K,lambda,mu,r) {
  condition1 = (s >= K/(1-mu))
  condition2 = (s >= K/(1+lambda)) && (s < K/(1-mu))
  f1 = 0.0
  if (condition1) {f1 = -K}
  if (condition2) {f1 = (1-mu)/(lambda+mu)*(K - s*(1+lambda))}
  return(f1)
}

f2init = function(s,K,lambda,mu,r) {
  condition1 = (s >= K/(1-mu))
  condition2 = (s >= K/(1+lambda)) && (s < K/(1-mu))
  f2 = 0.0
  if (condition1) {f2 = s}
  if (condition2) {f2 = (1+lambda)/(lambda+mu)*s - K/(lambda+mu)}
  return(f2)
}

c1init = function(s,K,lambda,mu,r) {
  return(max(s-K/(1+lambda),0))
}

c2init = function(s,K,lambda,mu,r) {
  return(max(s-K/(1-mu),0))
}

iterateC = function(cpi,r,lambda,mu,a,b) {
  gamma = (1+lambda)*(1+b) - (1-mu)*(1+a)
  denom = (1+r)*gamma
  A1 = ((1+lambda)*(1+r) - (1-mu)*(1+a))/denom
  B1 = (b-r)*(1-mu)/denom
  A2 = (r-a)*(1+lambda)/denom
  B2 = ((1+lambda)*(1+b) - (1-mu)*(1+r))/denom
  c1 = cpi[[1]]
  c2 = cpi[[2]]
  cp1 = function(s,K,lambda,mu,r) {
    ans = A1*c1(s*(1+b),K,lambda,mu,r) + B1*c2(s*(1+a),K,lambda,mu,r)
    return(ans) }
  cp2 = function(s,K,lambda,mu,r) {
    ans = A2*c1(s*(1+b),K,lambda,mu,r) + B2*c2(s*(1+a),K,lambda,mu,r)
    return(ans) }
  return(list(cp1,cp2))
  }

iterateCDriver = function(K=100,lambda=0.2,mu=1/6,r=0,a=-0.1,b=0.3,nFlips=2) {
  cpnow = list(c1init,c2init)
  listOfcFunctions = list()
  listOfcFunctions[[1]] = cpnow
  for (i in 1:nFlips) {
    cpnext = iterateC(cpnow,r,lambda,mu,a,b)
    listOfcFunctions[[i+1]] = cpnext
    cpnow = cpnext
  }
  invisible(listOfcFunctions)
}

iterateF = function(cpi,r,lambda,mu,a,b){
  gamma = (1+lambda)*(1+b) - (1-mu)*(1+a)
  constant1 = (1-mu)*(1+lambda)/((1+r)*gamma)
  constant2 = -(1+r)/((1+lambda)*(1+b))
  c1 = cpi[[1]]
  c2 = cpi[[2]]
  f1 = function(s,K,lambda,mu,r) {
    value1 = constant1*((1+b)*c2(s*(1+a),K,lambda,mu,r)-(1+a)*c1(s*(1+b),K,lambda,mu,r))
    return(value1) }
  f2 = function(s,K,lambda,mu,r) {
    value2 = constant2*f1(s,K,lambda,mu,r) + c1(s*(1+b),K,lambda,mu,r)/(1+b)
    return(value2)}
  return(list(f1,f2))
}

iterateFDriver = function(K=100,lambda=0.2,mu=1/6,r=0,a=-0.1,b=0.3,nFlips=2) {
  cpList = iterateCDriver(K=K,lambda=lambda,mu=mu,r=r,a=a,b=b,nFlips=nFlips)
  fnow  = list(f1init,f2init)
  listOffFunctions      = list()
  listOffFunctions[[1]] = fnow
  for (i in 1:nFlips) {
    fnext = iterateF(cpList[[i]],r,lambda,mu,a,b)
    listOffFunctions[[i+1]] = fnext
    fnow = fnext
  }
  invisible(listOffFunctions)
}  

setParameters =  function() {
  S0 <<- 100
  K  <<- 100
  lambda <<- 0.2
  mu <<- 1/6
  r  <<- 0
  a  <<- -0.1
  b  <<- 0.3
  nFlips <<- 2
}

applyStockPrice = function(S0=100,K=100,lambda=0.2,mu=1/6,r=0,a=-0.1,b=0.3,nFlips=2) {
  stockTree = stock(S0=S0,u=b+1,d=a+1,nFlips=nFlips)
  revStockTree = rev(stockTree)
#  terminalStockPrices = revStockTree[[1]]
  fList = iterateFDriver(K=K,lambda=lambda,mu=mu,r=r,a=a,b=b,nFlips=nFlips)
  nFFunctions = length(fList)
  for (nf in 1:nFFunctions) {
    stockPrices = revStockTree[[nf]]
    cat("\n f1 for nf=",nf,"\n")   
    fTerminal  = fList[[nf]]
    fTerminal1 = fTerminal[[1]]
    fTerminal2 = fTerminal[[2]]
    f1 = rep(0,length(stockPrices))
    f2 = rep(0,length(stockPrices))
    for (i in 1:length(stockPrices)) {
      stockPrice = stockPrices[i]
      f1[i] = fTerminal1(stockPrice,K,lambda,mu,r)
      f2[i] = fTerminal2(stockPrice,K,lambda,mu,r)
      cat('\n stockPrice:',stockPrice, ' f1, f2:',f1[i],f2[i])
    } # end i loop
    cat("\n\n")
  } # end nf loop
} # end applyStockPrice

