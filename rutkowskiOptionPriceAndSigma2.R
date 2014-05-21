# rutkowskiOptionPriceAndSigma2.R  6/19/2012

# source('rutkowski-2.R')

# 6/21/2012  Added unname to computeBaseUD

# 67/21/2012 Added print statement in computeGridPrice() outer loop for u.

# Uses the following functions from rutkowski-2.R:
# prob(), stock(), computeF1F2(), optionPriceRut()

# h = T/n, where T=time to expiration in years, n=number of flips per year
# sigma = volatility of underlying stock asset

# Changed optionPrice() calls to optionPriceRut() calls 


moreConstantsInWorkspace = function() {
  nFlips <<- 2
  nPtsU  <<- 9
  nPtsD  <<- 9
  dStart <<- 0.80
  dEnd   <<- 0.96
  uStart <<- 1.10
  uEnd   <<- 1.90
  R      <<- 1
  rho    <<- 1
  r      <<- 0
  K      <<- 100
  S0     <<- 100
  lambda <<- 0.2
  mu     <<- 1/6
}

computeBaseUD = function(sigma,h) {
  u = unname(exp(sigma*sqrt(h)))
  d = unname(1/u)
  return(c(d=d,u=u))
}

# compute option price using Rutkowski algorithm
computeBasePrice = function(u,d,lambda=.2,mu=1/6,r=0,
                            S0=100,K=100,nFlips=2){
  p      = prob(lambda,mu,r,u,d)
  phat   = p['phat']
  ptilda = p['ptilda']
  stockList = stock(S0,u,d,nFlips)
  fStored   = computeF1F2(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r)
  price     = optionPriceRut(phat,r,fStored)
  return(unname(price))
}

testComputeBasePrice = function(nFlips=2) {
  sigma = 0.20 # annual
  h     = 1/nFlips  # one year to expiration
  ud    = computeBaseUD(sigma,h)
  price = computeBasePrice(u=ud['u'],d=ud['d'])
  cat("\n\n d,u ",ud,'price',price,'\n\n')
}

computeGridPrice = function(uStart,uEnd,dStart,dEnd,nPtsU,nPtsD,
                            lambda,mu,r,S0,K,nFlips){
  uValues = seq(uStart,uEnd,length=nPtsU)
  dValues = seq(dStart,dEnd,length=nPtsD)
  priceArray = matrix(NA,nrow=nPtsD,ncol=nPtsU)
  cat('\n\n IN computeGridPrice() FOR Rutkowski Contour Computation')
  for (i in 1:nPtsU) { # each column i fixes one u value
    u = uValues[i]
    cat('\n u value number',i,' out of',nPtsU)
    for (j in 1:nPtsD) { # each row j fixes one d value
      d = dValues[j]
      priceArray[j,i] = computeBasePrice(u=u,d=d,lambda=lambda,mu=mu,
                                         r=r,S0=S0,K=K,nFlips=nFlips)
    } # end j loop
  }   # end i loop
  cat('\n-----------------------------------------\n')
  invisible(list(u=uValues,d=dValues,c=priceArray))
}

# dL computed and returned by function computeGridPrice()
getContourRutkowski = function(dL,S0=100,oPrice=30) {
  # NO SCALING IN COMPUTATION OF c.
  cLines = contourLines(dL$u,dL$d,dL$c,levels=oPrice)
  if (length(cLines) == 0) {stop('No Rutkowski contour. Exiting program')
  } else {return(cbind(u=cLines[[1]]$x,d=cLines[[1]]$y))}
}


testGetContourRutkowski= function() {
  uStart = 1.05
  uEnd   = 1.70
  dStart = 0.80
  dEnd   = 0.96
  nPtsU  = 100
  nPtsD  = 100
  lambda = 0.2
  mu     = 1/6
  r      = 0.0
  S0     = 100
  K      = 100
  oPrice = 30
  nFlips = 2
  dL = computeGridPrice(uStart,uEnd,dStart,dEnd,nPtsU,nPtsD,
                                    lambda,mu,r,S0,K,nFlips)
  rutkowskiContour = getContourRutkowski(dL=dL,S0=S0,oPrice=oPrice)
  plot(rutkowskiContour[,'d'],rutkowskiContour[,'u'],type='l',xlab='d',ylab='u',
       main=paste('Rutkowski Contour For Option Price',oPrice))
  invisible(rutkowskiContour)
}
