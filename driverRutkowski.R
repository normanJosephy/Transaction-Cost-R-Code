# driverRutkowski.R 

setup = function() {
  source(file='rutkowski-2.R')
  source(file='rutkowskiOptionPriceAndSigma2.R')
  cat("\n\n Sourced rutkowski-2.R and rutkowskiOptionPriceAndSigma2.R\n\n")
}

driverRutkowskiIter = function() {
# Set fixed parameters
  S0 = 100
  lambda  =  0.01
  mu      =  0.01
  r       =  0.0
  u       = (1 + lambda)*(1 + r)
  d       = (1 - mu)*(1 + r)
  KValues = seq(80,120,by=5)
  flipValues  =  c(6,13,26)
  nKValues = length(KValues)
  nflipValues = length(flipValues)
  # Storage for prices
  c0 = matrix(0,nrow=nKValues,ncol=nflipValues)
  colnames(c0) = paste('nFlips=',flipValues,sep='')
  rownames(c0) = paste('strike=',KValues,sep='')
  # Compute probability
  p         = prob(lambda,mu,r,u,d)
  phat      = p['phat']
  ptilda    = p['ptilda']
  # loop over strikes for each of nFlips = 6,13,26
  for (iFlips in 1:nflipValues) {
    nFlips = flipValues[iFlips] 
    for (kValue in 1:nKValues) {
      K         = KValues[kValue] 
      stockList = stock(S0,u,d,nFlips)
      fStored   = computeF1F2(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r)
      price     = optionPriceRut(phat,r,fStored)
      c0[kValue,iFlips] = price
      }
  }
  # hg        = HG(mu,lambda,fStored)  
  #  costList  = tCostRutkowski(hg,stockList,r)
#   output    = list(constants=constants,p=p,stockList=stockList,
#                    fStored=fStored,price=price,hg=hg)
  invisible(c0)
}
# rutkowskiDelta = function(path,H,G,lambda=0.2,mu=1/6,r=0)

plotLowerBound = function(c0) {
  flipValues  =  c(6,13,26)
  KValues = seq(80,120,by=5)
  matplot(KValues,c0,
          type='l',
          col=c(1,2,3),
          lwd=2,
          xlab='Strike',
          ylab='Lower Bound',
          main='Lower Bound')
  legend(x=108,y=21,text.width=6,
         col=c(1,2,3),lwd=2,lty=1,
         legend=flipValues,cex=0.8,
         title='Rebalances')
  grid()
  points(x=c(100,100,100,90,110),
         y=c(6.88,7.31,7.1,12.43,6),
         col=c(1,2,3,1,1),
         pch=19,cex=1.2)
  text(x=c(96,109,116),cex=.8, 
       y=c(12.5,7.3,6),
       labels=c('Case 13','Cases 10-12','Case 14'))
}
