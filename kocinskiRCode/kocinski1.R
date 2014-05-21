# kocinski1.R

# source('kocinskiTCosts.R')

stock = function(S0,u,d,nFlips) {
  stkList = list()
  for (i in 0:nFlips){
    uIndex = seq(i,0)
    dIndex = seq(0,i)
    stkList[[i+1]] = u^uIndex * d^dIndex * S0
  }
#  names(stkList) = timeNames(nFlips+1)
  return(stkList)
}


assignConstants = function(){
  # Rutkinski example parameter values
  lambda  = 0.20
  mu      = 1/6
  r       = 0
  a       = -0.1
  b       = 0.3
  S0      = 100
  K       = 100
  nFlips  = 2
  # Computed constants
  gammaV  = (1+lambda)*(1+b) - (1-mu)*(1+a)
  denom   = (1+r)*gammaV
  A1      = ((1+lambda)*(1+r) - (1-mu)*(1+a))/denom
  B1      = (b-r)*(1-mu)/denom
  A2      = (r-a)*(1+lambda)/denom
  B2      = ((1+lambda)*(1+b) - (1-mu)*(1+r))/denom
  constants = c(lambda=lambda,mu=mu,r=r,a=a,b=b,S0=S0,K=K,
                gammaV=gammaV,denom=denom,nFlips=nFlips,
                A1=A1,B1=B1,A2=A2,B2=B2)
#  constants <<- constants
  return(constants)
}

fInit = function(constants) {
  K       = unname(constants['K'])
  lambda  = unname(constants['lambda'])
  mu      = unname(constants['mu'])
  sTop    = K/(1-mu)
  sBot    = K/(1+lambda)
  # f1
  intercept1  = K*(1-mu)/(lambda+mu)
  slope1      = -(1+lambda)*(1-mu)/(lambda+mu)
  f1 = function(s) {
    ans = 0
    if (s >= sTop) ans = -K
    if ((s < sTop) && (s >= sBot)) ans = slope1*s + intercept1
    return(unname(ans))
  }
  # f2
  intercept2  = -K/(lambda+mu)
  slope2      = (1+lambda)/(lambda+mu)
  f2 = function(s) {
    ans = 0
    if (s >= sTop) ans = s
    if ((s < sTop) && (s >= sBot)) ans = slope2*s + intercept2
    return(unname(ans))
  }
#   cat("\n\t fInit")
#   cat("\n\n f1 slope:",slope1," intercept",intercept1,'\n\n')
#   cat(" f2 slope:",slope2," intercept",intercept2,'\n\n')
#   cat('sTop, sBot:',sTop,sBot,'\n\n')
  invisible(list(f1=f1,f2=f2))
}

testfInit = function() {
  constants = assignConstants()
  f = fInit(constants)
  f1 = f$f1
  f2 = f$f2
  cat("\n f1 values \n",f1(169),f1(117),f1(81))
  cat('\n f2 values \n',f2(169),f2(117),f2(81),'\n\n')
}

cInit = function(constants) {
  K      = unname(constants['K'])
  lambda = unname(constants['lambda'])
  mu     = unname(constants['mu'])
  cf1 = function(s) {
    ans = 0
    if (s >= K/(1+lambda)) ans = s - K/(1+lambda)
    return(unname(ans))  
  }
  cf2 = function(s) {
    ans = 0
    if (s >= K/(1-mu)) ans = s - K/(1-mu)
    return(unname(ans))
  }
#   cat('\n\n cf1 break, cf2 break ',K/(1+lambda),K/(1-mu))
#   cat("\n\n cf1 values ",cf1(169),cf1(117),cf1(81))
#   cat("\n\n cf2 values ",cf2(169),cf2(117),cf2(81))
#   cat('\n\n')
  invisible(list(cf1=cf1,cf2=cf2))
}

cIterate = function(constants) {
  A1 = constants['A1']
  B1 = constants['B1']
  A2 = constants['A2']
  B2 = constants['B2']
  a  = constants['a']
  b  = constants['b']
  cInitial = cInit(constants)
  cf1 = cInitial[['cf1']]
  cf2 = cInitial[['cf2']]
  # cf11 is first iterate of cf1
  # cf21 is first iterate of cf2
  # cf11
  cf11 = function(s) {
    ans = A1 * cf1((1+b)*s) + B1 * cf2((1+a)*s)
    return(unname(ans))
  }
  # cf21
  cf21 = function(s) {
    ans = A2 * cf1((1+b)*s) + B2 * cf2((1+a)*s)
    return(unname(ans))
  }
  # cf12 is second iterate of cf1
  # cf22 is second iterate of cf2
  # cf12
  cf12 = function(s) {
    ans = A1 * cf11((1+b)*s) + B1 * cf21((1+a)*s)
    return(unname(ans))
  }
  cf22 = function(s) {
    ans = A2 * cf11((1+b)*s) + B2 * cf21((1+a)*s)
    return(unname(ans))
  }
  invisible(list(cf11=cf11,cf21=cf21,cf12=cf12,cf22=cf22))
}

byHandCfIterateTwice = function(s,constants) {
  A1 = constants['A1']
  B1 = constants['B1']
  a  = constants['a']
  b  = constants['b']
  mu = constants['mu']
  term1 = pmax(1.69*s - 100*(1-mu),0)
  term2 = pmax(1.17*s - 120,0)
  term3 = pmax(1.17*s - 100*(1-mu),0)
  term4 = pmax(.81*s-120,0)
  ans1  = .30864*term1 + .17147*term2 + .04572*term3 + .27689*term4
  ans2  = .08230*term1 + .04572*term2 + .13291*term3 + .80482*term4
  # compare to cf12 computed by cIterate
  cF = cIterate(assignConstants())
  cf12 = cF[['cf12']]
  cf22 = cF[['cf22']]
  for (ss in 1:length(s)) {
    cat('stock',s[ss],'cf12 value by hand',ans1[ss],'by cIterate',cf12(s[ss]),'\n')
  }
  cat("\n\n")
  for (ss in 1:length(s)) {
    cat('stock',s[ss],'cf22 value by hand',ans2[ss],'by cIterate',cf22(s[ss]),'\n')
  }  
  cat('\n\n')
}


fIterate = function(constants) {
  lambda = constants['lambda']
  mu     = constants['mu']
  r      = constants['r']
  gammaV = constants['gammaV']
  a      = constants['a']
  b      = constants['b']
  cfList = cInit(constants)
  cf1    = cfList$cf1
  cf2    = cfList$cf2
  cF     = cIterate(constants)
  cf11   = cF$cf11
  cf21   = cF$cf21
  cf12   = cF$cf12
  cf22   = cF$cf22
  coeff1 = (1-mu)*(1+lambda)/((1+r)*gammaV)
  coeff2 = -(1+r)/((1+lambda)*(1+b))
  f11 = function(s) {
    term   = (1+b)*cf2((1+a)*s) - (1+a)*cf1((1+b)*s)
    ans    = coeff1*term
    return(ans)
  }
  f21 = function(s) {
    ans    = coeff2*f11(s) + 1/(1+b)*cf1((1+b)*s)
    return(ans)
  }
  f12 = function(s) {
    term   = (1+b)*cf21((1+a)*s) - (1+a)*cf11((1+b)*s)
    ans    = coeff1 * term
    return(ans)
  }
  f22 = function(s) {
    ans    = coeff2*f12(s) + 1/(1+b)*cf11((1+b)*s)
    return(ans)
  }
  f        = fInit(constants)
  f1       = f$f1
  f2       = f$f2
  fList = list(f1=f1,f2=f2,f11=f11,f21=f21,f12=f12,f22=f22)
  cList = list(cf1=cf1,cf2=cf2,cf11=cf11,cf21=cf21,cf12=cf12,cf22=cf22)
  return(list(fList=fList,cList=cList))
}

evalF = function(constants,combinedList) {
  fList = combinedList$fList
  cList = combinedList$cList
  S0 = constants['S0']
  u  = 1 + constants['b']
  d  = 1 + constants['a']
  nFlips = constants['nFlips']
  stockList = stock(S0,u,d,nFlips)
  revStock = rev(stockList)
  # expiration
  stockPrices = revStock[[1]]
  for (stockPrice in stockPrices) {
    f1Value = fList$f1(stockPrice)
    f2Value = fList$f2(stockPrice)
    nShares = f2Value/stockPrice
    cat("\n stock price",stockPrice,"f1",f1Value,"f2",f2Value,"nShares",nShares,'\n')
  }
  # expiration - 1
  stockPrices = revStock[[2]]
  for (stockPrice in stockPrices) {
    f11Value = fList$f11(stockPrice)
    f21Value = fList$f21(stockPrice)
    nShares  = f21Value/stockPrice
    cat("\n stock price", stockPrice, 'f11',f11Value,'f21',f21Value,"nShares",nShares,'\n')
  }
  # expiration - 2
  stockPrices = revStock[[3]]
  for (stockPrice in stockPrices) {
    f12Value = fList$f12(stockPrice)
    f22Value = fList$f22(stockPrice)
    nShares  = f22Value/stockPrice
    cat("\n stock price",stockPrice,'f12',f12Value,'f22',f22Value,"nShares",nShares,'\n')
  }
  cat("\n\n")
}

testEvalF = function() {
  constants = assignConstants()
  combinedList = fIterate(constants)
  evalF(constants,combinedList)
  invisible(combinedList)
}

byHandF = function() {
  # f11,f21 at expiration - 1 with stock prices 130, 90
  fHand11 = function(s) {
    ans = 1.60494 * pmax(.9*s - 120,0) - 1.11111* pmax(1.3*s - 83.33333,0)
    return(ans)
  }
  fHand21 = function(s) {
    term1 = 1.3*pmax(.9*s - 120,0) - 0.9*pmax(1.3*s - 83.33333,0)
    ans = -0.79139*term1 + 10/13*pmax(1.3*s - 83.33333,0)
    return(ans)
  }
  cat("\n by hand \n")
  cat("\n f11 at s=130,90 ",fHand11(c(130,90)))
  cat("\n f21 at s=130,90 ",fHand21(c(130,90)))
  # 
  fHand12 = function(s) {
    term1 = 0.23777 * pmax(1.17*s - 83.3333,0) 
    term2 = 1.43983 * pmax(0.81*s - 120,0) 
    term3 = 0.61728 * pmax(1.69*s - 83.3333,0)
    term4 = 0.34294 * pmax(1.17*s - 120,0)
    ans = term1 + term2 - term3 - term4
    return(ans)
  }
  fHand22 = function(s) {
    term1 = -0.64103  * fHand12(s)
    term2 = 10/13 * 5/9 * pmax(1.69*s - 83.3333,0)
    term3 = 10/13 *25/81* pmax(1.17*s - 120,0)
    ans = term1 + term2 + term3
    return(ans)
  }
  cat("\n by hand \n")
  cat('\n f12 at s=100',fHand12(100))
  cat('\n f22 at s=100',fHand22(100))
  cat('\n\n')
}

