#  inputParametersNoGUI.R

source("createPathsFromIBMPricesUpdatedNoGUI.R")
source("createDeltaRutkowskiNewNoGUI.R")
source("rutkowski-2.R")
source("createRutkowskiContourNewNoGUI.R")


ibmConstants = function() {
  myEnv = new.env()
  WD = 'c:/Research/Lucy-2014/Transaction-Cost-R-Code/data/'
  FN = paste(WD,'IBMData2014.Rdata',sep='')
  runNumber = 701
  # Option data
  nDaysInYear = 252
  S0          = 193.15       # stock price at time option price is noted.
  K           = 190          # call option strike price
  optionPrice = 7.00         # option price
  nFlips      = 6            # number of rebalancing times.
  TimeToExpiration = 1       # time to expiration in years
  R           = K/S0         # normalized strike - used in CRR contour computation
  #
  nPaths = 100        # number of paths constructed 
  nNewPointsOnPath = 6
  lambda = 0.01       # unit transaction cost for buying a share of stock.
  mu     = 0.01      # unit transaction cost for selling a share of stock. 
  nUDPairsToUse = 50     # number of contour (u,d) pairs to use in algorithm.
  #  
  rAnnual = 0.00
  r      = rAnnual/nFlips # single period risk-free interest rate
  rho    = 1 + r        # risk-free single period accumulation factor
  nPtsD  = 100          # number of u values used in computing (u,d,c) surface.
  nPtsU  = 100          # number of d values used in computing (u,d,c) surface.
  seed   = 12345        # seed set in rutkowskiSimulation()
  drift  = 0.00  
  sigma  = 0.10
  percentCRR = 0  
  #
  uStart=1.01; uEnd=1.20
  dStart=0.80; dEnd=0.99
  nPtsU=20; nPtsD=20
  assign('uStart',uStart,envir=myEnv)
  assign('uEnd',uEnd,envir=myEnv)
  assign('dStart',dStart,envir=myEnv)
  assign('dEnd',dEnd,envir=myEnv)
  assign('nPtsU',nPtsU,envir=myEnv)
  assign('nPtsD',nPtsD,envir=myEnv)
  assign('WD',WD,envir=myEnv)
  assign('FN',FN,envir=myEnv)
#  setwd(dataDir)
  assign('runNumber' , runNumber ,envir=myEnv)
  # Option data
  assign('nDaysInYear' , nDaysInYear,envir=myEnv)
  assign('S0' , S0,envir=myEnv)      # stock price at time option price is noted.
  assign('K'  , K,envir=myEnv)          # call option strike price
  assign('optionPrice', optionPrice,envir=myEnv)         # option price
  assign('nFlips' , nFlips,envir=myEnv)            # number of rebalancing times.
  assign('TimeToExpiration', TimeToExpiration,envir=myEnv)     # time to expiration in years
  assign('R'  ,R,envir=myEnv)         # normalized strike - used in CRR contour computation
  #
  assign('nPaths', nPaths,envir=myEnv)        # number of paths constructed 
  assign('nNewPointsOnPath',nNewPointsOnPath,envir=myEnv)
  assign('lambda', lambda,envir=myEnv)       # unit transaction cost for buying a share of stock.
  assign('mu', mu,envir=myEnv)      # unit transaction cost for selling a share of stock. 
  assign('nUDPairsToUse', nUDPairsToUse,envir=myEnv)     # number of contour (u,d) pairs to use in algorithm.
  #  
  assign('rAnnual', rAnnual,envir=myEnv)
  assign('r', r,envir=myEnv) # single period risk-free interest rate
  assign('rho', rho,envir=myEnv)        # risk-free single period accumulation factor
  assign('nPtsD', nPtsD,envir=myEnv)          # number of u values used in computing (u,d,c) surface.
  assign('nPtsU', nPtsU,envir=myEnv)          # number of d values used in computing (u,d,c) surface.
  assign('seed', seed,envir=myEnv)        # seed set in rutkowskiSimulation()
  assign('drift', drift,envir=myEnv)         # annual stock drift
  assign('sigma',sigma,envir=myEnv)
  #
  assign('percentCRR', percentCRR,envir=myEnv)        # (1+percentage)*CRR is initial capital
  assign("myEnv",myEnv,envir=.GlobalEnv)
  return(myEnv)}
#   constants  = list(nPaths=nPaths,TimeToExpiration=TimeToExpiration,
#                  percentCRR=percentCRR,optionPrice=optionPrice,
#                  S0=S0,K=K,R=R,seed=seed,nDaysInYear=nDaysInYear,
#                  lambda=lambda,mu=mu,nFlips=nFlips,r=r,rho=rho,
#                  rAnnual=rAnnual,runNumber=runNumber,
#                  nPtsD=nPtsD,nPtsU=nPtsU,nUDPairsToUse=nUDPairsToUse)
# #  fileName = paste('run',runNumber,'constants.Rdata',sep='')
# #  save(constants,file=fileName)
# #  cat("\n constants saved to file",fileName,'\n\n')
#   return(constants)
# }

testEnvironment = function() {
  myEnv = ibmConstants()
  print(ls(name = myEnv))
  return(myEnv)
}

# Copied from PBSmodelling package.
# Modified to unpack an environment.
# Also works for unpacking a list
unpackList = function (x) {
  namx <- ls(x)
  nx <- length(namx)
    for (i in 1:nx) {
          assign(namx[i], x[[namx[i]]], pos = parent.frame(1))
    }
}

# Copies the contents of a list into an environment.
# Also copies contents of an environment (myList) into an environment (myEnvironment)
packListToEnvironment = function(myList, myEnvironment) {
  nVars = length(myList)
  nameVars = ls(myList)
  for (i in 1:nVars) {
    assign(nameVars[i],myList[[ls(myList)[i]]],myEnvironment)
  }
  invisible(myEnvironment)
}
