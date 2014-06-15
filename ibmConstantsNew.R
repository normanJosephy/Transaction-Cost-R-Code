#  ibmConstantsNew.R

# This file replaces inputParametersNoGUI.R

source("createPathsFromIBMPricesUpdatedNoGUI.R")
source("createDeltaRutkowskiNewNoGUI.R")
source("rutkowski-2.R")
source("createRutkowskiContourNewNoGUI.R")


ibmConstantsNew = function() {
  myEnv = new.env()
  with(myEnv,{
  WD = 'C:/Norm/Research/Transaction-Cost-R-Code/data/'
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
  nPtsU=50; nPtsD=50
  })
  assign("myEnv",myEnv,envir=.GlobalEnv)
  return(myEnv)}

testEnvironment = function() {
  myEnv = ibmConstantsNew()
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

