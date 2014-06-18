# createPathsFromIBMPricesUpdatedNoGUI.R  6-14-2014 

# 7/24/2012 Changed computation of S0. It was last(ibm), which, at 195.58,
#           would put the option with K=190 in the money.
#           It currently is set by user, which should be set at the
#           stock price at the time the option was priced, which is 
#           190.65, as set in constants vector.
#
#           In volatilityOneCompany(), changed default nDaysInYear from
#           length of stock price time series to 252. 
#           Actual value used comes from constants vector passed in.
#
# 6/9/2014 updated ibm data to be stored in 
#   c:/Research/Lucy-2014/Transaction-Cost-R-Code

# 6/11/2014 Changed createPathsAndJumpsFromIBMData() to create
#           paths based on training period ibm data and use
#           ibm value at first value in actualPath as S0 
#           The actualPath is ibm[(n-nNewPts):n], where
#           n = length of ibm time series, and S0=ibm[n-nNewPts]

computeDailyJumps = function(priceData) {
    if ('zoo' %in% class(priceData)) {priceData = coredata(priceData)}
    jumps = priceData[-1]/priceData[-length(priceData)]
    invisible(jumps)
    }


# nNewPointsOnPath is one less than total prices 
# (S0 appended to front of paths)
# Changed return output to just be paths and not also jumps.
createDailyPathsFromJumps = function(jumps,S0,nPaths=100,nNewPointsOnPath=6) {
    sampledJumpsVector = sample(jumps,nPaths*nNewPointsOnPath,replace=TRUE)
    sampledJumps       = matrix(sampledJumpsVector,
                            nrow=nNewPointsOnPath,ncol=nPaths)
    X                  = rbind(S0,sampledJumps)
    simulatedPaths     = apply(X,2,cumprod)
    invisible(simulatedPaths)
    }

# computedEnv created by ibmConstantsNew()

createPathsAndJumpsFromIBMData = function() {
    unpackList(myEnv)
    fileName = FN
    load(fileName)
    actualPathStartingValueAt = length(stockPrices)-nNewPointsOnPath
    S0 = coredata(stockPrices)[actualPathStartingValueAt]
    myEnv$S0 = S0
    cat("\n #############\n  Changed initial S0 to ",S0,'in myEnv\n #########\n\n')
    #
    # Use entire stockPrices time series to construct jump population
    jumps = computeDailyJumps(priceData=stockPrices)
    paths = createDailyPathsFromJumps(jumps=jumps,
        S0=S0,nPaths=nPaths,nNewPointsOnPath=nNewPointsOnPath)
    actualPath = computeActualPath(stockPrices,paths)
    outputList = list(paths=paths,actualPath=actualPath,S0=S0)
    packListToEnvironment(outputList,computedEnv)
        invisible(outputList)
    }

# Actual path is testing path, the last nPtsOnNewPaths values of ibm.

computeActualPath = function(stockPrices,paths){
  nPtsOnNewPaths = nrow(paths)
  nPtsOnIBM = length(stockPrices)
  actualPathIndexSet = (nPtsOnIBM - nPtsOnNewPaths+1):nPtsOnIBM
  actualPath = coredata(stockPrices[actualPathIndexSet])
}

###############################################################
# volatilityOneCompany assumes number of days in year as 252.
# Changed name of stock price data to coData; it was coDataOneYear.
volatilityOneCompany = function(coData,nDaysInYear=252) {
  # compute S(i+1)/S(i)
  if ('zoo' %in% class(coData)) {coData = coredata(coData)}
#  nDaysInYear = length(coDataOneYear)
  stockRatio = coData[-1]/coData[-length(coData)] 
  # remove NA
  stockRatio = stockRatio[!is.na(stockRatio)]
  # compute natural log of stockRatio
  logReturns = log(stockRatio)
  # compute standard deviation
  logReturnsSD = sd(logReturns)
  # scale up to annual
  annualVolatility = logReturnsSD * sqrt(nDaysInYear)
  return(annualVolatility)}  

extractPathAtRebalanceTimes = function(pathMatrix,nFlips) {
  nPrices = nrow(pathMatrix)
  rebalanceTimes = round(seq(1,nPrices,length=1+nFlips))
  pathMatrixAtRebalanceTimes = pathMatrix[rebalanceTimes,]
  return(pathMatrixAtRebalanceTimes)
}

testExtractPath = function() {
  pathMatrix = matrix(1:48,nrow=8,ncol=6)
  nFlips = 3
  print(pathMatrix)
  print(extractPathAtRebalanceTimes(pathMatrix,nFlips))
}