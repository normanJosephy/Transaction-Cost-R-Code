# createPathsFromIBMPricesUpdatedNoGUI.R  6-14-2014 

# require(PBSmodelling)
# require(lattice)
# require(latticeExtra)

# 9/26/2011
# Dates changed in IBMData() to sept. to sept. data
# IBMData() modified to save ibm time series in file IBMData.Rdata
# computeDailyJumps() modified to optionally use all data and
# input time series from file IBMData.Rdata.

# 7/23/2012 Moved to directory 
#              c:/Research/Lucy/LucyVitaTCostAlgorithm-july2012-part2/'
#           Changed ibm data dates to: july 1,2011 to july 1,2012
#           Copied volatilityOneCompany() from volatility-2010.R

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
#           Removed directory switch from createPathsAndJumpsFromIBMData().

# 11/9/2013 Modified to collect most recent data
# switch to c:/Research/Lucy-Oct-Nov2013RComputationsForLatestVersionOfPaper/
# 6/9/2014 updated ibm data to be stored in 
#   c:/Research/Lucy-2014/Transaction-Cost-R-Code

# 6/11/2014 Changed createPathsAndJumpsFromIBMData() to create
#           paths based on training period ibm data and use
#           ibm value at first value in actualPath as S0 
#           The actualPath is ibm[(n-nNewPts):n], where
#           n = length of ibm time series, and S0=ibm[n-nNewPts]

IBMData = function(fileName='IBMData2014.Rdata') {
    fromDate = '2013-06-01'
    toDate   = '2014-06-01'
    # getSymbols creates IBM in workspace
    getSymbols('IBM',src='yahoo',from=fromDate,to=toDate)
    ibm = IBM$IBM.Close[,1,drop=TRUE]
  #  dataDir=setwdGUI()
  # setwd(dataDir)
    save(ibm,file=fileName)
    invisible(ibm)
                    }

# Copied from dailyPaths.R
# Modified to accept either zoo object or plain vector
# Modified 9/26/2011
# 7/23/2012 Removed 'not using all data' code.
computeDailyJumps = function(priceData) {
    if ('zoo' %in% class(priceData)) {priceData = coredata(priceData)}
    jumps = priceData[-1]/priceData[-length(priceData)]
    invisible(jumps)
    }

# Original code copied from dailyPaths.R and modified
# nNewPointsOnPath is one less than total prices (S0 appended to front of paths)
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
    actualPathStartingValueAt = length(ibm)-nNewPointsOnPath
    S0 = coredata(ibm)[actualPathStartingValueAt]
    # Use entire IBM time series to construct jump population
    jumps = computeDailyJumps(priceData=ibm)
    paths = createDailyPathsFromJumps(jumps=jumps,
        S0=S0,nPaths=nPaths,nNewPointsOnPath=nNewPointsOnPath)
    actualPath = computeActualPath(ibm,paths)
    outputList = list(paths=paths,ibm=ibm,actualPath=actualPath)
    packListToEnvironment(outputList,computedEnv)
    invisible(outputList)
    }

# Actual path is testing path, the last nPtsOnNewPaths values of ibm.

computeActualPath = function(ibm,paths){
  nPtsOnNewPaths = nrow(paths)
  nPtsOnIBM = length(ibm)
  actualPathIndexSet = (nPtsOnIBM - nPtsOnNewPaths+1):nPtsOnIBM
  actualPath = coredata(ibm[actualPathIndexSet])
}

testCreatePathsAndJumpsFromIBMData = function() {
  answer = createPathsAndJumpsFromIBMData()
  paths = answer$paths
  ibm   = answer$ibm
  actualPath = answer$actualPath
  pp = plotPaths(paths,actualPath)
  print(pp)
}

# require(lattice)

# plot simulated paths using lattice
plotPaths = function(paths,actualPath){
  x = 1:nrow(paths)
  dataList = list(actualPath=actualPath)
  yLim = range(c(actualPath,paths))
  p = xyplot(c(paths) ~ rep(x,ncol(paths)),
             groups=c(col(paths)), 
             type='l',
             lwd=1,
             xlab='Time',
             ylab='Stock price',
             main='Simulated IBM Stock Price Paths',
             ylim=yLim)
  pp = p + layer(panel.points(x=x,y=actualPath,pch=19,cex=1.3,col='black'),data=dataList)
  invisible(pp)
}

testPlotUsingLattice = function() {
  paths = newPathsFromNewIBMData()
  if (! exists('ibmFuture')) load('IBMActualPath.Rdata')
  p     = paths[,1:40] # Use 40 paths in plot
  pp    = plotPaths(paths=p,actualPath=ibmFuture)
  print(pp)
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