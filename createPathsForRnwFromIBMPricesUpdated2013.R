# createPathsForRnwFromIBMPricesUpdated2013.R  9/9/2013
# 
# Copied from createPathsFromIBMPricesUpdated.R to extract Friday stock prices
# for weekly paths and last day of month for monthly stock prices.

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
# 11/20/2013 Prepared code for 7-16-2013 to 9-16-2013 daily runs

## @knitr loadPackages
require(quantmod)   # getSymbols
require(zoo)
require(xts)
require(lubridate)
require(lattice)

## @knitr getIBMData2013
getIBMData2013 = function(fileName='IBMData2014.Rdata') {
    fromDate = '2013-06-16'
    toDate   = '2013-09-16'
    # getSymbols creates IBM in workspace
    getSymbols('IBM',src='yahoo',from=fromDate,to=toDate)
    ibm = IBM$IBM.Close[,1,drop=TRUE]
    dataDir='c:/Research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
    setwd(dataDir)
    save(ibm,file=fileName)
    invisible(ibm)
                    }

## @knitr fridayData
ibmFridayData = function(ibm) {
  whichFriday = (wday(ibm) == 6)
  fridayData  = ibm[whichFriday]
  invisible(fridayData)
}

## @knitr endOfMonthData
ibmEndOfMonthData = function(ibm) {
  whichMonth = month(ibm)
  whereMonthEnd = which(diff(whichMonth) != 0)
  endOfMonthData = ibm[whereMonthEnd]
  invisible(endOfMonthData)
}  

## @knitr computeJumps
  

# Copied from dailyPaths.R
# Modified to accept either zoo object or plain vector
# Modified 9/26/2011
# 7/23/2012 Removed 'not using all data' code.
# 11/9/2013 changed name to computeJumps
computeJumps = function(priceData) {
    if ('zoo' %in% class(priceData)) {priceData = coredata(priceData)}
    jumps = priceData[-1]/priceData[-length(priceData)]
    invisible(jumps)
    }

## @knitr createPathsFromJumps

# Original code copied from dailyPaths.R and modified
# nNewPointsOnPath is one less than total prices (S0 appended to front of paths)
# Changed return output to just be paths and not also jumps.
createPathsFromJumps = function(jumps,S0,nPaths=100,nNewPointsOnPath=6) {
    sampledJumpsVector = sample(jumps,nPaths*nNewPointsOnPath,replace=TRUE)
    sampledJumps       = matrix(sampledJumpsVector,
                            nrow=nNewPointsOnPath,ncol=nPaths)
    X                  = rbind(S0,sampledJumps)
    simulatedPaths     = apply(X,2,cumprod)
    invisible(simulatedPaths)
    }
## @knitr createIBMPaths 
# File IBMData2013.Rdata created by function getIBMData2013().
# 
newPathsFromNewIBMData = function() {
  IBM      = getIBMData2013()
  IBMJumps = computeJumps(priceData=IBM)
  IBMVol   = volatilityOneCompany(coredata(IBM))
  cat('\n IBM volatility:',IBMVol)
  paths = createPathsFromJumps(
    jumps=IBMJumps,
    S0=193.15, # Stock price on 9/16/2013
    nPaths=100,
    nNewPointsOnPath=63)
  invisible(paths)
}


testNewPathConstruction = function() {
  paths = newPathsFromNewIBMData()
  pathLength = nrow(paths)
  nPaths     = ncol(paths)
  pathIndex = 1:pathLength
  matplot(x=pathIndex,y=paths,type='l')
  invisible(ans)
}

## @knitr endOfCode 

###
# Test construction of paths using Friday weekly stock prices for IBM1
# and end-on-month stock prices for monthly jumps for IBM2

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
#############################################################
# extractPathAtRebalanceTimes() no longer needed.
# Paths constructed from jumps of Friday stock prices for IBM1,
# and jumps of end of month stock prices for IBM2.
extractPathAtRebalanceTimes = function(pathMatrix,nFlips) {
  nPrices = nrow(pathMatrix)
  rebalanceTimes = round(seq(1,nPrices,length=1+nFlips))
  pathMatrixAtRebalanceTimes = pathMatrix[rebalanceTimes,]
  return(pathMatrixAtRebalanceTimes)
}
#
testExtractPath = function() {
  pathMatrix = matrix(1:48,nrow=8,ncol=6)
  nFlips = 3
  print(pathMatrix)
  print(extractPathAtRebalanceTimes(pathMatrix,nFlips))}

createPathsAndJumpsFromIBMFridayData = 
  function(nPaths=100,nNewPointsOnPath=8,S0=180) {
    if (! exists('ibm')) {ibm = getIBMData2013()}  
    ibmFriday = ibmFridayData(ibm)
    jumps = computeJumps(priceData=ibmFriday)
    paths = createPathsFromJumps(jumps=jumps,
                                 S0=S0,nPaths=nPaths,nNewPointsOnPath=nNewPointsOnPath)
    rownames(paths) = NULL
    invisible(paths)
  }
###
createPathsAndJumpsFromIBMEndOfMonthData = 
  function(nPaths=100,nNewPointsOnPath=6,S0=180) {
    if (! exists('ibm')) {ibm = getIBMData2013()}  
    ibmEndOfMonth = ibmEndOfMonthData(ibm)
    jumps = computeJumps(priceData=ibmEndOfMonth)
    paths = createPathsFromJumps(jumps=jumps,
                                 S0=S0,nPaths=nPaths,
                                 nNewPointsOnPath=nNewPointsOnPath)
    rownames(paths) = NULL
    invisible(paths)}
###############################################################