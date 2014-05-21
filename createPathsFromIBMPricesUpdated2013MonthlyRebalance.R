# createPathsFromIBMPricesUpdated2013MonthlyRebalance.R  9/9/2013
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

library(quantmod)   # getSymbols
library(zoo)
library(xts)
library(lubridate)

getIBMData2013 = function(fileName='IBMData2013.Rdata') {
    fromDate = '2012-11-08'
    toDate   = '2013-11-08'
    # getSymbols creates IBM in workspace
    getSymbols('IBM',src='yahoo',from=fromDate,to=toDate)
    ibm = IBM$IBM.Close[,1,drop=TRUE]
    dataDir='c:/Research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
    setwd(dataDir)
    save(ibm,file=fileName)
    invisible(ibm)
                    }

ibmFridayData = function(ibm) {
  whichFriday = (wday(ibm) == 6)
  fridayData  = ibm[whichFriday]
  invisible(fridayData)
}

ibmEndOfMonthData = function(ibm) {
  whichMonth = month(ibm)
  whereMonthEnd = which(diff(whichMonth) != 0)
  endOfMonthData = ibm[whereMonthEnd]
  invisible(endOfMonthData)
}
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

# File IBMData2013.Rdata created by function getIBMData2013().
# 
###
createPathsAndJumpsFromIBMEndOfMonthData = function(nPaths=100,
                                                nNewPointsOnPath=6,
                                                S0=180) {
  if (! exists('ibm')) {ibm = getIBMData2013()}  
  #    S0 = coredata(last(ibm))
  # Use entire IBM time series to construct jump population
  ibmEndOfMonth = ibmEndOfMonthData(ibm)
  jumps = computeJumps(priceData=ibmEndOfMonth)
  paths = createPathsFromJumps(jumps=jumps,
                               S0=S0,nPaths=nPaths,
                               nNewPointsOnPath=nNewPointsOnPath)
  rownames(paths) = NULL
  invisible(list(paths=paths,ibm=ibmEndOfMonth))
}
###
# Test construction of paths using Friday weekly stock prices for IBM1
# and end-on-month stock prices for monthly jumps for IBM2
testNewPathConstruction = function() {
  nPaths=5
  
  ansa = createPathsAndJumpsFromIBMEndOfMonthData(nPaths=nPaths)
  print(ansa)
  invisible(ansa)
}

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
###############################################################