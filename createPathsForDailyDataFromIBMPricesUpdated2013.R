# createPathsForDailyDataFromIBMPricesUpdated2013.R  11/23/2013
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

# 11/23/2013 Changed getIBMData2013 to getIBMData2014
#            IBM data from 2013-06-16 to 2013-09-16
#            There are 64 daily closing prices in ibm data set
#            Stock price path 'ibm' stored in IBMData2014.Rdata

require(quantmod)   # getSymbols
require(zoo)
require(xts)
require(lubridate)
require(lattice)
require(latticeExtra)

getIBMData2014 = function(fileName='IBMData2014.Rdata') {
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

getActualFutureIBMStockPricePath = function(fileName='IBMActualPath.Rdata'){
  fromDate = '2013-09-16'
  toDate   = '2013-11-16'
  getSymbols('IBM',src='yahoo',from=fromDate,to=toDate)
  ibmFuture = IBM$IBM.Close[,1,drop=TRUE]
  dataDir='c:/Research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
  setwd(dataDir)
  save(ibmFuture,file=fileName)
  invisible(ibmFuture)
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
# Used 64 stock prices to compute jumps.
# Used 45 stock prices from jumps to compute paths,
# since actual 'future' ibm path has 45 values, from 2013-09-16 to 2013-11-15
# 
createPathsFromJumps = function(jumps,S0,nPaths=100,nNewPointsOnPath=44) {
    sampledJumpsVector = sample(jumps,nPaths*nNewPointsOnPath,replace=TRUE)
    sampledJumps       = matrix(sampledJumpsVector,
                            nrow=nNewPointsOnPath,ncol=nPaths)
    X                  = rbind(S0,sampledJumps)
    simulatedPaths     = apply(X,2,cumprod)
    invisible(simulatedPaths)
    }

# File IBMData2014.Rdata created by function getIBMData2014().
# 
newPathsFromNewIBMData = function() {
  if (! exists('ibm')) {ibm = getIBMData2014()}  
  IBMJumps = computeJumps(priceData=ibm)
  IBMVol   = volatilityOneCompany(coredata(ibm))
  cat('\n IBM volatility:',IBMVol)
  paths = createPathsFromJumps(
    jumps=IBMJumps,
    S0=193.15, # Stock price on 9/16/2013
    nPaths=100,
    nNewPointsOnPath=44)
  invisible(paths)
}

# plot simulated paths using lattice
plotPaths = function(paths,actualPath){
  x = 1:nrow(paths)
  dataList = list(actualPath=actualPath)
  p = xyplot(c(paths) ~ rep(x,ncol(paths)),
             groups=c(col(paths)), 
             type='l',
             lwd=1,
             xlab='Time',
             ylab='Stock price',
             main='Simulated IBM Stock Price Paths')
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

testNewPathConstructionUsingMatplot = function() {
  paths      = newPathsFromNewIBMData()
  if (! exists('ibmFuture')) load('IBMActualPath.Rdata')
  pathLength = nrow(paths)
  nPaths     = ncol(paths)
  pathIndex  = 1:pathLength
  matplot(x=pathIndex,y=paths,
          type='l',
          xlab='Time',ylab='Stock Price',
          main='Simulated IBM Stock Price Paths')
  lines(1:pathLength,ibmFuture,lwd=2,col='black')
  invisible(paths)
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
