#  ibmConstantsNew.R

# This file replaces inputParametersNoGUI.R

require(quantmod)   # getSymbols
require(lattice)
require(latticeExtra)

source("createPathsFromIBMPricesUpdatedNoGUI.R")
source("createDeltaRutkowskiNewNoGUI.R")
source("rutkowski-2.R")
source("createRutkowskiContourNewNoGUI.R")

savemyEnv = function(fileName) {
  fullFileName = paste(myEnv$WD,fileName,sep='')
  save(myEnv,file=fullFileName,envir=.GlobalEnv)
  cat("\n Saved run ",myEnv$runNumber," in file ",fullFileName,"\n\n")
}

loadmyEnv = function(fileName,
    fileDirectory='C:/research/Lucy-2014/Transaction-Cost-R-Code/data/') {
  fullFileName = paste(fileDirectory,fileName,sep='')
  load(file = fullFileName,envir = .GlobalEnv)
  cat("\n Loaded run ",myEnv$runNumber, ' from file \n ',fullFileName,'\n\n')
}

tests = function() {
  cat("\n Storing constants in environment myEnv\n")
  ibmConstantsNew()
  cat("\n *** Run testEnvironment ***\n")
  testEnvironment()
  cat("\n ***  Run testCreatePathsAndJumpsFromIBMData *** \n")
  testCreatePathsAndJumpsFromIBMData()
  cat("\n ***  Run testCreateRutkowskiContourNew *** \n")
  testCreateRutkowskiContourNew()
  cat("\n ***  Run testCreateDeltaRutkowskiNew *** \n")
  testCreateDeltaRutkowskiNew()
  cat("\n *** DONE *** \n")
}

ibmConstantsNew = function() {
  myEnv = new.env()
  computedEnv = new.env()
  with(myEnv,{
   baseDir = getwd()
   WD = paste(baseDir,'data',sep='/')
   FN = paste(WD,'IBMData2014.Rdata',sep='/')
   if (! file.exists(FN)) {
    cat("\n\n***** File ",FN," does not exist\n ***** EXITING PROGRAM\n\n")
    invisible(NULL)
                           }
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
  stockName = 'IBM'
  from = '2013-06-01'
  to   = '2014-06-01'
  })
  assign("myEnv",myEnv,envir=.GlobalEnv)
  cat("\n\n Created environment myEnv with run ",myEnv$runNumber, " constants. \n\n")
  assign("computedEnv",computedEnv,envir=.GlobalEnv)
  cat(" Created environment computedEnv to store computed value\n\n")
  # Load stock price time series
  IBMDataFromStoredFile()
  invisible(myEnv)}

# NOTE: IBMDataFromStoredFile() will call IBMDataFromYahoo() if the
# data file  stockName+Data2014.RData does not exist.

# Run IBMDataFromYahoo to retrieve historical data and store closing
# prices in file such as IBMData2014.RData in subdirectory WD
# It also assigns file name to myEnv value FN.

IBMDataFromYahoo = function() {
  stockName = myEnv$stockName
  getSymbols(stockName,src='yahoo',
             from=myEnv$from,to=myEnv$to)
  # This evaluates (e.g.) IBM$IBM.Close[,1,drop=TRUE]
  # and assigns stock prices to variable named stockPrices.
  dataName = paste(stockName,'$',stockName,'.Close[,1,drop=TRUE]',sep='')
  stockPrices = eval(parse(text=dataName))
  myEnv$stockPrices = stockPrices
  dataFileName = paste(stockName,'Data2014.RData',sep='')
  fullFileName = paste(myEnv$WD,dataFileName,sep='/')
  save(stockPrices,file = fullFileName)
  myEnv$FN = fullFileName
}

# Loads stock prices from e.g. IBMData2014.RData
# and places them in myEnv$stockPrices
# It also assigns file name to myEnv variable FN
IBMDataFromStoredFile = function() {
  stockName    = myEnv$stockName
  dataFileName = paste(stockName,'Data2014.RData',sep='')
  fullFileName = paste(myEnv$WD,dataFileName,sep='/')
  if (! file.exists(fullFileName)) {
    IBMDataFromYahoo()
    return(NULL)
  }
  load(file = fullFileName)
  myEnv$stockPrices = stockPrices
  myEnv$FN    = fullFileName
  cat("\n\n Stock prices ", stockName, 'loaded and stored in myEnv\n\n')
  cat(" File name containing stock price data\n",myEnv$FN,'\n\n')
}

testEnvironment = function() {
#  myEnv = ibmConstantsNew()
  print(" myEnv contents\n")
  print(ls(name = myEnv))
  print(" computedEnv contents")
  print(ls(name = computedEnv))
  cat("\n *******************************************")
  cat("\n\n Run ",myEnv$runNumber, " stored in myEnv\n\n")
}
#########################################################
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

