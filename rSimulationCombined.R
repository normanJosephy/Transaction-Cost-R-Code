# rSimulationCombined.R

##########################################
###  from: rutkowskiSimulationNoGUI-parallel-1.R
#######################
# require(parallel)
require(doParallel) # Loads parallel, iterators and foreach
# require(foreach)  
#registerDoParallel(cores=2)

# source("createPathsFromIBMPricesUpdatedNoGUI.R")
# source("createDeltaRutkowskiNewNoGUI.R")
# source("rutkowski-2.R")
# source("createRutkowskiContourNewNoGUI.R")
# # source("rutkowskiSimulationNoGUI.R")
# source("dataPlots.R")
# source("lmplot.R")
# source("dataProcessingDeltaRutOnly-1.R")
# source("testCreateFunctionsNew.R")

# varToExport = c("createDeltaRutkowskiNew",
#                 "createTCosts",
#                 "collectGHRutkowski",
#                 "rutkowskiDelta",
#                 "prob",
#                 "stock",
#                 "computeF1F2",
#                 "HG",
#                 "optionPriceRut",
#                 "callPayoffGH",
#                 "fAtExpiration",
#                 "updateF",
#                 "myEnv",
#                 "computedEnv",
#                 "timeNames")

# 
# 
#  cl = makeCluster(2)
#  registerDoParallel(cl) 
#  cat('\n Number of parallel workers: ',getDoParWorkers())
#  clusterExport(cl=cl,varlist=varToExport)                    
#
# source('ibmConstantsNew.R')
# 

# TO RUN SIMULATION:
#   1. Edit contents of ibmConstantsNew() in file ibmConstantsNew.R 
#   2. Source ibmConstantsNew.R:  source('ibmConstantsNew.R')
#   3. Run rSimulationParallel()

rSimulationParallel = function() {
  #
  # Step 1 Record simulation constants in environment myEnv
  #
  cat("\n Step 1\n Record simulation constants in environment myEnv\n")
  flush.console()
  #
  ibmConstantsNew()
  #
  # Step 2 Construct stock price paths
  #
  cat("\n Step 2\n Construct stock price paths\n")
  flush.console()
  #
  createPathsAndJumpsFromIBMData()
  #
  # Step 3 Construct market contour
  #
  cat("\n Step 3\n Construct market contour\n")
  flush.console()
  #
  createRutContour()
  #
  # Step 4 Load constants from myEnv and computedEnv into working directory
  #
  cat("\n Step 4\n Load constants from myEnv and computedEnv into working directory\n")
  flush.console()
  #
  nFlips = myEnv$nFlips
  nPaths = myEnv$nPaths
  udMatrixRut      = computedEnv$udMatrixRut
  nUDPairsToUseRut = computedEnv$nUDPairsToUseRut
  pathMatrix       = computedEnv$paths
  #
  lambda = myEnv$lambda
  mu = myEnv$mu
  r  = myEnv$r
  u  = myEnv$u
  d  = myEnv$d
  K  = myEnv$K
  #
  # Step 5 Construct empty containers for simulation data
  #
  cat("\n Step 5\n Construct empty containers for simulation data\n")
  flush.console()
  #
  rowN = paste('time-',0:nFlips,sep='')
  colN = paste('path-',1:nPaths,sep='')
  uvector  = round(udMatrixRut[,1],digits=3)
  dvector  = round(udMatrixRut[,2],digits=3)
  thdN     = paste('u:',uvector,' d:',dvector,sep='')
  dimN     = list(rowN,colN,thdN)
  dimV     = c(nFlips+1,nPaths,nUDPairsToUseRut)
  dimD     = c(nFlips,nPaths,nUDPairsToUseRut)
  dimD1    = c(nFlips+1,2,nPaths,nUDPairsToUseRut)
  colN1    = c('stock $','bond $')
  rowN1    = paste('time-',1:nFlips,sep='')
  dimN1    = list(rowN1,colN,thdN)
  dimN2    = list(rowN,colN1,colN,thdN)
  costBigRut  = array(data=NA,dim=dimV,dimnames=dimN)
  deltaBigRut = array(data=NA,dim=dimD,dimnames=dimN1)
  portBigRut  = array(data=NA,dim=dimD1,dimnames=dimN2)
  #
  # Step 6 Loop over contour pairs
  #
  cat("\n Step 6\n Loop over contour pairs\n\n")
  flush.console()
  #
  clusterExport(cl=cl,varlist=varToExport)    
  answer = foreach(iUDPair = 1:nUDPairsToUseRut,.combine=rbind) %dopar% {  # loop over ud pairs
    ud = computedEnv$udMatrixRut[iUDPair,]
    u = ud[1]
    d = ud[2] 
    #    cat("\n u,d pair: ",iUDPair," out of ",nUDPairsToUseRut,'\n')
    rutOut   = createDeltaRutkowskiNew(u,d)
    deltaBigRut[,,iUDPair] = rutOut$delta
    ## Computing average total delta
    totalDelta = apply(X = rutOut$delta,MARGIN = 2,FUN = sum)
    avgTotalDelta = mean(totalDelta)
    #
    GMatrix  = rutOut$GMatrix    
    HMatrix  = rutOut$HMatrix 
    #
    for (iPath in 1:nPaths) {
      path     = pathMatrix[,iPath]
      G        = rutOut$GMatrix [,iPath]
      shareQt  = G/path
      costRut  = createTCosts(path,shareQt,lambda,mu)
      costBigRut[,iPath,iUDPair]   = costRut
      portBigRut[,1,iPath,iUDPair] = GMatrix[,iPath]
      portBigRut[,2,iPath,iUDPair] = HMatrix[,iPath]
    }  # end iPath loop
    ### Computing average total cost
    totalCost = apply(X = costBigRut[,,iUDPair],2,sum)
    avgTotalCost = mean(totalCost)
    ###
    # result is iUDPair, avgTotalDelta, avgTotalCost rbind-ed by foreach loop
    result = c(iUDPair=iUDPair,avgTotalDelta=avgTotalDelta,avgTotalCost=avgTotalCost)
  }  #end iUDPair loop 
  #
  # Plot netDelta
  #
  netDelta = answer[,2] - answer[,3]
  myMain = expression(paste("Net ",Delta," vs. (u,d) pair"))
  plot(1:nUDPairsToUseRut,netDelta,
       type='l',xlab="(u,d)",ylab=expression(paste("Net ",Delta)),
       main=myMain)
  maxLoc = which.max(netDelta)
  udMax  = udMatrixRut[maxLoc,]
  uMax   = round(udMax[1],digits=3)
  dMax   = round(udMax[2],digits=3)
  points(x = maxLoc,y = netDelta[maxLoc], pch=19, cex=1.3,col='blue')
  label  = paste('(',uMax,',',dMax,')',sep="")
  text(x = maxLoc,y = netDelta[maxLoc],labels = label,pos = 1)
  ###############
#   #  STOP COMPUTATION AT THIS POINT
#   #  NO DATA SAVED IN LOOP OTHER THAN THE result TUPLE.
#   return(answer)
  #
  # Step 7 Store output in computedEnv
  #
  cat("\n Step 7\n Store output in computedEnv\n")
  flush.console()
  #
  computedEnv$deltaBigRut = deltaBigRut
  computedEnv$costBigRut  = costBigRut
  computedEnv$portBigRut  = portBigRut
  #
  # Step 8 Save myEnv and computedEnv to data files
  #
  cat("\n Step 8\n Save myEnv and computedEnv to data files\n")
  flush.console()
  #
  fileName = paste(myEnv$WD,'/',paste(myEnv$stockName,myEnv$runNumber,sep=''),sep='')
  myEnvFileName = paste(fileName,'myEnv.RData',sep='')
  computedEnvFileName = paste(fileName,'computedEnv.RData',sep='')
  save(myEnv,     file = myEnvFileName,     envir = .GlobalEnv)
  save(computedEnv,file=computedEnvFileName,envir = .GlobalEnv)
  #
  # Step 9 Draw plots
  #
  cat("\n Step 9\n Draw plots\n")
  flush.console()
  #
  dataPlots()
  #
  # Step 10 Declare simulation done
  #
  cat("\n Step 10\n Declare simulation done\n")
  flush.console()
  #
  cat("\n\n Run",myEnv$runNumber,"COMPLETED")
  cat("\n\n Data stored in files\n",myEnvFileName,'\n',computedEnvFileName,'\n\n')
  invisible(NULL)
}

##########################################
#  from: createPathsFromIBMPricesUpdatedNoGUI.R
##################
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
  #    R = myEnv$K/S0
  myEnv$S0 = S0
  #    myEnv$R  = R
  cat("\n #############\n  Changed initial S0 to ",S0,'in myEnv\n ')
  cat(" ############ Assigned R=myEnv$K/S0 to myEnv$R\n ##############\n\n")
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

###########
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

##########################################
####  from: createDeltaRutkowskiNewNoGUI.R
###########################
createDeltaRutkowskiNew = function(u,d) {
  #  unpackList(myEnv)
  #  unpackList(computedEnv)
  pathMatrix = computedEnv$paths
  lambda = myEnv$lambda
  mu     = myEnv$mu
  r      = myEnv$r
  K      = myEnv$K
  #
  nPaths = ncol(pathMatrix)
  nTimes = nrow(pathMatrix)
  rNames = paste('time-',0:(nTimes-1),sep='')
  cNames = paste('path-',1:nPaths,sep='')
  HMatrix = matrix(NA,nrow=nTimes,ncol=nPaths,dimnames=list(rNames,cNames))
  GMatrix = matrix(NA,nrow=nTimes,ncol=nPaths,dimnames=list(rNames,cNames))
  colnames(pathMatrix) = paste('path-',1:nPaths,sep='')
  rownames(pathMatrix) = paste('time-',0:(nTimes-1),sep='')
  delta = matrix(NA,nrow=nTimes-1,ncol=nPaths)
  rownames(delta) = paste('time-',1:(nTimes-1),sep='')
  colnames(delta) = cNames
  for (i in 1:nPaths) {
    path        = pathMatrix[,i]
    oList       = collectGHRutkowski(path,lambda,mu,r,u,d,K)
    G           = oList$hgInitial[,'G']
    H           = oList$hgInitial[,'H']
    GMatrix[,i] = G
    HMatrix[,i] = H
    delta[,i]   = rutkowskiDelta(path,H,G,lambda,mu,r)
  }
  # Prepare delta
  del = rbind(rep(NA,ncol(delta)),delta)
  rownames(del) = NULL # paste('path-',1:nPaths,sep='')
  colnames(del) = rep('deltaRut',ncol(delta)) # c('time-1','time-2')
  outputList = list(delta=delta,GMatrix=GMatrix,HMatrix=HMatrix,pathMatrix=pathMatrix)
  invisible(outputList)
}

# Use Rutkowski base u,d pair as u,d at which deltas are created
testCreateDeltaRutkowskiNew = function() {
  ud    = computeBaseUDNew()
  u     = ud['u']
  d     = ud['d']
  outputList=createDeltaRutkowskiNew(u,d)
  unpackList(outputList)
  #layout(c(1,3))
  par(mar=c(4,3,2,1),mfrow=c(3,1))
  matplot(1:nrow(GMatrix),GMatrix,type='l',ylab="", xlab="")
  mtext('Stock G',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  matplot(1:nrow(HMatrix),HMatrix,type='l',ylab="", xlab="")
  mtext('Bond H',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  matplot(1:nrow(delta),delta,type='l',ylab="",xlab="")
  abline(h=0)
  mtext('Delta',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  layout(1)
  #  print(outputList)
  invisible(outputList)
}
#############################################################
####  from: rutkowski-2.R
############################
timeNames = function(nTimes) paste('time=',0:(nTimes-1),sep='')

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

prob = function(lambda,mu,r,u,d) {
  l1 = 1 + lambda
  m1 = 1 - mu
  r1 = 1 + r
  top = unname(l1*r1 - m1*d)
  bot = unname(l1*u - m1*d)
  phat = top/bot
  top1 = unname(m1*r1 - m1*d)
  ptilda = top1/bot
  return(c(phat=phat,ptilda=ptilda))
}

# callPayoffGH is by delivery: g=ST,h=-K if in the money
callPayoffGH = function(ST,K) {
  g = ifelse(ST > K,ST,0)
  h = ifelse(ST > K,-K,0)
  return(cbind(g=g,h=h))
}

fAtExpiration = function(g,h,lambda,mu) {
  f1 = h + (1+lambda)*g
  f2 = h + (1 - mu)*g
  return(cbind(f1=f1,f2=f2))
}

updateF = function(phat,ptilda,r,jTime,f1,f2){
  numberOfStates = jTime+1
  phatC   = 1 - phat
  ptildaC = 1 - ptilda
  f1Update = rep(NA,numberOfStates)
  f2Update = rep(NA,numberOfStates)
  rFactor = 1/(1+r)
  for (iState in 1:numberOfStates) {
    f1Update[iState] = rFactor*(phat   * f1[iState] + phatC   * f2[iState+1])
    f2Update[iState] = rFactor*(ptilda * f1[iState] + ptildaC * f2[iState+1])   
  }
  return(cbind(f1=f1Update,f2=f2Update))
}

HG = function(mu,lambda,fStored) {
  hgStored = list()
  nTimePeriods = length(fStored)
  coeff = 1/(mu + lambda)
  for (time in 1:nTimePeriods) {
    #f1, f2 stored in reverse time order: expiration is first element
    f1 = unname(fStored[[time]][,'f1'])
    f2 = unname(fStored[[time]][,'f2'])
    H = coeff*((1+lambda)*f2 - (1 - mu)*f1)
    G = coeff*(f1 - f2)
    hgStored[[time]] = cbind(H=H,G=G)
  }
  names(hgStored) = rev(timeNames(nTimePeriods))
  return(hgStored)
}

optionPriceRut = function(phat,r,fStored) {
  nTime   = length(fStored)
  # fStored is backwards; first entry is at expiration
  fValues = rev(fStored)[[2]]
  fUp     = fValues[1,'f1']
  fDown   = fValues[2,'f2']
  optionValue = 1/(r+1)*(phat*fUp + (1 - phat)*fDown)
  return(optionValue)
}

computeF1F2 = function(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r) {
  # create storage for f1,f2
  fStored = list()
  #
  # calculate at expiration the g and h vectors
  #
  ST = stockList[[length(stockList)]]
  gh = callPayoffGH(ST,K)
  g  = gh[,'g']
  h  = gh[,'h']
  #
  # compute f1,f2 at expiration
  #
  f12 = fAtExpiration(g,h,lambda,mu)
  f1  = f12[,'f1']
  f2  = f12[,'f2']
  ifStored = 1
  fStored[[ifStored]] = f12
  #
  # loop over time to compute f1,f2 at all non-terminal nodes
  lastJTime = nFlips # jTime goes from 0 to nFlips
  #
  # fold f1,f2 tree backwards from jTime = lastJTime-1 to jTime=0
  for (jTime in seq(lastJTime-1,0)) {
    fUpdated = updateF(phat,ptilda,r,jTime,f1,f2)
    #    print(fUpdated)
    ifStored            = ifStored + 1
    fStored[[ifStored]] = fUpdated
    f1                  = fUpdated[,'f1']
    f2                  = fUpdated[,'f2']
  }
  names(fStored) = rev(timeNames(nFlips+1))
  return(fStored)
}

rutkowskiDelta = function(path,H,G,lambda=0.2,mu=1/6,r=0) {
  nPathValues = length(path)
  psi         = path[-1]/path[-nPathValues]
  factorV     = vector(mode='numeric',length=length(G) - 1)
  for (i in 1:length(factorV)){
    if (G[i+1]/path[i+1] > G[i]/path[i]) {factorV[i]=1+lambda
    } else {factorV[i]=1-mu}
  }
  HD = H[-length(H)]*(1+r) - H[-1]
  GD = G[-length(G)]*psi - G[-1]
  delta = HD + factorV*GD
  return(delta)
}

collectGHRutkowski = function(path,lambda=0.2,mu=1/6,r=0,u=1.3,d=0.9,K=100) {
  nFlips           = length(path) - 1
  hgInitial        = matrix(0,nrow=nFlips+1,ncol=2)
  colnames(hgInitial) = c('H','G')
  rownames(hgInitial) = paste('time',0:nFlips,sep='-')
  optionPriceValue = rep(NA,nFlips+1)
  outputList       = list()
  nFlipsList       = seq(nFlips,1)
  p                = prob(lambda,mu,r,u,d)
  phat   = p['phat']
  ptilda = p['ptilda']
  #
  for (i in 1:nFlips) {
    s  = path[i]
    nF = nFlipsList[i] 
    stockList = stock(s,u,d,nF)
    fStored   = computeF1F2(stockList,s,u,d,nF,K,lambda,mu,phat,ptilda,r)
    price     = unname(optionPriceRut(phat,r,fStored))
    hg        = HG(mu,lambda,fStored)
    hgInitial[i,]        = rev(hg)[[1]]
    optionPriceValue[i]  = price
    output    = list(s=s,nF=nF,hg=hg,price=price,
                     fStored=fStored,p=p,stockList=stockList)
    outputList[[i]]      = output
  }
  # Calculate and save expiration time G,H, which are the
  # option payoffs at expiration.
  # Compute option payoff at expiration as max(s-K,0).
  #
  # Return value of callPayoffGH is a one row matrix.
  # callPayoffGH was modified to accept a vector of stock prices,
  # and it returns a matrix of g,h values.
  # Access its values by GHTerminal[,'g'] and GHTerminal[,'h'].
  pathTerminalValue = path[length(path)]
  GHTerminal        = callPayoffGH(pathTerminalValue,K)
  gValue = GHTerminal[,'g']
  hValue = GHTerminal[,'h']
  gValue = unname(gValue)
  hValue = unname(hValue)
  hgInitial[nFlips+1,]       = c(hValue,gValue)
  optionPriceValue[nFlips+1] = max(pathTerminalValue - K,0)  
  invisible(list(hgInitial=hgInitial,
                 optionPriceValue=optionPriceValue,
                 outputList=outputList))
}


# From residualsCRR.R
# All costs will be positive.
createTCosts = function(sPath,portShareQt,lambda,mu,verbose=FALSE){
  # Prepend 0 to share quantity, to compute setup transaction cost 
  portShareQtWithZero = c(0,portShareQt)
  changeShareQt       = diff(portShareQtWithZero)
  factor = lambda * (changeShareQt > 0) - mu * (changeShareQt <= 0)
  tCost  = changeShareQt * sPath * factor
  # Prepare data for printing
  if (verbose) {
    output = cbind(sPath,portShareQt,factor,tCost)
    print(round(output,digits=4),digits=4)
  }
  invisible(tCost)
}

# All costs will be positive.
# createTCosts() is in residualCRR.R
# GMatrix from createDeltaRutkowski()
tCostRutkowski = function(pathMatrix,GMatrix,lambda,mu){
  nTime = nrow(pathMatrix)
  nPath = ncol(pathMatrix)
  rutCosts = matrix(NA,nrow=nTime,ncol=nPath)
  colnames(rutCosts) = paste('path-',1:nPath,sep='')
  for (kpath in 1:nPath) {
    path = pathMatrix[,kpath]
    G    = GMatrix[,kpath]
    shareQt = G/path
    rutCosts[,kpath] = createTCosts(path,shareQt,lambda,mu)
  }   # end ipath loop
  invisible(rutCosts)
}

#############################################################
#####  from: createRutkowskiContourNewNoGUI.R
##############################################
createRutContour= function() {
  udBaseRut = computeBaseUDNew()
  u         = udBaseRut['u']
  d         = udBaseRut['d']
  priceBaseRut = computeBasePriceNew(u=u,d=d)
  dLRut        = computeGridPriceNew()
  udMatrix2    = getContourRutkowski(dL=dLRut,oPrice=priceBaseRut)
  # Choose subset of (u,d) pairs based on nUDPairsToUse
  nUDPairsToUse = myEnv$nUDPairsToUse
  nUDPairsRut   = nrow(udMatrix2)
  rowsToUseRut = unique(round(seq(1,nUDPairsRut,length=nUDPairsToUse)))
  nUDPairsToUseRut = length(rowsToUseRut)
  stopifnot(length(rowsToUseRut) > 0)
  udMatrixRut = udMatrix2[rowsToUseRut,,drop=FALSE]
  colnames(udMatrixRut) = c('u','d')
  returnList   = list(udBaseRut=udBaseRut,
                      priceBaseRut=priceBaseRut,
                      dLRutkowski=dLRut,
                      udMatrix2=udMatrix2,
                      rowsToUseRut=rowsToUseRut,
                      nUDPairsToUseRut=nUDPairsToUseRut,
                      udMatrixRut=udMatrixRut)
  packListToEnvironment(myList = returnList,myEnvironment = computedEnv)
  invisible(returnList)}

computeBaseUDNew = function() {
  unpackList(myEnv)
  h = TimeToExpiration/nFlips
  u = unname(exp(sigma*sqrt(h)))
  d = unname(1/u)
  return(c(d=d,u=u))
}

computeBasePriceNew = function(u,d){
  unpackList(myEnv)
  p      = prob(lambda,mu,r,u,d)
  phat   = p['phat']
  ptilda = p['ptilda']
  stockList = stock(S0,u,d,nFlips)
  fStored   = computeF1F2(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r)
  price     = optionPriceRut(phat,r,fStored)
  return(unname(price))
}

computeGridPriceNew = function(){
  unpackList(myEnv)
  uValues = seq(uStart,uEnd,length=nPtsU)
  dValues = seq(dStart,dEnd,length=nPtsD)
  priceArray = matrix(NA,nrow=nPtsD,ncol=nPtsU)
  cat('\n\n IN computeGridPrice() FOR Rutkowski Contour Computation')
  for (i in 1:nPtsU) { # each column i fixes one u value
    u = uValues[i]
    cat('\n u value number',i,' out of',nPtsU)
    for (j in 1:nPtsD) { # each row j fixes one d value
      d = dValues[j]
      priceArray[j,i] = computeBasePriceNew(u=u,d=d)
    } # end j loop
  }   # end i loop
  #  cat('\n-----------------------------------------\n')
  invisible(list(u=uValues,d=dValues,c=priceArray))
}

getContourRutkowski = function(dL,oPrice) {
  # NO SCALING IN COMPUTATION OF c.
  cLines = contourLines(dL$u,dL$d,dL$c,levels=oPrice)
  if (length(cLines) == 0) {stop('No Rutkowski contour. Exiting program')
  } else {return(cbind(u=cLines[[1]]$x,d=cLines[[1]]$y))}
}
###################################
####  from: dataPlots.R
########################
dataPlots = function() {
  if (! exists('nPaths')) unpackList(myEnv)
  if (! exists('costBigRut')) unpackList(computedEnv)
  #
  # Plot transaction cost for each (u,d) pair: summed over time, averaged over paths
  # Repeat for delta
  #
  c1 = apply(costBigRut,c(2,3),sum)
  c2 = apply(c1,2,mean)
  plot(1:length(c2),c2,type='l',main="Average Total Cost",xlab='(u,d)',ylab="Cost")
  d1 = apply(deltaBigRut,c(2,3),sum)
  d2 = apply(d1,2,mean)
  plot(1:length(d2),d2,type='l',main="AverageTotalDelta",xlab='(u,d)',ylab='Delta')
  #
  # Plot market contour and option price surface
  #
  cat('\n Rutkowski base price',priceBaseRut,'\n')
  cat('\n Rutkowski contour plotted from',nrow(udMatrix2),'(u,d) pairs\n')
  optionPriceRounded = round(priceBaseRut,digits=3)
  mainT1 = paste('Rutkowski contour for option price', optionPriceRounded)
  mainT2 = paste('Rutkowski surface plots')
  plotContour(udMatrix2,mainTitle=mainT1)
  cat("\n Rutkowski contour using",nUDPairsToUse,"(u,d) pairs\n")
  plotContour(udMatrixRut,mainTitle="Subset of market contour")
  plotSurface(dLRutkowski,mainTitle=mainT2)
  plotSurface1(dLRutkowski,mainTitle=mainT2)
  cat('\n Rutkowski option price surface plotted')
  #
  # Plot simulated paths and actual path
  #
  pc = min(ncol(paths),40)
  p     = paths[,1:pc] # Use 40 paths in plot or all if less than 40 exist.
  pp    = plotPaths(paths=p,actualPath=actualPath)
  print(pp)
  #
  # Plot Delta density functions
  #
  processRutDelta()
}


# cL returned by getContour()
plotContour = function(cL,mainTitle=NULL) {
  plot(cL[,2],cL[,1],type='l',xlab='d',ylab='u',main=mainTitle)
}

# dL returned by cSurface()
plotSurface1 = function(dL,mainTitle=NULL) {
  persp(dL$u,dL$d,dL$c,
        xlab="u",ylab="d",zlab="c",
        shade = 0.15, border = TRUE,scale=TRUE,axes=TRUE,
        ticktyp="detailed",nticks=2,main=mainTitle)
  return(NULL)
}

plotSurface = function(dL,mainTitle=NULL) {
  w = wireframe(dL$c,row.values=dL$u,column.values=dL$d,
                scales=list(arrows=FALSE),main=mainTitle,
                xlab='u',ylab='d',zlab='c',
                drape=TRUE,colorkey=TRUE)
  print(w)
}


# plot simulated paths using lattice
plotPaths = function(paths,actualPath){
  x = 1:nrow(paths)
  dataList = list(actualPath=actualPath)
  yLim = range(c(actualPath,paths))
  myMain = paste('Simulated',myEnv$stockName,'Stock Price Paths')
  p = xyplot(c(paths) ~ rep(x,ncol(paths)),
             groups=c(col(paths)), 
             type='l',
             lwd=1,
             xlab='Time',
             ylab='Stock price',
             main=myMain,
             ylim=yLim)
  pp = p + layer(panel.points(x=x,y=actualPath,pch=19,cex=1.3,col='black'),data=dataList)
  invisible(pp)
}
#####################################
#####   from: dataProcessingDeltaRutOnly-1.R
########################
dSum = function(x,r) {
  sum(x/(1+r)^seq(1,length(x)))
}

processRutDelta = function(printIt=FALSE) {
  #
  if (! exists('nPaths')) unpackList(myEnv)
  if (! exists('costBigRut')) unpackList(computedEnv)
  #
  quantileProbs = seq(0,1,by=.1)
  #
  uVectorRut = udMatrixRut[,1]
  dVectorRut = udMatrixRut[,2]
  nUDPairsRut  = nrow(udMatrixRut)
  xList = list()
  yList = list()
  
  xList[[1]] = uVectorRut
  yList[[1]] = dVectorRut
  #
  # Create formula expressions for graph titles
  #
  runText = paste('Run',runNumber,'  ')
  mainSmallDelta = bquote(expression(paste(.(runText),delta)))
  mainLargeDelta = bquote(expression(paste(.(runText),Delta)))
  mainDeltaBar   = bquote(expression(paste(.(runText),bar(Delta))))
  # Density of small delta costs
  # Population  is of size (nFlips+1) * nPaths * nUDPairs
  #   
  # Sum individual delta on each path to get Delta.
  #
  totalDeltaRut = apply(deltaBigRut,c(2,3),sum)
  #  totalDeltaRutDiscounted = apply(deltaBigRut,c(2,3),dSum,r=r)
  
  dataSetList = list(totalDeltaRut)
  
  dp1 = densityListPlot(dataSetList,printIt=TRUE,
                        xlab=expression(delta),
                        legendText=c('Rut'),
                        mainText='delta distributions')
  
  #
  # Average summed deltas to get average Delta, which we maximize.
  #
  avgTotalDeltaRut     = apply(totalDeltaRut,2,mean)
  # avgTotalDeltaRutDisc = apply(totalDeltaRutDiscounted,2,mean)
  
  aList = list()
  aList[[1]] = avgTotalDeltaRut
  # aList[[2]] = avgTotalDeltaRutDisc
  
  dp2 = lPlot(xList,aList,legendText=c('Rut'),
              main=mainDeltaBar,
              xlab='u',ylab=expression(bar(Delta)))
  #
  if (printIt) {
    #
    cat('\n\n Rut total delta quantiles\n')
    print(quantile(totalDeltaRut,probs=quantileProbs))
    #
    cat('\n\n Rut delta quantiles\n')
    print(round(quantile(deltaBigRut,probs=quantileProbs),digits=4))
    listNames = c('avgTotalDelta Rut')
    cat("\n ",listNames[1],'\n')
    print(quantile(aList[[1]],probs=quantileProbs))
  }   # end if(printIt)
  invisible(NULL)
}
############################################
