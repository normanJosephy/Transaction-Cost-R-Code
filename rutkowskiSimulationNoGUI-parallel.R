# rutkowskiSimulationNoGUI-parallel.R

# require(parallel)
require(doParallel) # Loads parallel, iterators and foreach
# require(foreach)  
#registerDoParallel(cores=2)

setup = function() {
 cl = makeCluster(2)
 registerDoParallel(cl) }

#
# source('ibmConstantsNew.R')
# 
testTime = function() {
  system.time({
  x = foreach(i=1:10000,.combine=rbind) %dopar% {c(first=i,second=i^2)}})

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
#  dimD     = c(nFlips,nPaths,nUDPairsToUseRut)
#  dimD1    = c(nFlips+1,2,nPaths,nUDPairsToUseRut)
#  colN1    = c('stock $','bond $')
#  rowN1    = paste('time-',1:nFlips,sep='')
#  dimN1    = list(rowN1,colN,thdN)
#  dimN2    = list(rowN,colN1,colN,thdN)
  costBigRut  = array(data=NA,dim=dimV,dimnames=dimN)
#  deltaBigRut = array(data=NA,dim=dimD,dimnames=dimN1)
#  portBigRut  = array(data=NA,dim=dimD1,dimnames=dimN2)
  #
  # Step 6 Loop over contour pairs
  #
  cat("\n Step 6\n Loop over contour pairs\n\n")
  flush.console()
  #
  answer = foreach(iUDPair = 1:nUDPairsToUseRut,.combine=rbind) %do% {  # loop over ud pairs
    ud = udMatrixRut[iUDPair,]
    u = ud[1]
    d = ud[2] 
    cat("\n u,d pair: ",iUDPair," out of ",nUDPairsToUseRut,'\n')
    rutOut   = createDeltaRutkowskiNew(u,d)
#    deltaBigRut[,,iUDPair] = rutOut$delta
    ## Computing average total delta
    totalDelta = apply(X = rutOut$delta,MARGIN = 2,FUN = sum)
    avgTotalDelta = mean(totalDelta)
    #
#    GMatrix  = rutOut$GMatrix    
#    HMatrix  = rutOut$HMatrix 
    #
    for (iPath in 1:nPaths) {
      path     = pathMatrix[,iPath]
      G        = rutOut$GMatrix [,iPath]
      shareQt  = G/path
      costRut  = createTCosts(path,shareQt,lambda,mu)
      costBigRut[,iPath,iUDPair]   = costRut
#      portBigRut[,1,iPath,iUDPair] = GMatrix[,iPath]
#      portBigRut[,2,iPath,iUDPair] = HMatrix[,iPath]
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
  plot(1:nUDPairsToUseRut,netDelta,type='l',main="Net Delta vs. (u,d) pair")
  maxLoc = which.max(netDelta)
  udMax  = udMatrixRut[maxLoc,]
  uMax   = round(ud[1],digits=3)
  dMax   = round(ud[2],digits=3)
  points(x = maxLoc,y = netDelta[maxLoc], pch=19, cex=1.3,col='blue')
  label  = paste('(',uMax,',',dMax,')',sep="")
  text(x = maxLoc,y = netDelta[maxLoc],labels = label,pos = 1)
  ###############
  #  STOP COMPUTATION AT THIS POINT
  #  NO DATA SAVED IN LOOP OTHER THAN THE result TUPLE.
  return(answer)
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
