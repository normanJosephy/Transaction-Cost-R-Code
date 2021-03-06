# rutkowskiSimulationMCparallel.R

require(parallel)

# TO RUN SIMULATION:
#   1. Edit contents of ibmConstantsNew1() in file ibmConstantsNew.R 
#   2. Source ibmConstantsNew-1.R:  source('ibmConstantsNew-1.R')
#   3. Run ibmConstantsNew1()
#   4. Source rutkowskiSimulationMCparallel.R (this file)
#   5. Run rSimulationParallel2()
#      system.time(rSimulationParallel2())

rSimulationParallel2 = function() {
  #
  # step 0 Test out compilation
  #
  #  require(compiler)
  #  enableJIT(1)
  #
  # Step 1 Record simulation constants in environment myEnv
  #
  cat("\n Step 1\n Record simulation constants in environment myEnv\n")
  flush.console()
  #
  ibmConstantsNew1()
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
  computedEnv$costBigRut=costBigRut
  computedEnv$deltaBigRut=deltaBigRut
  computedEnv$portBigRut=portBigRut
  #
  #
  # Step 6 Loop over contour pairs
  #
  cat("\n Step 6\n Loop over contour pairs\n\n")
  flush.console()
  #
  # clusterExport(cl=cl,varlist=varToExport)    
#  answer = foreach(iUDPair = 1:nUDPairsToUseRut,.combine=rbind) %dopar% {  # loop over ud pairs
  contourLoop = function(iUDPair) {
    cat(iUDPair,' out of ',nUDPairsToUseRut,'\n')
    flush.console()
    ud = computedEnv$udMatrixRut[iUDPair,]
    u = ud[1]
    d = ud[2] 
#    cat("\n u,d pair: ",iUDPair," out of ",nUDPairsToUseRut,'\n')
    rutOut   = createDeltaRutkowskiNew(u,d)
    computedEnv$deltaBigRut[,,iUDPair] = rutOut$delta
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
      computedEnv$costBigRut[,iPath,iUDPair]   = costRut
      computedEnv$portBigRut[,1,iPath,iUDPair] = GMatrix[,iPath]
      computedEnv$portBigRut[,2,iPath,iUDPair] = HMatrix[,iPath]
    }  # end iPath loop
    ### Computing average total cost
    totalCost = apply(X = computedEnv$costBigRut[,,iUDPair],2,sum)
    avgTotalCost = mean(totalCost)
    ###
    # result is iUDPair, avgTotalDelta, avgTotalCost rbind-ed by foreach loop
    result = c(iUDPair=iUDPair,avgTotalDelta=avgTotalDelta,avgTotalCost=avgTotalCost)
  }  #end iUDPair contourloop function
  #############
  answerList = mclapply(1:nUDPairsToUseRut,contourLoop,mc.cores=1)
  #############
  answer = matrix(NA,nrow=nUDPairsToUseRut,ncol=3)
  for (i in 1:nUDPairsToUseRut) answer[i,] = answerList[[i]]
######
## Alternative to for loop:
## answer = matrix(unlist(answerList),ncol=3,byrow=TRUE)
  #
  # Plot netDelta
  #
  netDelta = answer[,2] - answer[,3]
  plot(1:nUDPairsToUseRut,netDelta,
       type='l',xlab="(u,d)",ylab=expression(paste("Net ",Delta)),
       main="Net Delta vs. (u,d) pair")
  maxLoc = which.max(netDelta)
  udMax  = udMatrixRut[maxLoc,]
  uMax   = round(udMax[1],digits=3)
  dMax   = round(udMax[2],digits=3)
  points(x = maxLoc,y = netDelta[maxLoc], pch=19, cex=1.3,col='blue')
  label  = paste('(',uMax,',',dMax,')',sep="")
  text(x = maxLoc,y = netDelta[maxLoc],labels = label,pos = 1)
  ###############
  #  STOP COMPUTATION AT THIS POINT
  #  NO DATA SAVED IN LOOP OTHER THAN THE result TUPLE.
  ###  return(answer)
  ###
  # Step 7 Store output in computedEnv
  #
  cat("\n Step 7\n Store output in computedEnv\n")
  flush.console()
  #
  ####
  #
  # Step 8 Save myEnv and computedEnv to data files
  #
  cat("\n Step 8\n Save run ",myEnv$runNumber," myEnv and computedEnv to data files\n")
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


contourLoopCode = function(iUDPair) {
  ud = udMatrixRut[iUDPair,]
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
