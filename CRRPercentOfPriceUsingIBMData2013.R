# CRRPercentOfPriceUsingIBMData2013.R  7/10/2012

# 7/11/2012 Added setupUseCRR() and rutkowskiConstantsCRRUsed()
#           Added computeGoodUDRangeForCRR() and computeGoodUDRangeForRut()
#           Changed constants vector:
#               Removed uStart,uEnd,dStart,dEnd
#               Added rAnnual and computed r as rAnnual/nFlips

# 7/20/2012 Added processData(), processCost(), and processDelta()
#           at end of function CRRPercentOfPriceUsedByRut().

# 7/23/2012 Added S0 argument to call to volatilityOneCompany().
#           Added nDaysInYear to constants and call to volatilityOneCompany().

# 11/09/2013 Switched to using Friday stock prices for IMB1
# and end of month stock prices for IBM2

# 5/22/2014 Changed dataDir to Transaction-Cost-R-Code under git
#           Changed run number to 999.
#           Added '~/data/' to path to data created by code.
#           Added subdirectory 'data' to store data created.

setupUseIBM = function() {
  dataDir = "C:/research/Lucy-2014/Transaction-Cost-R-Code"
  setwd(dataDir)
  #
  source('rutkowskiCreateGBMPaths.R')
  source('rutkowskiOptionPriceAndSigma2.R')
  source('sigma1Contour.R')
  source('crr-1.R')
  source('residualsCRR.R')
  source('rutkowski-2.R')
  #
  source('processData.R')
  source('lmplot.R')
  source('dataProcessingCost.R')
  source('dataProcessingDelta.R')
  #
  source('createPathsFromIBMPricesUpdated2013.R')
  #
  require('fOptions')
  require('lattice')
}

# source('rutkowskiCreateGBMPaths.R') 
#  for modifiedPathSimulation()
# source('rutkowskiOptionPriceAndSigma2.R')  
#  for computeBaseUD() computeBasePrice()  
#      computeGridPrice() getContourRutkowski()     
# source('sigma1Contour.R')
#  for cSurface()  getContourCRR()
# source('crr-1.R')
#  for optionTree()  
# source('residualsCRR.R')
#  for createGi()  createPortfolioCRR()  
#      createDelta()  createTCosts()
# source('rutkowski-2.R')
#  for createDeltaRutkowski()

ibmConstantsCRRUsed = function() {
  runNumber = 999
  # Option data
  nDaysInYear = 252
  S0          = 179.99       # stock price at time option price is noted.
  K           = 180          # call option strike price
  optionPrice = 5.04         # option price
  nFlips      = 8            # number of rebalancing times.
  TimeToExpiration = 1/6     # time to expiration in years
  R           = K/S0         # normalized strike - used in CRR contour computation
  #
  nPaths = 100        # number of paths constructed  
  lambda = 0.01       # unit transaction cost for buying a share of stock.
  mu     = 0.01       # unit transaction cost for selling a share of stock. 
  nUDPairsToUse = 50     # number of contour (u,d) pairs to use in algorithm.
  #  
  rAnnual = 0.00
  r      = rAnnual/nFlips # single period risk-free interest rate
  rho    = 1 + r        # risk-free single period accumulation factor
  nPtsD  = 100          # number of u values used in computing (u,d,c) surface.
  nPtsU  = 100          # number of d values used in computing (u,d,c) surface.
  seed   = 12345        # seed set in rutkowskiSimulation()
  drift  = 0.00         # annual stock drift
#
  percentCRR = -0.10     # (1+percentage)*CRR is initial capital
  constants  = c(nPaths=nPaths,TimeToExpiration=TimeToExpiration,
                 percentCRR=percentCRR,optionPrice=optionPrice,
                 S0=S0,K=K,R=R,seed=seed,nDaysInYear=nDaysInYear,
                 lambda=lambda,mu=mu,nFlips=nFlips,r=r,rho=rho,
                 rAnnual=rAnnual,runNumber=runNumber,
                 nPtsD=nPtsD,nPtsU=nPtsU,nUDPairsToUse=nUDPairsToUse)
  fileName = paste('run',runNumber,'constants.Rdata',sep='')
  fullFileName = paste('~/data/',fileName,sep='')
  save(constants,file=fullFileName)
  cat("\n constants saved to file",fullFileName,'\n\n')
  return(constants)
}


computeUDRange = function(u,d,divisor) {
  uIncrement = (u-1)/divisor
  uStart     = u - uIncrement
  uEnd       = u + uIncrement
  dIncrement = (1 - d)/divisor
  dStart     = d - dIncrement
  dEnd       = d + dIncrement
  cat('\n\n u range:',uStart,uEnd,
      '\n d range:',dStart,dEnd,
      '\n  base u:',u,' base d:',d,
      '\n',' divisor:',divisor,'\n')
  cat('\n')
  flush.console()
  output = c(uStart,uEnd,dStart,dEnd)
  return(output)
}

########################################
computeGoodUDRangeForCRR = function(priceBaseCRR,u,d,nFlips,R,rho,nPtsU,nPtsD,
                                    S0,K,r,oType,scaleBy) {
  divisorVector  = c(4,3,2,1.5,1.2,1.1,1.05)
  goodRange      = FALSE
  foundGoodRange = FALSE
  count          = 1
#   oTree         = optionTree(S0,K,u,d,r,nFlips)
#   priceBaseCRR  = unname(rev(oTree)[[1]])
  cat('\n\n CRR option price:',priceBaseCRR,'\n\n')
  while( (count <= length(divisorVector)) && (! goodRange)) {
    divisor = divisorVector[count]
    v       = computeUDRange(u,d,divisor)
    # Make sure uStart doesn't drop below rho.
    # We need the no arbitrage condition (1+r) < u
    uStart = max(v[1],rho+.005)
    uEnd   = v[2]
    dStart = v[3]
    dEnd   = v[4]
    dLCRR  = cSurface(nFlips=nFlips,R=R,rho=rho,
                         nPtsU=nPtsU,nPtsD=nPtsD,
                         uStart=uStart,uEnd=uEnd,
                         dStart=dStart,dEnd=dEnd,
                         oType=oType)
    oPrices        = scaleBy*c(dLCRR[[3]])
    maxOptionPrice = max(oPrices)
    minOptionPrice = min(oPrices)
    cat('\n option price range:',minOptionPrice,maxOptionPrice,'\n------------')
    cond1 = (priceBaseCRR <= maxOptionPrice)
    cond2 = (minOptionPrice <= priceBaseCRR)
    if (cond1 && cond2) {
      if (foundGoodRange) {goodRange = TRUE
      } else {foundGoodRange=TRUE; count=count+1}
    } else {
      count = count + 1
    }  # end if
               }  # end while
  stopifnot(goodRange)
  udMatrix1 = getContourCRR(dL=dLCRR,S0=S0,optionPrice=priceBaseCRR,
                            scaleBy=scaleBy)
  invisible(udMatrix1)
  }

computeGoodUDRangeForRut = function(priceBaseRut,u,d,nFlips,R,rho,nPtsU,nPtsD,
                                    lambda,mu,S0,K,r,oType) {
  divisorVector  = c(4,3,2,1.5,1.2,1.1,1.07,1.05,1.03)
  goodRange      = FALSE
  foundGoodRange = FALSE # This will allow one extra iteration.
  count          = 1
  cat('\n\n Rut option price:',priceBaseRut,'\n\n')
  while( (count <= length(divisorVector)) && (! goodRange)) {
    divisor = divisorVector[count]
    v       = computeUDRange(u,d,divisor)
    # Make sure uStart doesn't drop below rho.
    # We need the no arbitrage condition (1+r) < u
    uStart = max(v[1],rho+.005)
    uEnd   = v[2]
    dStart = v[3]
    dEnd   = v[4]
    dLRut  = computeGridPrice(uStart=uStart,uEnd=uEnd,
                                   dStart=dStart,dEnd=dEnd,
                                   nPtsU=nPtsU,nPtsD=nPtsD,
                                   lambda=lambda,mu=mu,
                                   r=r,S0=S0,K=K,nFlips=nFlips)
    oPrices        = c(dLRut[[3]])
    maxOptionPrice = max(oPrices)
    minOptionPrice = min(oPrices)
    cat('\n option price range:',minOptionPrice,maxOptionPrice,'\n------------')
    cond1 = (priceBaseRut <= maxOptionPrice)
    cond2 = (minOptionPrice <= priceBaseRut)
    if (cond1 && cond2) {
      if (foundGoodRange) {goodRange = TRUE
                            } else {foundGoodRange=TRUE; count=count+1}
    } else {
      count = count + 1
    }  # end if
  }  # end while
  stopifnot(goodRange)
  udMatrix2 = getContourRutkowski(dL=dLRut,S0=S0,oPrice=priceBaseRut)
  invisible(udMatrix2)
}


CRRPercentOfPriceUsingIBMData = function(runNumber=NA) {  
  
  # 7/10/2012 Replaced createGBMPaths with modifiedPathSimulation
  #           in file rutkowskiCreateGBMPaths.R
  
  #            Read run##constants.rData file for simulation constants 
  # 7/11/2012 Implemented automatic choice of u and d ranges; see
  #           computeGoodUDRangeForRut() and computeGoodUDRangeForCRR()
  
  # 7/23/2012 Modified to use IBM data and historical volatility.
  
  # 7/24/2012 Added S0 argument to createPathsAndJumpsFromIBMData() call.
  # 7/24/2012 Added nDaysInYear to volatilityOneCompany() call.
  
  if (is.na(runNumber)) {
    runNumber = readline(' Run Number: ')
    runNumber = as.integer(runNumber)
    if (is.na(runNumber)) {
      cat('\n *** runNumber not a number - exiting\n\n')
      invisible(NULL)
    }
  }
  fileName = paste('run',runNumber,'constants.Rdata',sep='')
  load(fileName)
  #  print(constants)
  #
  runNumber = constants['runNumber']
  oType     = 'call'  
  nPaths    = constants['nPaths']
  nFlips    = constants['nFlips']
  TimeToExpiration = constants['TimeToExpiration']
  nDaysInYear = constants['nDaysInYear']
#  sigma     = constants['sigma']
#  drift     = constants['drift']
  S0        = constants['S0']
  lambda    = constants['lambda']
  mu        = constants['mu']
  nPtsU     = constants['nPtsU']
  nPtsD     = constants['nPtsD']
  ###############################
  # Vector constants changed to compute period interest rate as r.
  r   = constants['r']
  rho = constants['rho']
  #############################
  optionPrice = constants['optionPrice']
  K         = constants['K']
  R         = constants['R'] 
  scaleBy   = 100
  nUDPairsToUse = constants['nUDPairsToUse']
  seed      = constants['seed']
  set.seed(seed)
  ##  
  ## Create IBM paths - see createPathsFromIBMPricesUpdated.R
  ##
  print(round(constants,digits=3))
  pIBM = createPathsAndJumpsFromIBMData(nPaths=nPaths,
                                        nNewPointsOnPath=nFlips,
                                        S0=S0)
  pathMatrix = pIBM$paths
  ibm        = pIBM$ibm
  #
  percentCRR = constants['percentCRR']
  #############################################################
  ## Create CRR market contour udMatrix1 - see sigma1Contour.R
  ##

  priceBaseCRR   = optionPrice
  initialCapital = unname(priceBaseCRR * (1 + percentCRR))
  volatility     = volatilityOneCompany(coData=ibm,nDaysInYear=nDaysInYear)
  h              = TimeToExpiration/nFlips
  uStar          = unname(exp(volatility*sqrt(h)))
  dStar          = 1/uStar

#  cat('\n\n\t Base CRR price',priceBaseCRR,'\t uBase,dBase',u,d,'\n')
  cat('\n\n\t percentCRR',percentCRR,'\n')
  cat('\t initial capital',initialCapital,'\t uStar,dStar',uStar,dStar)
  cat("\n\n")
  #
#  constants['priceBaseCRR']      = priceBaseCRR
  constants['initialCapital']    = initialCapital
  constants['uStar']             = uStar
  constants['dStar']             = dStar
  constants['volatility'] = volatility
  #
  udMatrix1      = computeGoodUDRangeForCRR(priceBaseCRR=initialCapital,
                                          u=uStar,
                                          d=dStar,
                                          nFlips=nFlips,
                                          R=R,
                                          rho=rho,
                                          nPtsU=nPtsU,
                                          nPtsD=nPtsD,
                                          S0=S0,
                                          K=K,
                                          r=r,
                                          oType=oType,
                                          scaleBy=scaleBy)
  #################################################
  ## Create Rutkowski market contour udMatrix2 
  #  
  # Use CRR option price as Rutkowski option price.
  priceBaseRut = initialCapital
  udMatrix2 = computeGoodUDRangeForRut(priceBaseRut=priceBaseRut,
                                       u=uStar,
                                       d=dStar,
                                       nFlips=nFlips,
                                       R=R,
                                       rho=rho,
                                       nPtsU=nPtsU,
                                       nPtsD=nPtsD,
                                       lambda=lambda,
                                       mu=mu,
                                       S0=S0,
                                       K=K,
                                       r=r,
                                       oType=oType)
  # Plot market contours for Rutkowski and CRR.
  CRRAndRutContours(udMatrix1=udMatrix1,
                    udMatrix2=udMatrix2,
                    runNumber=runNumber)
  ##
  ## CRR:
  ## Calculate delta and tCost for each (u,d) in udMatrix1
  ## and each path in pathMatrix.
  ## - see residualsCRR.R
  ##
  # Choose nUDPairsToUse number of (u,d) pairs from udMatrix1
  # As set above, nUDPairsToUse=10
  nUDPairsCRR    = nrow(udMatrix1)
  rowsToUseCRR   = unique(round(seq(1,nUDPairsCRR,length=nUDPairsToUse)))
  nUDPairsToUseCRR = length(rowsToUseCRR)
  stopifnot(length(rowsToUseCRR) > 0)
  udMatrixCRR    = udMatrix1[rowsToUseCRR,,drop=FALSE]
  colnames(udMatrixCRR) = c('u','d')
  uValues     = round(udMatrixCRR[,1],digits=4)
  dValues     = round(udMatrixCRR[,2],digits=4)
  # array names
  rowNamesDelta = paste('time-',1:nFlips,sep='')
  colNamesDelta = paste('path-',1:nPaths,sep='')
  lstNamesDelta = paste('u-',uValues,' d-',dValues,sep='')
  rowNamesPort  = paste('time-',0:nFlips,sep='')
  colNamesPort  = c('stock #','bond $')
  thirdDimPort  = colNamesDelta
  forthDimPort  = lstNamesDelta
  rowNamesCost  = rowNamesPort
  colNamesCost  = colNamesDelta
  lstNamesCost  = lstNamesDelta
  dimNamesDelta = list(rowNamesDelta,colNamesDelta,lstNamesDelta)
  dimNamesPort  = list(rowNamesPort,colNamesPort,thirdDimPort,forthDimPort)
  dimNamesCost  = list(rowNamesCost,colNamesCost,lstNamesCost)
  #
  portBigCRR  = array(data=NA,dim=c(nFlips+1,2,nPaths,nUDPairsToUseCRR),
                      dimnames=dimNamesPort)
  costBigCRR  = array(data=NA,dim=c(nFlips+1,nPaths,nUDPairsToUseCRR),
                      dimnames=dimNamesCost)
  deltaBigCRR = array(data=NA,dim=c(nFlips,nPaths,nUDPairsToUseCRR),
                      dimnames=dimNamesDelta)
  #
  for (iUDPair in 1:nUDPairsToUseCRR) {  # loop over ud pairs
    cat('\n CRR loop u,d pair number',iUDPair)
    flush.console()
    ud = udMatrixCRR[iUDPair,]
    u = ud[1]
    d = ud[2]
    for (iPath in 1:nPaths) {    # loop over paths
      path         = pathMatrix[,iPath]
      pathLength   = length(path)
      pathDropLast = path[-pathLength]
      psi          = path[-1]/pathDropLast
      gi           = createGi(path,K,u,d,r)
      portfolio    = createPortfolioCRR(gi,path,u,d,r,K)
      delta        = createDelta(gi,u,d,psi)
      tCost        = createTCosts(path,portfolio[,1],lambda,mu)
      deltaBigCRR[,iPath,iUDPair] = unname(delta)
      costBigCRR[,iPath,iUDPair]  = unname(tCost)
      portBigCRR[,,iPath,iUDPair] = portfolio
    }     # end loop over iPath
  }       # end loop over iUDPair
  #
  ##   Rutkowski:
  ## Calculate delta and tCost for each (u,d) in udMatrix2
  ## and each path in pathMatrix.
  ## Choose nUDPairsToUse number of (u,d) pairs from udMatrix2
  #
  nUDPairsRut      = nrow(udMatrix2)
  rowsToUseRut     = unique(round(seq(1,nUDPairsRut,length=nUDPairsToUse)))
  nUDPairsToUseRut = length(rowsToUseRut)
  stopifnot(length(rowsToUseRut) > 0)
  udMatrixRut      = udMatrix2[rowsToUseRut,,drop=FALSE]
  colnames(udMatrixRut) = c('u','d')
  ##
  ## loop over (u,d) pairs
  ##
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
  for (iUDPair in 1:nUDPairsToUseRut) {  # loop over ud pairs
    ud = udMatrixRut[iUDPair,]
    u = ud[1]
    d = ud[2]  
    rutOut   = createDeltaRutkowski(pathMatrix,lambda,mu,r,u,d,K)
    deltaRut = rutOut$delta
    deltaBigRut[,,iUDPair] = deltaRut
    GMatrix  = rutOut$GMatrix    
    HMatrix  = rutOut$HMatrix 
    cat('\n Rutkowski loop u,d pair number',iUDPair)
    flush.console()
    #
    for (iPath in 1:nPaths) {
      path     = pathMatrix[,iPath]
      G        = GMatrix[,iPath]
      shareQt  = G/path
      costRut  = createTCosts(path,shareQt,lambda,mu)
      costBigRut[,iPath,iUDPair]   = costRut
      portBigRut[,1,iPath,iUDPair] = GMatrix[,iPath]
      portBigRut[,2,iPath,iUDPair] = HMatrix[,iPath]
    }  # end iPath loop
  }  #end iUDPair loop
  ##
  ## Collect data to save
  constants['h']            = h
  constants['nUDPairsToUseCRR'] = nUDPairsToUseCRR
  constants['nUDPairsToUseRut'] = nUDPairsToUseRut
  ##
  # Determine optimal values and locations
  outputDataVector   = processData(costBigRut=costBigRut,
                                   deltaBigRut=deltaBigRut,
                                   costBigCRR=costBigCRR,
                                   deltaBigCRR=deltaBigCRR,
                                   udMatrixCRR=udMatrixCRR,
                                   udMatrixRut=udMatrixRut,
                                   constants=constants,
                                   runNumber=runNumber)
  outputCostDataList = processCost(costBigRut=costBigRut,
                                   deltaBigRut=deltaBigRut,
                                   costBigCRR=costBigCRR,
                                   deltaBigCRR=deltaBigCRR,
                                   udMatrixCRR=udMatrixCRR,
                                   udMatrixRut=udMatrixRut,
                                   constants=constants,
                                   runNumber=runNumber,
                                   printIt=FALSE,
                                   saveIt=TRUE)
  outputDeltaGraphs  = processDelta(costBigRut=costBigRut,
                                    deltaBigRut=deltaBigRut,
                                    costBigCRR=costBigCRR,
                                    deltaBigCRR=deltaBigCRR,
                                    udMatrixCRR=udMatrixCRR,
                                    udMatrixRut=udMatrixRut,
                                    constants=constants,
                                    runNumber=runNumber,
                                    r=r,
                                    printIt=FALSE,
                                    saveIt=TRUE)
  # Extract data for setup transaction cost at optimal u,d pair.
  RutSetupTransactionCost = 
    outputCostDataList$outputCostDataList$averageSetupTransactionCostRut
  CRRSetupTransactionCost = 
    outputCostDataList$outputCostDataList$averageSetupTransactionCostCRR
  optimalCRRAt = outputDataVector['maxNetDeltaLocationCRR']  
  optimalRutAt = outputDataVector['maxNetDeltaLocationRut']
  setupTransactionCostCRR = CRRSetupTransactionCost[optimalCRRAt]
  setupTransactionCostRut = RutSetupTransactionCost[optimalRutAt]
  cat('\n\n Setup transaction cost CRR',setupTransactionCostCRR)
  cat('\n Setup transaction cost Rut',  setupTransactionCostRut)
  cat('\n\n')
  #
  ## Dump data in current directory
  # Add setup transaction costs to constants vector and re-save it
  constants['averageSetupTransactionCostAtOptimalCRR'] = setupTransactionCostCRR
  constants['averageSetupTransactionCostAtOptimalRut'] = setupTransactionCostRut
  saveFileName = paste('run',runNumber,'rut.Rdata',sep='')
  dataNames = c('constants','pathMatrix','udMatrixCRR','udMatrixRut',
                'udMatrix1','udMatrix2','portBigCRR','portBigRut',
                'deltaBigCRR','deltaBigRut','costBigCRR','costBigRut',
                'rowsToUseCRR','rowsToUseRut')
  
  save(list=dataNames,file=saveFileName)
  cat('\n\n Saving data in file ',saveFileName,' for run ',runNumber,'\n\n')
  flush.console()
  
  # 
} # end function CRRPercentOfPriceUsingIBMData()

CRRAndRutContours = function(udMatrix1,udMatrix2,runNumber=NA) {
  uRange = range(range(udMatrix1[,1],udMatrix2[,1]))
  dRange = range(range(udMatrix1[,2],udMatrix2[,2]))
  if (! is.na(runNumber)) {
    mainTitle = paste('CRR and Rutkowski Market Contour\n',
                      'Run Number',runNumber)
  } else {
    mainTitle = 'CRR and Rutkowski Market Contour'
  }
  plot(udMatrix1[,1],udMatrix1[,2],
       xlim=uRange,ylim=dRange,
       xlab='u',ylab='d',
       main=mainTitle,
       type='l',col='red',lwd=2)
  lines(udMatrix2[,1],udMatrix2[,2],col='green',lwd=2,lty=1)
  legend('top',bty='n',
         legend=c('CRR','Rut'),
         text.col=c('red','green'),
         col=c('red','green'))
}
