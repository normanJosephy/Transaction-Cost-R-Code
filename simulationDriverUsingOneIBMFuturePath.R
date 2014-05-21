setupUseIBM = function() {
  dataDir = 'c:/Research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
  setwd(dataDir)
  #
  # source('rutkowskiCreateGBMPaths.R')
  source('rutkowskiOptionPriceAndSigma2.R')
  source('sigma1Contour.R')
  source('crr-1.R')
  source('residualsCRR.R')
  source('rutkowski-2.R')
  #
  source('processDataRutOnly.R')
  source('lmplot.R')
  source('dataProcessingCost.R')
  source('dataProcessingDelta.R')
  #
  source('createPathsForDailyDataFromIBMPricesUpdated2013.R')
  #
  require(fOptions)
  require(quantmod)   # getSymbols
  require(zoo)
  require(xts)
  require(lubridate)
  require(lattice)
  require(latticeExtra)
}


simulationDriverUsingOneIBMFuturePath = function(runNumber=NA,saveUnderRun=801) {  
  # 11/24/2013 Stripped out all code that computes udMatrix and paths
  # This function uses pre-computed udMatrix and one stock path (ibmFuture)
 
  # IMPORTANT: LOAD run701rut.Rdata TO LOAD udMatrixRut computed in run 701
  # IMPORTANT: RUN setupUseIBM() TO CREATE run701constants.Rdata
  # IMPORTANT: LOAD IBMActualPath.Rdata TO LOAD ibmFuture path
  
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
  # set number of paths to 1
  nPaths    = 1
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
  
  # Create one column path matrix containing ibmFuture path
  pathMatrix = matrix(coredata(ibmFuture),ncol=1)
  ibm        = ibmFuture
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
  
  ## Create Rutkowski market contour udMatrix2 
  #  
  # Use CRR option price as Rutkowski option price.
  priceBaseRut = initialCapital
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
  #constants['nUDPairsToUseCRR'] = nUDPairsToUseCRR
  constants['nUDPairsToUseRut'] = nUDPairsToUseRut
  ##
  # Determine optimal values and locations
  outputDataVector   = processDataRutOnly(costBigRut=costBigRut,
                                          deltaBigRut=deltaBigRut,
                                          udMatrixRut=udMatrixRut,
                                          constants=constants,
                                          runNumber=saveUnderRun)
  #
  outputCostDataList = processCostRutOnly(costBigRut=costBigRut,
                                          deltaBigRut=deltaBigRut,
                                          udMatrixRut=udMatrixRut,
                                          constants=constants,
                                          runNumber=saveUnderRun,
                                          printIt=FALSE,
                                          saveIt=TRUE)
  #   outputDeltaGraphs  = processDeltaRutOnly(costBigRut=costBigRut,
  #                                     deltaBigRut=deltaBigRut,
  #                                     udMatrixRut=udMatrixRut,
  #                                     constants=constants,
  #                                     runNumber=saveUnderRun,
  #                                     r=r,
  #                                     printIt=FALSE,
  #                                     saveIt=TRUE)
  # Extract data for setup transaction cost at optimal u,d pair.
  RutSetupTransactionCost = 
    outputCostDataList$outputCostDataList$averageSetupTransactionCostRut
  #CRRSetupTransactionCost = 
  #  outputCostDataList$outputCostDataList$averageSetupTransactionCostCRR
  #optimalCRRAt = outputDataVector['maxNetDeltaLocationCRR']  
  optimalRutAt = outputDataVector['maxNetDeltaLocationRut']
  #setupTransactionCostCRR = CRRSetupTransactionCost[optimalCRRAt]
  setupTransactionCostRut = RutSetupTransactionCost[optimalRutAt]
  #cat('\n\n Setup transaction cost CRR',setupTransactionCostCRR)
  cat('\n Setup transaction cost Rut',  setupTransactionCostRut)
  cat('\n\n')
  #
  ## Dump data in current directory
  # Add setup transaction costs to constants vector and re-save it
  #constants['averageSetupTransactionCostAtOptimalCRR'] = setupTransactionCostCRR
  constants['averageSetupTransactionCostAtOptimalRut'] = setupTransactionCostRut
  saveFileName = paste('run',saveUnderRun,'rut.Rdata',sep='')
  dataNames = c('constants','pathMatrix','udMatrixRut',
                'udMatrix2','portBigRut',
                'deltaBigRut','costBigRut',
                'rowsToUseRut')
  
  save(list=dataNames,file=saveFileName)
  cat('\n\n Saving data in file ',saveFileName,' for run ',saveUnderRun,'\n\n')
  flush.console()
  
  # 
} # end function simulationDriverUsingOneIBMFuturePath