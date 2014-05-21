# processData.R  6/22/2012

# Removed udMatrix1,udMatrix2 from printing list

# 6/26/2012 Again corrected avgCostRut, etc. by adding [i] in nUDPairs loop
#           Removed the subtraction of cost in Rutkowski net delta computation.
#           Split loop in processData into two loops. nUDPairs might be 
#           different for Rut and CRR, since number used might be different.
# 7/20/2012 Added saving optimal values to a file in processData().
#
# 7/21/2012 Added: return outputVector on exit of processData()
#           Changed order of arguments in processData: runNumber first.
# 7/22/2012 Removed check if loaded; now passing all needed data as arguments.

processData = function(costBigRut,
                       deltaBigRut,
                       costBigCRR,
                       deltaBigCRR,
                       udMatrixCRR,
                       udMatrixRut,
                       constants,
                       runNumber) {
  fileNameOutput = paste('run',runNumber,'OptimalResult.Rdata',sep='')
  # Process Rutkowski data
  dimC     = dim(costBigRut)
  nTime    = dimC[1]
  npaths   = dimC[2]
  nUDPairs = dimC[3]
  avgCostRut  = vector(mode='numeric',length=nUDPairs)  
  avgDeltaRut = vector(mode='numeric',length=nUDPairs)
  for (i in 1:nUDPairs) {
    avgCostRut[i]  = mean(apply(costBigRut[,,i],2,sum))
    avgDeltaRut[i] = mean(apply(deltaBigRut[,,i],2,sum))
                          }  # end nUDPairs loop
  # Process CRR data
  dimC     = dim(costBigCRR)
  nTime    = dimC[1]
  npaths   = dimC[2]
  nUDPairs = dimC[3]  
  avgCostCRR  = vector(mode='numeric',length=nUDPairs)
  avgDeltaCRR = vector(mode='numeric',length=nUDPairs)
  for (i in 1:nUDPairs) {
    avgCostCRR[i]  = mean(apply(costBigCRR[,,i],2,sum))
    avgDeltaCRR[i] = mean(apply(deltaBigCRR[,,i],2,sum))
  }   # end nUDPairs loop
  # Process CRR data
  netDeltaCRR            = avgDeltaCRR - avgCostCRR
  maxNetDeltaLocationCRR = which.max(netDeltaCRR)
  maxNetDeltaCRR         = netDeltaCRR[maxNetDeltaLocationCRR]
  udOptimalCRR           = udMatrixCRR[maxNetDeltaLocationCRR,]
  # Process Rutkowski data
  netDeltaRut            = avgDeltaRut
  maxNetDeltaLocationRut = which.max(netDeltaRut)
  maxNetDeltaRut         = netDeltaRut[maxNetDeltaLocationRut]
  udOptimalRut           = udMatrixRut[maxNetDeltaLocationRut,]
  # save optimal values
  outputVector = c(maxNetDeltaLocationCRR=maxNetDeltaLocationCRR,
                   maxNetDeltaCRR=maxNetDeltaCRR,
                   uOptimalCRR=udOptimalCRR[1],
                   dOptimalCRR=udOptimalCRR[2],
                   maxNetDeltaLocationRut=maxNetDeltaLocationRut,
                   maxNetDeltaRut=maxNetDeltaRut,
                   uOptimalRut=udOptimalRut[1],
                   dOptimalRut=udOptimalRut[2],
                   nUDPairsCRR=nrow(udMatrixCRR),
                   nUDPairsRut=nrow(udMatrixRut))
  save(outputVector,file=fileNameOutput)
  cat('\n\n Saved outputVector from processData() in file',fileNameOutput,'\n\n')
  #
  cat('\n\n Run number',runNumber)
#  cat('\n constants\n')
#  print(round(constants,digits=3))
  cat('\n\n CRR Results\n')
  cat('Optimal net average delta:'
      ,round(maxNetDeltaCRR,digits=3),
      'at location'
      ,maxNetDeltaLocationCRR,
      'out of',nrow(udMatrixCRR),
      '\nOptimal u,d pair:',
      round(udOptimalCRR,digits=3),'\n\n')
  cat(' Rut Results\n')
  cat('Optimal net average delta:',
      round(maxNetDeltaRut,digits=3),
      'at location',
      maxNetDeltaLocationRut,
      'out of',nrow(udMatrixRut),
      '\nOptimal u,d pair:',
      round(udOptimalRut,digits=3),'\n\n')
  return(outputVector)
}   # end function processData

processPort = function(pathNumber=1,udPairNumber=1,notLoaded=FALSE,runNumber=NA) {
  if (notLoaded) {
    fileName = paste('run',runNumber,'rut.Rdata',sep='')
    load(fileName)
  }
  # Portfolio for first u,d pair on first path
  `stock $` = portBigCRR[,1,pathNumber,udPairNumber]*pathMatrix[,pathNumber]
  path = pathMatrix[,pathNumber,drop=FALSE]
  colnames(path) = 'path'
  x = cbind(path,portBigRut[,,pathNumber,udPairNumber],
            `stock $`,portBigCRR[,,pathNumber,udPairNumber])
  y = round(x,digits=3)
  colnames(y) = c(paste(c('','Rut','Rut','CRR','CRR','CRR'),colnames(y),sep='-'))
  print(y)
  msg1 = 'Don"t forget that the u,d pairs for Rut and CRR are different' 
  cat('\n\n *** ',msg1,  ' ***\n\n')
}

checkPortExpirationValue = function(udPairNumber=10,notLoaded=FALSE,runNumber=NA) {
  if (notLoaded) {
    fileName = paste('run',runNumber,'rut.Rdata',sep='')
    load(fileName)
  }
  dimP        = dim(portBigRut)
  nTime       = dimP[1]
  nComponents = dimP[2]  # = 2, stock,bond
  nPaths      = dimP[3]
  nUDPairs    = dimP[4]
  pathAtExpir = pathMatrix[nTime,]
  moneyness   = (pathAtExpir > 100)*1
  rutAtExpir  = portBigRut[nTime,1,,udPairNumber]
#  names(rutAtExpir) = paste('Rut',names(rutAtExpir),sep='-')
  crrAtExpir  = portBigCRR[nTime,1,,udPairNumber]
#  names(crrAtExpir) = paste('CRR',names(crrAtExpir),sep='-')
  x           = cbind(pathAtExpir,moneyness,rutAtExpir,crrAtExpir)
  colnames(x) = c('pathEnd','moneyness','rutEnd','crrEnd')
  print(x)
}

dataStatsDelta = function(notLoaded=FALSE,runNumber=NA) {
  if (notLoaded) {
    fileName = paste('run',runNumber,'rut.Rdata',sep='')
    load(fileName)
  }
  deltaDistributionCRR = quantile(deltaBigCRR)
  deltaDistributionRut = quantile(deltaBigRut)
  #
  avgSumDeltaCRR = apply(apply(deltaBigCRR,c(2,3),sum),2,mean)
  avgSumDeltaRut = apply(apply(deltaBigRut,c(2,3),sum),2,mean)
  cat('\n\n delta distribution CRR\n')
  print(round(deltaDistributionCRR,digits=3))
  cat('\n\n delta distribution Rut\n')
  print(round(deltaDistributionRut,digits=3))
  cat('\n\n avg DELTA CRR\n')
  print(round(quantile(avgSumDeltaCRR),digits=3))
  cat('\n\n avg DELTA Rut\n')
  print(round(quantile(avgSumDeltaRut),digits=3))
  cat('\n\n')
}

dataStatsCost = function(notLoaded=FALSE,runNumber=NA) {
  if (notLoaded) {
    fileName = paste('run',runNumber,'rut.Rdata',sep='')
    load(fileName)
  }
  costDistributionCRR = quantile(costBigCRR)
  costDistributionRut = quantile(costBigRut)
  #
  avgSumcostCRR = apply(apply(costBigCRR,c(2,3),sum),2,mean)
  avgSumcostRut = apply(apply(costBigRut,c(2,3),sum),2,mean)
  cat('\n\n cost distribution CRR\n')
  print(round(costDistributionCRR,digits=3))
  cat('\n\n cost distribution Rut\n')
  print(round(costDistributionRut,digits=3))
  cat('\n\n avg COST CRR\n')
  print(round(quantile(avgSumcostCRR),digits=3))
  cat('\n\n avg COST Rut\n')
  print(round(quantile(avgSumcostRut),digits=3))
  cat('\n\n')
}