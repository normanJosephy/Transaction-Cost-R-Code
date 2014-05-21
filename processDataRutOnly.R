# processDataRutOnly.R  11/24/2013

# Added drop=FALSE to lines 19,20

processDataRutOnly = function(costBigRut,
                       deltaBigRut,
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
    avgCostRut[i]  = mean(apply(costBigRut[,,i,drop=FALSE],2,sum))
    avgDeltaRut[i] = mean(apply(deltaBigRut[,,i,drop=FALSE],2,sum))
  }  # end nUDPairs loop
#
  # Process Rutkowski data
  netDeltaRut            = avgDeltaRut
  maxNetDeltaLocationRut = which.max(netDeltaRut)
  maxNetDeltaRut         = netDeltaRut[maxNetDeltaLocationRut]
  udOptimalRut           = udMatrixRut[maxNetDeltaLocationRut,]
  # save optimal values
  outputVector = c(maxNetDeltaLocationRut=maxNetDeltaLocationRut,
                   maxNetDeltaRut=maxNetDeltaRut,
                   uOptimalRut=udOptimalRut[1],
                   dOptimalRut=udOptimalRut[2],
                   nUDPairsRut=nrow(udMatrixRut))
  save(outputVector,file=fileNameOutput)
  cat('\n\n Saved outputVector from processData() in file',fileNameOutput,'\n\n')
  #
  cat('\n\n Run number',runNumber)
#
  cat(' Rut Results\n')
  cat('Optimal net average delta:',
      round(maxNetDeltaRut,digits=3),
      'at location',
      maxNetDeltaLocationRut,
      'out of',nrow(udMatrixRut),
      '\nOptimal u,d pair:',
      round(udOptimalRut,digits=3),'\n\n')
  return(outputVector)
}   # end function processDataRutOnly