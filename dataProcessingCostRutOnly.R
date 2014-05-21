# dataProcessingCostRutOnly.R  6/29/2012

# 11/24/2013 Stripped out all graphics

processCostRutOnly = function(costBigRut,
                       deltaBigRut,
                       udMatrixRut,
                       constants,
                       runNumber,
                       printIt=FALSE,
                       saveIt=FALSE) {
  #
  #
  quantileProbs = seq(0,1,by=.1)
  runText = paste('Run ',runNumber,' ')
  #
#   uVectorCRR = udMatrixCRR[,1]
  uVectorRut = udMatrixRut[,1]
#   dVectorCRR = udMatrixCRR[,2]
  dVectorRut = udMatrixRut[,2]
#   nUDPairsCRR  = nrow(udMatrixCRR)
  nUDPairsRut  = nrow(udMatrixRut)
  #
  setupCostRut = costBigRut[1,,]
  #
  # Compute average set-up cost; average over paths
  #
  # avgSetupCostCRR = apply(setupCostCRR,2,mean)
  # Cover case where there is only one path, and costBigRut[1,,] is not a matrix
  if (is.null(dim(setupCostRut))) {avgSetupCostRut = mean(setupCostRut)
  } else {avgSetupCostRut = apply(setupCostRut,2,mean)}
  
  #
  # Sum individual costs on each path to get totalCost
  #
  # totalCostRut is nPaths x nUDPairs matrix; ditto totalCostCRR
  totalCostRut = apply(costBigRut,c(2,3),sum)
  #
  # Average total cost over paths to get avg. total cost vs. u
  #
  avgTotalCostRut = apply(totalCostRut,2,mean)
  outputCostDataList = list(averageSetupTransactionCostRut=avgSetupCostRut)
  #
  invisible(list(outputCostDataList=outputCostDataList))
}

