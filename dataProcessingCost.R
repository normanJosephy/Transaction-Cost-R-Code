# dataProcessingCost.R  6/29/2012

# 7/6/2012 Edits to some comments
#          Added return list of graphics to processCost()
# 7/20/2012 Added saving 6 graphics
# 7/21/2012 Put print statements inside if (printIt) loop.

# 7/22/2012 Replaced file read with explicit passing in arguments of needed arrays.

require(lattice)

source('lmplot.R')  # densityPairPlot()

processCost = function(costBigRut,
                       deltaBigRut,
                       costBigCRR,
                       deltaBigCRR,
                       udMatrixCRR,
                       udMatrixRut,
                       constants,
                       runNumber,
                       printIt=FALSE,
                       saveIt=TRUE) {
  #
  #
  quantileProbs = seq(0,1,by=.1)
  runText = paste('Run ',runNumber,' ')
  #
  uVectorCRR = udMatrixCRR[,1]
  uVectorRut = udMatrixRut[,1]
  dVectorCRR = udMatrixCRR[,2]
  dVectorRut = udMatrixRut[,2]
  nUDPairsCRR  = nrow(udMatrixCRR)
  nUDPairsRut  = nrow(udMatrixRut)
  #
  # Density of cost; population (nFlips+1) * nPaths * nUDPairs
  #
  dp1 = densityPairPlot(dataSet1=costBigCRR,
                  dataSet2=costBigRut,
                  xlab='transaction cost',
                  legendText=c('CRR','Rut'),
                  mainText=paste(runText,'Transaction Cost'),
                  printIt=TRUE)
  #
  # CRR and Rut setup cost distribution
  #
  setupCostCRR = costBigCRR[1,,]
  setupCostRut = costBigRut[1,,]
  #
  # Create inputs to densityPairPlot function in file lmplot.R 
  #
  dp2 = densityPairPlot(dataSet1=setupCostCRR,
                  dataSet2=setupCostRut,
                  xlab='set-up transaction cost',
                  legendText=c('CRR','Rut'),
                  mainText=paste(runText,'Set-up Transaction Cost'),
                  printIt=printIt)
  ####################################################
  #
  # CRR and Rut contour
  xList = list()
  yList = list()
  xList[[1]] = uVectorCRR
  xList[[2]] = uVectorRut
  yList[[1]] = dVectorCRR
  yList[[2]] = dVectorRut

  dp3 = lPlot(xList,yList,legendText=c('CRR','Rut'),
              main=paste(runText,'Contour'),
              xlab='u',ylab='d',lwd=c(1,4),
              printIt=printIt)
  #####################################################
  #
  # Compute average set-up cost; average over paths
  #
  avgSetupCostCRR = apply(setupCostCRR,2,mean)
  avgSetupCostRut = apply(setupCostRut,2,mean)
  cList = list()
  cList[[1]] = avgSetupCostCRR
  cList[[2]] = avgSetupCostRut
  dp4 = lPlot(xList,cList,legendText=c('CRR','Rut'),
              main=paste(runText,'Average Setup Transaction Cost'),
              xlab='u',ylab='average setup transaction cost',
              printIt=printIt)
  #####################################################
  #
  # Sum individual costs on each path to get totalCost
  #
  # totalCostRut is nPaths x nUDPairs matrix; ditto totalCostCRR
  totalCostRut = apply(costBigRut,c(2,3),sum)
  totalCostCRR = apply(costBigCRR,c(2,3),sum)
  dp5 = densityPairPlot(dataSet1=totalCostCRR,
                  dataSet2=totalCostRut,
                  xlab='total cost',
                  legendText=c('CRR','Rut'),
                  mainText=paste(runText,'Total Cost'),
                  printIt=printIt)
  ########################################################
  #
  # Average total cost over paths to get avg. total cost vs. u
  #
  avgTotalCostRut = apply(totalCostRut,2,mean)
  avgTotalCostCRR = apply(totalCostCRR,2,mean)
  aList = list()
  aList[[1]] = avgTotalCostCRR
  aList[[2]] = avgTotalCostRut
  dp6 = lPlot(xList,aList,legendText=c('CRR','Rut'),
              main=paste(runText,'Average Total Cost'),
              xlab='u',ylab='average total cost',
              printIt=printIt)
  #
  outputGraphicsList = list(dp1=dp1,dp2=dp2,dp3=dp3,dp4=dp4,dp5=dp5,dp6=dp6)
  if (saveIt) {
    fileNamesName = c('TransactionCostDensity.pdf',
                  'SetupTransactionCostDensity.pdf',
                  'MarketContours.pdf',
                  'AverageSetupTransactionCostVsU.pdf',
                  'TotalCostDensity.pdf',
                  'AverageTotalCostVsU.pdf')
    fileNames = paste('run',sprintf(fmt='%3i',runNumber),fileNamesName,sep='')
    for (i in 1:6) {
      pdf(file=fileNames[i])
      print(outputGraphicsList[[i]])
      dev.off()
      cat('\n Saved graphic from processCost() in file',fileNames[i])
    }
    cat('\n\n')
  }
  outputCostDataList = list(averageSetupTransactionCostCRR=avgSetupCostCRR,
                            averageSetupTransactionCostRut=avgSetupCostRut)
  #
  if (printIt) {
    cat('\n\n CRR cost quantiles\n')
    print(round(quantile(costBigCRR,probs=quantileProbs),digits=4))
    cat('\n\n Rut cost quantiles\n')
    print(round(quantile(costBigRut,probs=quantileProbs),digits=4))
    #
    cat('\n\n CRR set-up transaction cost quantiles\n')
    print(quantile(setupCostCRR,probs=quantileProbs))
    cat('\n\n Rut set-up transaction cost quantiles\n')
    print(quantile(setupCostRut,probs=quantileProbs),printIt=printIt)
    #
    cat('\n\n CRR total cost quantiles\n')
    print(quantile(totalCostCRR,probs=quantileProbs))
    cat('\n\n Rut total cost quantiles\n')
    print(quantile(totalCostRut,probs=quantileProbs))
    #
  }
  
  invisible(list(outputGraphicsList=outputGraphicsList,
            outputCostDataList=outputCostDataList))
}

