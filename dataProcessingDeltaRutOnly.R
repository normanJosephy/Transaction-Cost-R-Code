# dataProcessingDeltaRutOnly.R  6/29/2012

# 7/01/2012  Added discounting to summation of delta

# 7/20/2012 Added saving graphics to file

# 7/21/2012 Removed r = constants['r'] from processDelta() .
#           Added r=0.0 to default argument list.

require(lattice)

source('lmplot.R')

dSum = function(x,r) {
  sum(x/(1+r)^seq(1,length(x)))
}

processDeltaRutOnly = function(costBigRut,
                        deltaBigRut,
                        udMatrixRut,
                        constants,
                        runNumber,
                        r,
                        printIt=FALSE,
                        saveIt=TRUE) {
  #
  #
  quantileProbs = seq(0,1,by=.1)
  #
#   uVectorCRR = udMatrixCRR[,1]
  uVectorRut = udMatrixRut[,1]
#   dVectorCRR = udMatrixCRR[,2]
  dVectorRut = udMatrixRut[,2]
#   nUDPairsCRR  = nrow(udMatrixCRR)
  nUDPairsRut  = nrow(udMatrixRut)
  #
  # Sum individual delta on each path to get Delta.
  #
  totalDeltaRut = apply(deltaBigRut,c(2,3),sum)
#   totalDeltaCRR = apply(deltaBigCRR,c(2,3),sum)
  totalDeltaRutDiscounted = apply(deltaBigRut,c(2,3),dSum,r=r)
#   totalDeltaCRRDiscounted = apply(deltaBigCRR,c(2,3),dSum,r=r)
  
  dataSetList = list(totalDeltaRut,totalDeltaRutDiscounted)
  #
  # Average summed deltas to get average Delta, which we maximize.
  #
  avgTotalDeltaRut = apply(totalDeltaRut,2,mean)
  avgTotalDeltaCRR = apply(totalDeltaCRR,2,mean)
  avgTotalDeltaRutDisc = apply(totalDeltaRutDiscounted,2,mean)
  avgTotalDeltaCRRDisc = apply(totalDeltaCRRDiscounted,2,mean)
  
  aList = list()
  aList[[1]] = avgTotalDeltaCRR
  aList[[2]] = avgTotalDeltaRut
  aList[[3]] = avgTotalDeltaCRRDisc
  aList[[4]] = avgTotalDeltaRutDisc
  xList[[3]] = xList[[1]]
  xList[[4]] = xList[[2]]
  dp4 = lPlot(xList,aList,legendText=c('CRR','Rut','CRR Disc','Rut Disc'),
        main=mainDeltaBar,
        xlab='u',ylab=expression(bar(Delta)))
  #
 outputGraphicsList = list(dp1=dp1,dp2=dp2,dp3=dp3,dp4=dp4) #
  if (saveIt) {
    fileNamesName = c('DeltaDensity.pdf',
                      'SmallDeltaDensity.pdf',
                      'SumDeltaDensity.pdf',
                      'AverageTotalDeltaVsU.pdf')
    fileNames = paste('run',sprintf(fmt='%3i',runNumber),fileNamesName,sep='')
    for (i in 1:length(fileNamesName)) {
      pdf(file=fileNames[i])
      print(outputGraphicsList[[i]])
      dev.off()
      cat('\n Saving graphic from processDelta() in file',fileNames[i])
    }
    cat("\n\n")
  }
  if (printIt) {
    #
    cat('\n\n CRR total delta quantiles\n')
    print(quantile(totalDeltaCRR,probs=quantileProbs))
    cat('\n\n Rut total delta quantiles\n')
    print(quantile(totalDeltaRut,probs=quantileProbs))
    #
    cat('\n\n CRR delta quantiles\n')
    print(round(quantile(deltaBigCRR,probs=quantileProbs),digits=4))
    cat('\n\n Rut delta quantiles\n')
    print(round(quantile(deltaBigRut,probs=quantileProbs),digits=4))
    listNames = c('avgTotalDelta CRR','avgTotalDelta Rut',
                  'avgTotalDeltaDisc CRR','avgTotalDeltaDisc Rut')
    for (iList in 1:4) {
      cat("\n ",listNames[iList],'\n')
      print(quantile(aList[[iList]],probs=quantileProbs))
    }  # end for(iList)
  }   # end if(printIt)
  invisible(outputGraphicsList)
}