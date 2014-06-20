# dataProcessingDeltaRutOnly-1.R  6/18/2014

# require(lattice)

# source('lmplot.R')

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