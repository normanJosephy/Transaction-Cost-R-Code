# lmplot.R  6/29/2012

# 7/1/2012 Generalized densityPairPlot to multiple plots: densityListPlot
# 7/6/2012 Moved and changed columns=3 from text list to myKey list.

densityListPlot = function(dataSetList,printIt=TRUE,
                           xlab,legendText,mainText,...) {
  allData = vector(mode='numeric')
  allGroup = vector(mode='numeric')
  nCurves  = length(dataSetList)
  for (iPlot in 1:nCurves) {
    data = dataSetList[[iPlot]]
    nPts = length(data)
    allData  = c(allData,data)
    allGroup = c(allGroup,rep(iPlot,nPts))
  }
  myKey = list(lines=list(col=seq(1,nCurves),lwd=2),
               columns=nCurves,
               text=list(label=legendText),
               space='top')
  dp = densityplot(~allData,group=allGroup,col=seq(1,nCurves),
                   xlab=xlab,ylab='density',key=myKey,lwd=2,
                   main = mainText,...)
  if (printIt) print(dp)
  invisible(dp)
}

densityPairPlot = function(dataSet1,dataSet2,printIt=TRUE,
                           xlab,legendText,mainText,...) {
  # dataSet1 and dataSet2 are nPaths x nUDPairs matrices.
  # Convert each matrix into a vector and concatenate vectors into one.
  vec1    = c(dataSet1)
  length1 = length(vec1)
  vec2    = c(dataSet2)
  length2 = length(vec2)
  allData = c(vec1,vec2)
  # grouping vector identifies each of the two data sets  
  groupVector   = c(rep(1,length1),rep(2,length2))
  # create density plot parameters and plot set-up cost data
  nCurves = 2
  myKey = list(lines=list(col=seq(1,nCurves)),
               text=list(label=legendText),
               space='top',
               columns=nCurves)
  dp = densityplot(~allData,group=groupVector,col=seq(1,nCurves),
              xlab=xlab,ylab='density',key=myKey,lwd=2,
              main = mainText,...)
  if (printIt) print(dp)
  invisible(dp)
}

# Uses panel functions to plot columns of a matrix
# If highlightPath is not NULL, it is graphed with lwd = 3
mPlot = function(x,yM,highlightPath=NULL,...,printIt=TRUE) {
  myPanel = function(xDummy,yDummy) {
    nPlots = ncol(yM)
    for (i in 1:nPlots) {
      panel.lines(x,yM[,i],col=i)
    }
    if (! is.null(highlightPath)) {
      panel.lines(x,highlightPath,lwd=3)
    }
  }
  yRange = range(yM)
  xRange = range(x)
  myP = xyplot(yRange ~ xRange, panel=myPanel,...)
  if(printIt) print(myP)
  invisible(myP)
}


# Using lattice to plot curves in a list
# Curves can have different number of points
# and overlapping x ranges
lPlot = function(xL,yL,legendText='',...,printIt=TRUE) {
  xRange  = range(xL)
  yRange  = range(yL)
  nCurves = length(yL)
  if (nCurves == 1) {
    palette()
  } else {palette(rainbow(nCurves))}
  
  # Note that each curve has a different number of points.
  myPanel = function(xDummy,yDummy) {
    for (i in 1:nCurves){
      x = xL[[i]]
      y = yL[[i]]
      panel.lines(x,y,col=i)}
    panel.abline(h=0)
    panel.grid()
  }
  myKey = list(lines=list(col=seq(1,nCurves)),
               text=list(label=legendText,col=seq(1,nCurves)),
               space='top',
               columns=nCurves)
  # Note: yRange ~ xRange sets the x-axis and y-axis extents.
  myP = xyplot(yRange ~ xRange, panel=myPanel,key=myKey,...)
  if (printIt) print(myP)
  invisible(myP)
}
