# dataPlots.R

dataPlots = function() {
  unpackList(myEnv)
  unpackList(computedEnv)
  #
  # Plot transaction cost for each (u,d) pair: summed over time, averaged over paths
  # Repeat for delta
  #
  c1 = apply(costBigRut,c(2,3),sum)
  c2 = apply(c1,2,mean)
  plot(1:length(c2),c2,type='l',main="Average Total Cost",xlab='(u,d)',ylab="Cost")
  d1 = apply(deltaBigRut,c(2,3),sum)
  d2 = apply(d1,2,mean)
  plot(1:length(d2),d2,type='l',main="AverageTotalDelta",xlab='(u,d)',ylab='Delta')
  #
  # Plot market contour and option price surface
  #
  cat('\n Rutkowski base price',priceBaseRut,'\n')
  cat('\n Rutkowski contour plotted from',nrow(udMatrix2),'(u,d) pairs\n')
  optionPriceRounded = round(priceBaseRut,digits=3)
  mainT1 = paste('Rutkowski contour for option price', optionPriceRounded)
  mainT2 = paste('Rutkowski surface plots')
  plotContour(udMatrix2,mainTitle=mainT1)
  cat("\n Rutkowski contour using",nUDPairsToUse,"(u,d) pairs\n")
  plotContour(udMatrixRut,mainTitle="Subset of market contour")
  plotSurface(dLRutkowski,mainTitle=mainT2)
  plotSurface1(dLRutkowski,mainTitle=mainT2)
  cat('\n Rutkowski option price surface plotted')
  #
  # Plot simulated paths and actual path
  #
  pc = min(ncol(paths),40)
  p     = paths[,1:pc] # Use 40 paths in plot or all if less than 40 exist.
  pp    = plotPaths(paths=p,actualPath=actualPath)
  print(pp)
}


# cL returned by getContour()
plotContour = function(cL,mainTitle=NULL) {
  plot(cL[,2],cL[,1],type='l',xlab='d',ylab='u',main=mainTitle)
}

# dL returned by cSurface()
plotSurface1 = function(dL,mainTitle=NULL) {
  persp(dL$u,dL$d,dL$c,
        xlab="u",ylab="d",zlab="c",
        shade = 0.15, border = TRUE,scale=TRUE,axes=TRUE,
        ticktyp="detailed",nticks=2,main=mainTitle)
  return(NULL)
}

plotSurface = function(dL,mainTitle=NULL) {
  w = wireframe(dL$c,row.values=dL$u,column.values=dL$d,
                scales=list(arrows=FALSE),main=mainTitle,
                xlab='u',ylab='d',zlab='c',
                drape=TRUE,colorkey=TRUE)
  print(w)
}


# plot simulated paths using lattice
plotPaths = function(paths,actualPath){
  x = 1:nrow(paths)
  dataList = list(actualPath=actualPath)
  yLim = range(c(actualPath,paths))
  myMain = paste('Simulated',myEnv$stockName,'Stock Price Paths')
  p = xyplot(c(paths) ~ rep(x,ncol(paths)),
             groups=c(col(paths)), 
             type='l',
             lwd=1,
             xlab='Time',
             ylab='Stock price',
             main=myMain,
             ylim=yLim)
  pp = p + layer(panel.points(x=x,y=actualPath,pch=19,cex=1.3,col='black'),data=dataList)
  invisible(pp)
}

