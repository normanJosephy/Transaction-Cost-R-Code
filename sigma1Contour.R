# sigma1Contour.R  6/19/2012

# 6/21/2012  Added plotSurface and plotSurface1.
#            Somehow the original plotSurface 'disappeared' (;-)

cValue = function(n,u,d,R,rho,oType) {
  # compute a single option rational price value
  # initial stock price = 1
  # Both K and S0 are normalized by R=K/S0, S0/S0
  p = (rho - d)/(u - d)
  stopifnot(p>0,p<1)
  # Do entire sum over [0,n]; the pmax will chop out the zeros.
  uv = u ** seq(0,n)
  dv = d ** seq(n,0)
  dbin = dbinom(seq(0,n),n,p)
  #   intrinsicValue = pmax(R - uv*dv,0)
  if (oType == 'call') {
    intrinsicValue = pmax(uv*dv - R,0)
  } else {intrinsicValue = pmax(R - uv*dv,0)}
  cV = sum(intrinsicValue * dbin)
  # Note: un-normalized c would be
  #  cV = sum(pmax(K - uv*dv*s,0) * dbin)
  # where s is initial stock price
  return(cV)
}

# Create array of c(u,d) values
cSurface = function(nFlips=2,R=1,rho=1,nPtsU=9,nPtsD=9,
            uStart=1.1,uEnd=1.9,dStart=0.80,dEnd=0.96,oType='call') {
  cSurfaceArray = array(data=0,dim=c(nPtsU,nPtsD))
  uV = seq(uStart,uEnd,length.out=nPtsU)
  dV = seq(dStart,dEnd,length.out=nPtsD)
  for (i in seq(1,nPtsU)){
    u = uV[i]
    for (j in seq(1,nPtsD)){
      d = dV[j]
      cSurfaceArray[i,j] = cValue(nFlips,u,d,R,rho,oType)
    }
  }
  return(list(u=uV,d=dV,c=cSurfaceArray))
}


# dL list is returned from cSurface().
getContourCRR = function(dL,S0=100,optionPrice=40,scaleBy=100) {
  #
  scaledCLevel = round(optionPrice/S0 * scaleBy,2)
  # scaledCLevel matches contour label value
  cLines = contourLines(dL$u,dL$d,scaleBy*dL$c,levels=scaledCLevel)
  if (length(cLines) == 0) {stop('No CRR contour. Exiting program')
  } else {return(cbind(u=cLines[[1]]$x,d=cLines[[1]]$y))}
}

constantsInWorkspace = function() {
  nFlips <<- 2
  nPtsU  <<- 9
  nPtsD  <<- 9
  dStart <<- 0.80
  dEnd   <<- 0.96
  uStart <<- 1.10
  uEnd   <<- 1.90
  R      <<- 1
  rho    <<- 1
  K      <<- 100
  S0     <<- 100
  optionPrice <<- 10
}

# cL returned by getContour()
plotContour = function(cL,mainTitle=NULL) {
  plot(cL[,2],cL[,1],type='l',xlab='d',ylab='u',main=mainTitle)
}

# dL returned by cSurface()
plotSurface1 = function(dL,mainTitle=NULL) {
  persp(dL$u,dL$d,dL$c,
        xlab="u",ylab="d",zlab="c",
        shade = 0.75, border = TRUE,scale=TRUE,axes=TRUE,
        ticktyp="detailed",nticks=2,main=mainTitle)
  return(NULL)
}

plotSurface = function(dL,mainTitle=NULL) {
  wireframe(dL$c,row.values=dL$u,column.values=dL$d,
            scales=list(arrows=FALSE),main=mainTitle,
            xlab='u',ylab='d',zlab='c',
            drape=TRUE,colorkey=TRUE)
  
}