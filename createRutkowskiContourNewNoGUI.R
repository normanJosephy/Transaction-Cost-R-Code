# createRutkowskiContourNewNoGUI.R

# source('rutkowski-2.R')

# Main function is createRutContour()

createRutContour= function() {
  udBaseRut = computeBaseUDNew()
  u         = udBaseRut['u']
  d         = udBaseRut['d']
  priceBaseRut = computeBasePriceNew(u=u,d=d)
  dLRut        = computeGridPriceNew()
  udMatrix2    = getContourRutkowski(dL=dLRut,oPrice=priceBaseRut)
  # Choose subset of (u,d) pairs based on nUDPairsToUse
  nUDPairsToUse = myEnv$nUDPairsToUse
  nUDPairsRut   = nrow(udMatrix2)
  rowsToUseRut = unique(round(seq(1,nUDPairsRut,length=nUDPairsToUse)))
  nUDPairsToUseRut = length(rowsToUseRut)
  stopifnot(length(rowsToUseRut) > 0)
  udMatrixRut = udMatrix2[rowsToUseRut,,drop=FALSE]
  colnames(udMatrixRut) = c('u','d')
  returnList   = list(udBaseRut=udBaseRut,
                  priceBaseRut=priceBaseRut,
                  dLRutkowski=dLRut,
                  udMatrix2=udMatrix2,
                  rowsToUseRut=rowsToUseRut,
                  nUDPairsToUseRut=nUDPairsToUseRut,
                  udMatrixRut=udMatrixRut)
  packListToEnvironment(myList = returnList,myEnvironment = computedEnv)
  invisible(returnList)}

computeBaseUDNew = function() {
  unpackList(myEnv)
  h = TimeToExpiration/nFlips
  u = unname(exp(sigma*sqrt(h)))
  d = unname(1/u)
  return(c(d=d,u=u))
}

computeBasePriceNew = function(u,d){
    unpackList(myEnv)
    p      = prob(lambda,mu,r,u,d)
    phat   = p['phat']
    ptilda = p['ptilda']
    stockList = stock(S0,u,d,nFlips)
    fStored   = computeF1F2(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r)
    price     = optionPriceRut(phat,r,fStored)
    return(unname(price))
  }

computeGridPriceNew = function(){
  unpackList(myEnv)
  uValues = seq(uStart,uEnd,length=nPtsU)
  dValues = seq(dStart,dEnd,length=nPtsD)
  priceArray = matrix(NA,nrow=nPtsD,ncol=nPtsU)
  cat('\n\n IN computeGridPrice() FOR Rutkowski Contour Computation')
  for (i in 1:nPtsU) { # each column i fixes one u value
    u = uValues[i]
    cat('\n u value number',i,' out of',nPtsU)
    for (j in 1:nPtsD) { # each row j fixes one d value
      d = dValues[j]
      priceArray[j,i] = computeBasePriceNew(u=u,d=d)
    } # end j loop
  }   # end i loop
#  cat('\n-----------------------------------------\n')
  invisible(list(u=uValues,d=dValues,c=priceArray))
}

getContourRutkowski = function(dL,oPrice) {
 #  ans = getWinVal(scope="L")
 #  unpackList(ans,scope="L")
  # NO SCALING IN COMPUTATION OF c.
  cLines = contourLines(dL$u,dL$d,dL$c,levels=oPrice)
  if (length(cLines) == 0) {stop('No Rutkowski contour. Exiting program')
  } else {return(cbind(u=cLines[[1]]$x,d=cLines[[1]]$y))}
}

testCreateRutkowskiContourNew = function() {
  answer = createRutContour()
  unpackList(answer)
  cat('\n Rutkowski base price',priceBaseRut,'\n')
  cat('\n Rutkowski contour plotted from',nrow(udMatrix2),'(u,d) pairs\n')
  optionPriceRounded = round(priceBaseRut,digits=3)
  mainT1 = paste('Rutkowski contour for option price', optionPriceRounded)
  mainT2 = paste('Rutkowski surface plots')
  plotContour(udMatrix2,
              mainTitle=mainT1)
  cat("\n Rutkowski contour using",myEnv$nUDPairsToUse,"(u,d) pairs\n")
  plotContour(udMatrixRut,mainTitle="Subset of market contour")
  plotSurface(dLRutkowski,mainTitle=mainT2)
  plotSurface1(dLRutkowski,mainTitle=mainT2)
  cat('\n Rutkowski option price surface plotted')
  flush.console()
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

