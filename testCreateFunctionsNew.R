# testCreateFunctionsNew.R

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

testCreatePathsAndJumpsFromIBMData = function() {
  createPathsAndJumpsFromIBMData()
  pc    = min(ncol(computedEnv$paths),40)
  p     = computedEnv$paths[,1:pc] # Use 40 paths in plot or all if less than 40 exist.
  pp    = plotPaths(paths=p,actualPath=computedEnv$actualPath)
  print(pp)
}