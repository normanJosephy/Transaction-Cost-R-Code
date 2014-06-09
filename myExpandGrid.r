# myExpandGrid.R

my.expand.grid = function (...) 
{
  nargs <- length(args <- list(...))
  cargs <- vector("list", nargs)
  iArgs <- seq_len(nargs)
  nmc <- paste0("Var", iArgs)
  nm <- names(args)
  if (is.null(nm)) 
    nm <- nmc
  else if (any(ng0 <- nzchar(nm))) 
    nmc[ng0] <- nm[ng0]
  names(cargs) <- nmc
  rep.fac <- 1L
  d <- sapply(args, length)
  orep <- prod(d)
  if (orep == 0L) {
    for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
  }
  else {
    for (i in iArgs) {
      x <- args[[i]]
      nx <- length(x)
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,nx)), orep)]
      cargs[[i]] <- x
      rep.fac <- rep.fac * nx
    }
  }
  rn <- .set_row_names(as.integer(prod(d)))
  structure(cargs, class = "data.frame", row.names = rn)
}

testExpandGrid = function(printIt=FALSE) {
  uValues = 1:1000
  dValues = 101:200
  dByu = expand.grid(dValues,uValues,KEEP.OUT.ATTRS = FALSE)
  nRows = length(uValues) * length(dValues)
  f = `+`
  answers = vector(mode = 'numeric',length = nRows)
  for (i in 1:nRows) {
    answers[i] = f(dByu[i,2],dByu[i,1])
  }
  dim(answers) = c(length(dValues),length(uValues))
  if (printIt) print(answers)
}
#---------------------------------------------------
computeGridPrice1 = function(uStart,uEnd,dStart,dEnd,nPtsU,nPtsD,
                            lambda,mu,r,S0,K,nFlips){
  uValues = seq(uStart,uEnd,length=nPtsU)
  dValues = seq(dStart,dEnd,length=nPtsD)
#  priceArray = matrix(NA,nrow=nPtsD,ncol=nPtsU)
  cat('\n\n Using expand.grid IN computeGridPrice1 FOR Rutkowski Contour Computation')
  #
  udCrossProduct = expand.grid(dValues,uValues,KEEP.OUT.ATTRS = FALSE)
  nUDPairs = nPtsU*nPtsD
  priceVector = vector(mode='numeric',length=nUDPairs)
  for(i in 1:nUDPairs){
    u = udCrossProduct[i,2]
    d = udCrossProduct[i,1]
    priceVector[i] = computeBasePrice(u=u,d=d,lambda=lambda,mu=mu,
                                      r=r,S0=S0,K=K,nFlips=nFlips)
    dim(priceVector) = c(nPtsD,nPtsU)
  }
  cat('\n-----------------------------------------\n')
  invisible(list(u=uValues,d=dValues,c=priceVector))
}

computeGridPrice2 = function(uStart,uEnd,dStart,dEnd,nPtsU,nPtsD,
                            lambda,mu,r,S0,K,nFlips){
  uValues = seq(uStart,uEnd,length=nPtsU)
  dValues = seq(dStart,dEnd,length=nPtsD)
  priceArray = matrix(NA,nrow=nPtsD,ncol=nPtsU)
  cat('\n\n Using for loops IN computeGridPrice() FOR Rutkowski Contour Computation') 
    for (i in 1:nPtsU) { # each column i fixes one u value
      u = uValues[i]
      for (j in 1:nPtsD) { # each row j fixes one d value
        d = dValues[j]
        priceArray[j,i] = computeBasePrice(u=u,d=d,lambda=lambda,mu=mu,
                                           r=r,S0=S0,K=K,nFlips=nFlips)
      } # end j loop
    }   # end i loop
  cat('\n-----------------------------------------\n')
  invisible(list(u=uValues,d=dValues,c=priceArray))
}