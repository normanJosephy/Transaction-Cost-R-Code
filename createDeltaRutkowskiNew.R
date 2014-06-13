# createDeltaRutkowskiNew.R

createDeltaRutkowskiNew = function(u,d,pathMatrix) {
  ans = getWinVal(scope="L")
  unpackList(ans,scope="L")
  nPaths = ncol(pathMatrix)
  nTimes = nrow(pathMatrix)
  rNames = paste('time-',0:(nTimes-1),sep='')
  cNames = paste('path-',1:nPaths,sep='')
  HMatrix = matrix(NA,nrow=nTimes,ncol=nPaths,dimnames=list(rNames,cNames))
  GMatrix = matrix(NA,nrow=nTimes,ncol=nPaths,dimnames=list(rNames,cNames))
  colnames(pathMatrix) = paste('path-',1:nPaths,sep='')
  rownames(pathMatrix) = paste('time-',0:(nTimes-1),sep='')
  delta = matrix(NA,nrow=nTimes-1,ncol=nPaths)
  rownames(delta) = paste('time-',1:(nTimes-1),sep='')
  colnames(delta) = cNames
  for (i in 1:nPaths) {
    path        = pathMatrix[,i]
    oList       = collectGHRutkowski(path,lambda,mu,r,u,d,K)
    G           = oList$hgInitial[,'G']
    H           = oList$hgInitial[,'H']
    GMatrix[,i] = G
    HMatrix[,i] = H
    delta[,i]   = rutkowskiDelta(path,H,G,lambda,mu,r)
  }
  # Prepare delta
  del = rbind(rep(NA,ncol(delta)),delta)
  rownames(del) = NULL # paste('path-',1:nPaths,sep='')
  colnames(del) = rep('deltaRut',ncol(delta)) # c('time-1','time-2')
  outputList = list(delta=delta,GMatrix=GMatrix,HMatrix=HMatrix,pathMatrix=pathMatrix)
  invisible(outputList)
}

# Use Rutkowski base u,d pair as u,d at which deltas are created
testCreateDeltaRutkowskiNew = function() {
  p = createPathsAndJumpsFromIBMData()
  paths = p$paths
  ud = computeBaseUDNew()
  u = ud['u']
  d = ud['d']
  outputList=createDeltaRutkowskiNew(u,d,paths)
  unpackList(outputList,scope="L")
  matplot(1:nrow(GMatrix),GMatrix,type='l',ylab="Portfolio Stock Amount G",
          xlab="Time")
#  print(outputList)
}
