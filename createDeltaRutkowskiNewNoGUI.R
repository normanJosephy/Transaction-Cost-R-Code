# createDeltaRutkowskiNewNoGUI.R 6/16/2014

createDeltaRutkowskiNew = function(u,d) {
#  unpackList(myEnv)
#  unpackList(computedEnv)
  pathMatrix = computedEnv$paths
  lambda = myEnv$lambda
  mu     = myEnv$mu
  r      = myEnv$r
  K      = myEnv$K
  #
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
  ud    = computeBaseUDNew()
  u     = ud['u']
  d     = ud['d']
  outputList=createDeltaRutkowskiNew(u,d)
  unpackList(outputList)
  #layout(c(1,3))
  par(mar=c(4,3,2,1),mfrow=c(3,1))
  matplot(1:nrow(GMatrix),GMatrix,type='l',ylab="", xlab="")
  mtext('Stock G',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  matplot(1:nrow(HMatrix),HMatrix,type='l',ylab="", xlab="")
  mtext('Bond H',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  matplot(1:nrow(delta),delta,type='l',ylab="",xlab="")
  abline(h=0)
  mtext('Delta',side=2,line=2,outer=FALSE)
  mtext('Time',side=1,line=2,outer=FALSE)
  layout(1)
  #  print(outputList)
invisible(outputList)
}
