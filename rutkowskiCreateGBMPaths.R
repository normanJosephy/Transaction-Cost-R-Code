# rutkowskiCreateGBMPaths.R  6/19/2012

# 6/21/2012 Changed mu to drift in pathSimulation.
# 6/22/2012 Added row and column names to pathMatrix.
# 6/23/2012 Changed default drift value to 0 in pathSimulation().

# 7/10/2012 Added modifiedPathSimulation().
#           It combines pathSimulation corrected for drift = \mu - 1/2 \sigma^2

pathSimulation = function(drift=0,sigma=.2,S0=100,
                          nFlips=2,TimeToExpiration=1.0) {
  dt = TimeToExpiration/nFlips
  noise = rnorm(nFlips)*sqrt(dt)*sigma  # Vector of scaled Brownian noise
  s = rep(0,nFlips+1)
  s[1] = S0
  shiftedNoise = 1 + drift*dt + noise
  for (i in 1:nFlips) {
    s[i+1] = s[i] * shiftedNoise[i]}
  invisible(s) }

createGBMPaths = function(nPaths=10,TimeToExpiration=1,nFlips=2,
                          sigma=0.2,drift=0.0,S0=100) {
  # Create storage for paths and assign row and column names
  rowN = paste('time-',0:nFlips,sep='')
  colN = paste('path-',1:nPaths,sep='')
  pathMatrix = matrix(NA,nrow=nFlips+1,ncol=nPaths,dimnames=list(rowN,colN))
  # Create paths
  for (iPath in 1:nPaths) {
    pathMatrix[,iPath] = pathSimulation(drift=drift,sigma=sigma,S0=S0,
                                nFlips=nFlips,
                                TimeToExpiration=TimeToExpiration)
  }  # end iPath loop
  invisible(pathMatrix)
}

testCreateGBMPaths = function(nFlips=2,nPaths=10) {
  pathMatrix = createGBMPaths(nFlips=nFlips,nPaths=nPaths)
  matplot(1:nrow(pathMatrix),pathMatrix,type='l',
          xlab='time',ylab='stock price',
          main=paste('GBM Paths For',nFlips,'Time Steps'))
}

modifiedPathSimulation = function(nPaths=100,TimeToExpiration=1,
                                  nFlips=6,sigma=.2,drift=0,S0=100) {
  nSamples = nFlips*nPaths
  noise = rnorm(nSamples)
  dt = TimeToExpiration/nFlips
  theta1 = drift - 1/2*sigma^2
  theta2 = sigma
#  jump1  = 1 + theta1*dt + theta2*sqrt(dt)*noise
  jump2  = exp(theta1*dt + theta2*sqrt(dt)*noise)
#  jumps = cbind(linear=jump1,exponential=jump2)
  cat('\n\n\t\t\t Stock Price Jump Distribution\n')
  print(summary(jump2))
#  cat("\n\n linear max and min:",max(jump1),min(jump1))
#  cat('\n expon. max and min jump size:',max(jump2),min(jump2),'\n\n')
  jumpMatrix = matrix(jump2,nrow=nFlips,ncol=nPaths)
  jumpMatrixWithS0 = rbind(rep(S0,nPaths),jumpMatrix)
  pathMatrix = apply(jumpMatrixWithS0,2,cumprod)
  rowN = paste('time-',0:nFlips,sep='')
  colN = paste('path-',1:nPaths,sep='')
  rownames(pathMatrix) = rowN
  colnames(pathMatrix) = colN
  invisible(pathMatrix)
}
