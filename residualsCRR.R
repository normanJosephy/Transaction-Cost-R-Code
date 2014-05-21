# residualsCRR.R 

# source('crr-1.R')
# source('rutkowski-2.R')

# 6/19/2012 generalized to accept paths of any path length

# 6/20/2012 Replaced old residual CRR code with three functions.

# 6/20/2012 Moved a modified transaction cost for CRR function to this file.

# 7/1/2012 In createPortfolioCRR(): Changed Bk from 1 to (1+r)^seq(0,nFlips-1)

testCreates = function() {
  path1 = c(100,120,96)
  path2 = c(100,140,105)
  path3 = c(100,110,107.8)
  u=1.3
  d=0.9
  r=0.05
  K=100
  lambda      = 0.01
  mu          = 0.01
  pathMatrix  = cbind(path1,path2,path3)
  #  cat('\n\n')
  for (ipath in 1:3) {
    path         = pathMatrix[,ipath]
    pathLength   = length(path)
    pathDropLast = path[-pathLength]
    psi          = c(path[-1]/pathDropLast)
    gi           = createGi(path,K,u,d,r)
    portfolio    = createPortfolioCRR(gi,path,u,d,r,K)
    delta        = createDelta(gi,u,d,psi)
    tCost        = createTCosts(path,portfolio[,1],lambda,mu)
    
    # Prepare data for printing
    # Add a row of NA to top of gi
    # Add a NA to top of delta
    giOutput     = rbind(rep(NA,ncol(gi)),gi)
    deltaOutput  = c(NA,delta)
    outputPrint  = cbind(path,giOutput,portfolio,'CRRdelta'=deltaOutput,
                         'CRRcost'=tCost)
    outputPrint  = round(outputPrint,digits=4)
    rownames(outputPrint) = paste('time-',0:(nrow(portfolio)-1),sep='')
    cat('\n Path:',path,'\n')
    print(outputPrint,digits=4)
  }
  cat('\n')
}

# All costs will be positive.
createTCosts = function(sPath,portShareQt,lambda,mu,verbose=FALSE){
  # Prepend 0 to share quantity, to compute setup transaction cost 
  portShareQtWithZero = c(0,portShareQt)
  changeShareQt       = diff(portShareQtWithZero)
  factor = lambda * (changeShareQt > 0) - mu * (changeShareQt <= 0)
  tCost  = changeShareQt * sPath * factor
  # Prepare data for printing
  if (verbose) {
    output = cbind(sPath,portShareQt,factor,tCost)
    print(round(output,digits=4),digits=4)
                }
  invisible(tCost)
}

createDelta = function(gi,u,d,psi) {
  giU = gi[,'giu']
  giD = gi[,'gid']
  giT = gi[,'git']
  delta  = (u - psi)/(u-d) * giD + (psi - d)/(u-d) * giU - giT
  return(delta)
}

# gi is output of createGi()
# NOTE: This function does NOT compute portfolio at expiration.
# That portfolio is either (1,-K) (if call is in the money),
# or (0,0) (if call is not in the money).
createPortfolioCRR = function(gi,path,u,d,r,K) {
  giu      = gi[,'giu']
  gid      = gi[,'gid']
  SiMinus1 = path[-length(path)]
  uMd      = u-d
  gamma    = (giu - gid)/(SiMinus1*uMd)
  nFlips   = length(SiMinus1)
  rPlus1   = 1 + r
  Bk       = rPlus1^seq(0,nFlips-1)
  beta     = (u*gid - d*giu)/(rPlus1*Bk*uMd)
# Add a final value to portfolio stock quantity: 1 if in the money, 0 otherwise.
# Do same for bond: -K if in the money, 0 otherwise.
  inTheMoney = (path[length(path)] > K)
  if (inTheMoney) {
    gamma = c(gamma,1)
    beta  = c(beta,-K)
  } else {
    gamma = c(gamma,0)
    beta  = c(beta,0)
  }
  output   = cbind(gamma,beta)
  colnames(output) = c('stock #', 'bond $')
  rownames(output) = paste('time-',0:(nrow(output)-1),sep='')
  return(output)
}

createGi = function(path,K,u,d,r) {
  pathLength    = length(path)
  nFlips        = pathLength - 1
  psi           = c(path[-1]/path[-pathLength])
  gi            = matrix(NA,nrow=nFlips,ncol=3)
  colnames(gi)  = c('giu','gid','git')
  rownames(gi)  = paste('time-',1:nFlips,sep='')
  for (iTime in 1:nFlips) {
    SiMinus1    = path[iTime]
    kFlips      = nFlips - iTime
    psiValue    = psi[iTime]
    if (kFlips == 0) {
      # Terminal nodes have option payoff as option value
      giU =  max(SiMinus1*u - K,0)
      giD =  max(SiMinus1*d - K,0)
      giT =  max(SiMinus1*psiValue - K,0)
    } else {
      otreeU = optionTree(SiMinus1*u,K,u,d,r,kFlips)
      giU    = rev(otreeU)[[1]]
      otreeD = optionTree(SiMinus1*d,K,u,d,r,kFlips)
      giD    = rev(otreeD)[[1]]
      otreeT = optionTree(SiMinus1*psiValue,K,u,d,r,kFlips)
      giT    = rev(otreeT)[[1]]
    }  # end else statement
    gi[iTime,] = c(giU,giD,giT)
  }    # end iTime loop
  return(gi)
}


