#   This file contains a set of functions that build vectors and matrices used in the computations 
#    associated with the transaction cost algorithm

# source('SetConstants.R')
  

timeNames = function(nTimes) paste('time=',0:(nTimes-1),sep='')

stock = function(S0,u,d,nFlips) {
  stkList = list()
  for (i in 0:nFlips){
    uIndex = seq(i,0)
    dIndex = seq(0,i)
    stkList[[i+1]] = u^uIndex * d^dIndex * S0
  }
#  names(stkList) = timeNames(nFlips+1)
  return(stkList)
}

prob = function(lambda,mu,r,u,d) {
  l1 = 1 + lambda
  m1 = 1 - mu
  r1 = 1 + r
  top = unname(l1*r1 - m1*d)
  bot = unname(l1*u - m1*d)
  phat = top/bot
  top1 = unname(m1*r1 - m1*d)
  ptilda = top1/bot
  return(c(phat=phat,ptilda=ptilda))
}

# callPayoffGH is by delivery: g=ST,h=-K if in the money
callPayoffGH = function(ST,K) {
  g = ifelse(ST > K,ST,0)
  h = ifelse(ST > K,-K,0)
  return(cbind(g=g,h=h))
}

fAtExpiration = function(g,h,lambda,mu) {
  f1 = h + (1+lambda)*g
  f2 = h + (1 - mu)*g
  return(cbind(f1=f1,f2=f2))
}

updateF = function(phat,ptilda,r,jTime,f1,f2){
  numberOfStates = jTime+1
  phatC   = 1 - phat
  ptildaC = 1 - ptilda
  f1Update = rep(NA,numberOfStates)
  f2Update = rep(NA,numberOfStates)
  rFactor = 1/(1+r)
  for (iState in 1:numberOfStates) {
    f1Update[iState] = rFactor*(phat   * f1[iState] + phatC   * f2[iState+1])
    f2Update[iState] = rFactor*(ptilda * f1[iState] + ptildaC * f2[iState+1])   
  }
  return(cbind(f1=f1Update,f2=f2Update))
}

HG = function(mu,lambda,fStored) {
  hgStored = list()
  nTimePeriods = length(fStored)
  coeff = 1/(mu + lambda)
  for (time in 1:nTimePeriods) {
    #f1, f2 stored in reverse time order: expiration is first element
    f1 = unname(fStored[[time]][,'f1'])
    f2 = unname(fStored[[time]][,'f2'])
    H = coeff*((1+lambda)*f2 - (1 - mu)*f1)
    G = coeff*(f1 - f2)
    hgStored[[time]] = cbind(H=H,G=G)
  }
  names(hgStored) = rev(timeNames(nTimePeriods))
  return(hgStored)
}

optionPriceRut = function(phat,r,fStored) {
  nTime   = length(fStored)
  # fStored is backwards; first entry is at expiration
  fValues = rev(fStored)[[2]]
  fUp     = fValues[1,'f1']
  fDown   = fValues[2,'f2']
  optionValue = 1/(r+1)*(phat*fUp + (1 - phat)*fDown)
  return(optionValue)
}

computeF1F2 = function(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r) {
  # create storage for f1,f2
  fStored = list()
  #
  # calculate at expiration the g and h vectors
  #
  ST = stockList[[length(stockList)]]
  gh = callPayoffGH(ST,K)
  g  = gh[,'g']
  h  = gh[,'h']
  #
  # compute f1,f2 at expiration
  #
  f12 = fAtExpiration(g,h,lambda,mu)
  f1  = f12[,'f1']
  f2  = f12[,'f2']
  ifStored = 1
  fStored[[ifStored]] = f12
  #
  # loop over time to compute f1,f2 at all non-terminal nodes
  lastJTime = nFlips # jTime goes from 0 to nFlips
  #
  # fold f1,f2 tree backwards from jTime = lastJTime-1 to jTime=0
  for (jTime in seq(lastJTime-1,0)) {
    fUpdated = updateF(phat,ptilda,r,jTime,f1,f2)
    #    print(fUpdated)
    ifStored            = ifStored + 1
    fStored[[ifStored]] = fUpdated
    f1                  = fUpdated[,'f1']
    f2                  = fUpdated[,'f2']
  }
  names(fStored) = rev(timeNames(nFlips+1))
  return(fStored)
}

rutkowskiDelta = function(path,H,G,lambda=0.2,mu=1/6,r=0) {
  nPathValues = length(path)
  psi         = path[-1]/path[-nPathValues]
  factorV     = vector(mode='numeric',length=length(G) - 1)
  for (i in 1:length(factorV)){
    if (G[i+1]/path[i+1] > G[i]/path[i]) {factorV[i]=1+lambda
    } else {factorV[i]=1-mu}
  }
  HD = H[-length(H)]*(1+r) - H[-1]
  GD = G[-length(G)]*psi - G[-1]
  delta = HD + factorV*GD
  return(delta)
}

# del is delta with NA appended at time 0.
createDeltaRutkowski = function(pathMatrix,lambda,mu,r,u,d,K) {
#   if (is.null(pathMatrix)) {
#     pathMatrix = cbind(c(100,120,96),
#                        c(100,110,107.8),
#                        c(100,140,105))
#   } 
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

# All costs will be positive.
# createTCosts() is in residualCRR.R
# GMatrix from createDeltaRutkowski()
tCostRutkowski = function(pathMatrix,GMatrix,lambda,mu){
  nTime = nrow(pathMatrix)
  nPath = ncol(pathMatrix)
  rutCosts = matrix(NA,nrow=nTime,ncol=nPath)
  colnames(rutCosts) = paste('path-',1:nPath,sep='')
  for (kpath in 1:nPath) {
    path = pathMatrix[,kpath]
    G    = GMatrix[,kpath]
    shareQt = G/path
    rutCosts[,kpath] = createTCosts(path,shareQt,lambda,mu)
  }   # end ipath loop
  invisible(rutCosts)
}

testTCostAndDeltaRutkowski = function() {
  pathMatrix = cbind(c(100,120,96),
                     c(100,110,107.8),
                     c(100,140,105))
  # See default values of lambda,mu,r,u,d,K in argument list
  # of createDeltaRutkowski() function.
  ans = createDeltaRutkowski(pathMatrix=pathMatrix)
  GMatrix = ans$GMatrix  
  HMatrix = ans$HMatrix
  delta   = ans$delta
  del     = rbind(rep(NA,ncol(delta)),delta)
  lambda = 0.2
  mu     = 1/6
  rutCosts = tCostRutkowski(pathMatrix,GMatrix,lambda,mu)
  rutCosts = round(rutCosts,digits=3)
  nPaths = ncol(pathMatrix)
  for (kpath in 1:nPaths) {
    rutCost = rutCosts[,kpath]
    G = GMatrix[,kpath]
    H = HMatrix[,kpath]
    D = del[,kpath]
    path = pathMatrix[,kpath]
    output = cbind(path,G,H,'delta'=D,'rutCost'=rutCost)
    output = round(output,digits=3)
    rownames(output) = paste('time-',0:(nTimes-1),sep='')
    print(output,digits=3)
    cat('\n')  
  }
  invisible(rutCosts)
}

collectGHRutkowski = function(path,lambda=0.2,mu=1/6,r=0,u=1.3,d=0.9,K=100) {
  nFlips           = length(path) - 1
  hgInitial        = matrix(0,nrow=nFlips+1,ncol=2)
  colnames(hgInitial) = c('H','G')
  rownames(hgInitial) = paste('time',0:nFlips,sep='-')
  optionPriceValue = rep(NA,nFlips+1)
  outputList       = list()
  nFlipsList       = seq(nFlips,1)
  p                = prob(lambda,mu,r,u,d)
  phat   = p['phat']
  ptilda = p['ptilda']
  #
  for (i in 1:nFlips) {
    s  = path[i]
    nF = nFlipsList[i] 
    stockList = stock(s,u,d,nF)
    fStored   = computeF1F2(stockList,s,u,d,nF,K,lambda,mu,phat,ptilda,r)
    price     = unname(optionPriceRut(phat,r,fStored))
    hg        = HG(mu,lambda,fStored)
    hgInitial[i,]        = rev(hg)[[1]]
    optionPriceValue[i]  = price
    output    = list(s=s,nF=nF,hg=hg,price=price,
                     fStored=fStored,p=p,stockList=stockList)
    outputList[[i]]      = output
  }
  # Calculate and save expiration time G,H, which are the
  # option payoffs at expiration.
  # Compute option payoff at expiration as max(s-K,0).
  #
  # Return value of callPayoffGH is a one row matrix.
  # callPayoffGH was modified to accept a vector of stock prices,
  # and it returns a matrix of g,h values.
  # Access its values by GHTerminal[,'g'] and GHTerminal[,'h'].
  pathTerminalValue = path[length(path)]
  GHTerminal        = callPayoffGH(pathTerminalValue,K)
  gValue = GHTerminal[,'g']
  hValue = GHTerminal[,'h']
  gValue = unname(gValue)
  hValue = unname(hValue)
  hgInitial[nFlips+1,]       = c(hValue,gValue)
  optionPriceValue[nFlips+1] = max(pathTerminalValue - K,0)  
 invisible(list(hgInitial=hgInitial,
                 optionPriceValue=optionPriceValue,
                 outputList=outputList))
}

testCollectGHRutkowski = function(path=c(100,140,105)) {  
  lambda = 0.2
  mu     = 1/6
  r = 0.0
  u = 1.3
  d = 0.9
  K=100
  doubleList = collectGHRutkowski(path=path,lambda=lambda,mu=mu,r=r,u=u,d=d,K=K)
  hgInitial        = doubleList$hgInitial
  optionPriceValue = doubleList$optionPriceValue
  outputList       = doubleList$outputList
#
  constants = c(lambda,mu,r,u,d,K)
  names(constants)= c('lambda','mu','r','u','d','K')
  cat('\n\tconstants\n')
  print(constants,digits=3)
  cat("\n")
  cat(' path:',path,'\n')
  hgAndPrice = cbind(hgInitial,option=optionPriceValue)
  print(hgAndPrice)
#  cat('\n option price')
#  print(optionPriceValue)
#  cat("\n",rep('=',20),'\n')
  if (TRUE) {
#    cat("\n\t outputList")
    nFlips = length(path) - 1
    for (i in 1:length(outputList)){
      cat("\n",rep('-',20),"\n\t flip",i,'outputList')
      output      = outputList[[i]]
      outputNames = names(output)
      for (j in 1:length(output)) {
        cat('\n\t\t',outputNames[j],'\n')
        print(output[[j]])
      }
    }
    cat('\n\n')
  }
  invisible(hgInitial)
}

  
testTestCollectGHRutkowski = function() {
  path1 = c(100,120,96)
  path2 = c(100.0,110.0,107.8)
  path3 = c(100,140,105)
  pathMatrix = matrix(c(path1,path2,path3),nrow=3,ncol=3)
  for (i in 1:3) {
    testCollectGHRutkowski(path=pathMatrix[,i])
    cat('\n',rep('-',30),'\n')
  }
} 




p         = prob(lambda,mu,r,u,d)
phat      = p['phat']
ptilda    = p['ptilda']
stockList = stock(S0,u,d,nFlips)
fStored   = computeF1F2(stockList,S0,u,d,nFlips,K,lambda,mu,phat,ptilda,r)
#price     = optionPriceRut(phat,r,fStored) 
#hg        = HG(mu,lambda,fStored)  
#  costList  = tCostRutkowski(hg,stockList,r)
#output    = list(constants=constants,p=p,stockList=stockList,
#                 fStored=fStored,price=price,hg=hg)


 