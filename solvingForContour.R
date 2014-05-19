# solvingForContour.R  5/12/2014
# Put on git
require('testthat')

# source(myMultiStart.R)  # for myMultiStart()

# library(parallel)
# library(doParallel)
# library(foreach)
# 
# registerDoParallel(cores=2)
# 
# n=1000
# ans1 = list()
# 
# system.time(for (i in 1:n) ans1[[i]] = dbinom(x=0:i,size=i,prob=.5))
# system.time(foreach(i = 1:n,.combine=list) %do% dbinom(x=0:i,size=i,prob=.5))
# system.time(foreach(i = 1:n,.combine=list) %dopar% dbinom(x=0:i,size=i,prob=.5))

# solvingForContour.R  5/12/2014

# THIS IS THE FUNCTION g.
# The contour is solving g - x0 = 0, i.e. g = x0,
# where x0 is initial option price.


g = function(x,
             r=0.01,
             s0=100,
             K=100,
             n=100
             ) {
  u = x[1]
  d = x[2]
  j = 0:n
  jr = n:0
  sj = s0*u^j*d^jr
  f = pmax(sj-K,0)
  prob = ((1+r) - d)/(u-d)
  pr = dbinom(x=j,size=n,prob=prob)
  disc = 1/(1+r)^n
  g0 = disc*sum(pr*f)
  return(g0)
}

myPairs = function() {
  ans = drawContour()
  u = ans$u
  d = ans$d
  n = length(u)
  #
  pairs = matrix(NA,nrow=n^2,ncol=2)
  k=0
  for (i in 1:n) {
    for (j in 1:n) {
      k=k+1
      pairs[k,] = c(u[i],d[j])
    }
  }
  return(pairs)
}

myMulti = function(x0,...) {
  p0 = myPairs()
  fn = function(x,...) g(x,...) - x0
  ans = myMultiStart(par=p0,fn=fn)
  points(ans[,1],ans[,2],cex=1.2,pch=19)
}

drawContour = function(n=10,
                 u = seq(1.02,1.2,length=n),
                 d = seq(0.5,0.9,length=n)) {
  testmatrix = matrix(NA,n,n)
  for (i in 1:n) {
    for (j in 1:n) {
      testmatrix[i,j] = g(c(u[i],d[j]))
    }
  }
  contour(x=u,y=d,z=testmatrix)
  cL = contourLines(x=u,y=d,z=testmatrix,levels=0)
  invisible(list(u=u,d=d,testmatrix=testmatrix,cL=cL))
}

testN = function(u0=1.1,d0=0.8,z0=75,fn=g) {
  ans = nleqslv(x=c(u0,d0),fn=fn)
}
