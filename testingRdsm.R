# testingRdms.R

# Run once as cls = initializeCluster() from top level
initializeCluster = function(nCores=4) {
  require(Rdsm)
  require(parallel)
  cls = shmcls(nCores)
  ans1=mgrinit(cls)
  cls                  }

# stopCluster(cls)

fun = function(x) cbind(x,2*x,x^2)

info = function() {
  if (myinfo$id == 1) print(myinfo)
}

driver = function() {
  nr = 10
  nc = 3
  mgrmakevar(cls,"cost",nr,nc)
  clusterExport(cls,"fun")
  clusterExport(cls,"computation")
  clusterEvalQ(cls,computation(cost))
  print(cost[,])
  invisible(NULL)
}

computation = function(cost) {
  require(parallel)
  nr        = nrow(cost)
  nCores    = myinfo$nwrkrs
  indexList = splitIndices(nr,nCores)
  myIndices = indexList[[myinfo$id]]
  cost[myIndices,] = fun(myIndices)
  invisible(NULL)
}
