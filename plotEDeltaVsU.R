# plotEDeltaVsU.R

getDeltaBigRut = function(fileName = 'run603rut.Rdata') {
  dirName = 'c:/research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
  setwd(dirName) 
  if (file.exists(fileName)){load(file=fileName,.GlobalEnv)} else {print('File run603rut.Rdata does not exist')}
}

createPlotData = function(fileName='run603rut.Rdata'){
  if (! exists('deltaBigRut')) {getDeltaBigRut(fileName=fileName)}
  dims = dim(deltaBigRut)
  ndelta = dims[1]
  nPaths = dims[2]
  nUD    = dims[3]
  plotData = rep(0,nUD)
  for (i in 1:nUD) {
    pathData = deltaBigRut[,,i]
    sumDelta = apply(pathData,2,sum)
    DeltaE   = mean(sumDelta)
    plotData[i] = DeltaE
  }
  plot(plotData,type='l',xlab='strategy',ylab=expression(paste('Average ' * Delta)))
  print(summary(plotData))
  return(plotData)
}