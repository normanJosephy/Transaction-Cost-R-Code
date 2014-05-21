# stripPlotOfResiduals.R

fileName = 'run702rut.Rdata'
dirName = 'C:/research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/'
fullFileName = paste(dirName,fileName,sep='')
load(fullFileName)
# load("C:/research/Lucy-Oct-Nov2013RComputationsFor LatestVersionOfPaper/delta.RData")
require(lattice)
require(latticeExtra)

data = apply(deltaBigRut,c(2,3),sum)
dataT = t(data)

sampleIndex = c(2,4)
dataTSample = dataT[,sampleIndex]
dataTVSample = col(dataTSample)
dim(dataTVSample) = NULL
inputSample = factor(dataTVSample)
valuesSample = dataTSample
dim(valuesSample) = NULL

dataTV = col(dataT)
dim(dataTV) = NULL
input = factor(dataTV)
values = dataT
dim(values) = NULL

myScales = list(y=list(labels=NULL,cex=0,tck=0,draw=FALSE))
myMain = expression(paste(Delta,' for each (u,d) pair on each path'))
myMainSingle = expression(paste(Delta,' for each (u,d) pair on actual path'))
stripplot(inputSample ~ valuesSample,xlab=expression(Delta),scales=myScales,main=expression(Delta))
# Single path plot with 
splot = stripplot(input ~ values,
          xlab=expression(Delta),
          ylab='Path',
          main=myMainSingle,
          scales=myScales)
print(splot + layer(panel.points(x=x[1],y=y[1],pch=19,col='black',cex=1.3)))
stripplot(values ~ input)

percentIncrease = function(vectorData) {
  rangeOfData = range(vectorData)
  percentImprovement = diff(rangeOfData)/rangeOfData[1]
  return(percentImprovement)
}
  
improvementPerPath = function(dataT) {
  perPathImprovement = apply(dataT,2,percentIncrease)
  improvementStats = summary(perPathImprovement)
  return(improvementStats)
}