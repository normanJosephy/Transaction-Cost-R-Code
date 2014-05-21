require(lattice)
# plot simulated paths using lattice
plotPaths = function(paths){
x = 1:nrow(paths)
# m = matrix(y,ncol=4)
# colnames(m) = c('first','second','third','fourth')
# myColors = c('red','blue','green','orange')
p = xyplot(c(paths) ~ rep(x,ncol(paths)),
           groups=c(col(paths)), 
           type='l',
           lwd=1,
           xlab='Time',
           ylab='Stock price',
           main='Simulated IBM Stock Price Paths')
invisible(p)
}

testPlot = function(paths) {
  p = paths[,1:40] # Use 40 paths in plot
  pp = plotPaths(p)
  print(pp)
}