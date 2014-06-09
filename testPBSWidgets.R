# testPBSWidgets.R

require(PBSmodelling)

# runWidget creates input widget
# When button is pressed, runs windowValues()
runWidget = function() {
  require(PBSmodelling)
  myMatrix = matrix(1:9,3,3)
  colnames(myMatrix) = c('col1','col2','col3')
  myLogicalVector = c(first=TRUE,second=TRUE,third=FALSE)
  myDataFrame = data.frame(first=1:3,
                           second=c(TRUE,TRUE,FALSE),
                           third=letters[1:3],
                           stringsAsFactors=FALSE)
  createWin("testPBSWidgets.txt")
}

windowValues = function() {
  ans = getWinVal(scope="L")
  cat("\n\n class of data frame myDataFrame columns\n")
  print(sapply(X = ans$myDataFrame,FUN = class))
  cat("\n\n class of data frame myInputDataFrame\n")
  print(sapply(X=ans$myInputDataFrame,FUN=class))
  cat("\n\n List created by getWinVal(scope='L')\n")
  print(ans)
  invisible(ans)
}

collectValues = function() {
  ans = windowValues()
  cat("\n In collectValues, retrieving window values in variable ans\n\n")
  print(ans)
}