# widgetDriver1.R

require(PBSmodelling)

runInEnv = function() {
  local(envir=.PBSmodEnv,expr={
    runWidget()
  })
}

callFocus = function() { cat("\n \n")}

# runWidget creates input widget
#
runWidget = function() {
  myMatrix = matrix(1:9,3,3)
  colnames(myMatrix) = c('col1','col2','col3')
  myLogicalVector = c(first=TRUE,second=TRUE,third=FALSE)
  myDataFrame = data.frame(first=1:3,
                           second=c(TRUE,TRUE,FALSE),
                           third=letters[1:3],
                           stringsAsFactors=FALSE)
# cat("\n Set working directory \n")
# flush.console()
# myWorkingDir = setwdGUI()
# cat("\n Set data directory \n")
# flush.console()
#  myDataDir    = selectDir()
  # chosenGUI = getChoice(choice=c("widgetDriver1.txt","allinclude.txt"))
  # createWin(chosenGUI)
  createWin("widgetDriver1.txt")
#  print(ls(envir=.PBSmodEnv))
  readline("Press any key with cursor in console to continue")
  ans = getWinVal(scope="L")
  cat("\n\n Retrieved values from getWinVal(scope='L')\n")
  print(ans)
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