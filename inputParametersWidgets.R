# inputParametersWidgets.R

require(PBSmodelling)

# runWidget creates input widget
#
runWidgetInputParameters = function() {
  createWin("inputParametersWidgets.txt")
#  print(ls(envir=.PBSmodEnv))
  readline("Press any key with cursor in console to continue")
  ans = getWinVal(scope="L")
  cat("\n\n Retrieved values from getWinVal(scope='L')\n")
  print(ans)
}
