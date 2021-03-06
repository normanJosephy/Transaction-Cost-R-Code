# inputParametersWidgets.R

require(PBSmodelling)

source("createPathsFromIBMPricesUpdated.R")
source("createDeltaRutkowskiNew.R")
source("rutkowski-2.R")
source("createRutkowskiContourNew.R")

# runWidget creates input widget
#
runWidgetInputParameters = function() {
#  setwdGUI()
  createWin("inputParametersWidgets.txt")
#  print(ls(envir=.PBSmodEnv))
  readline("Press any key with cursor in console to continue")
  ans = getWinVal(scope="L")
  cat("\n\n Retrieved values from getWinVal(scope='L')\n")
  print(ans)
}

id = function() invisible(NULL)
