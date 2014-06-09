# testPBS.R

require(PBSmodelling)

usingInputData = function() {
  ans = getWinVal(scope="L")
  cat("\n\n List created by getWinVal(scope='L')\n")
  print(ans)
  moneyness = ans$S0 - ans$K
  cat("\n moneyness = S0 - K: ",moneyness)
}

# runSimulation creates input widget
# Input widget, when button is pressed, runs usingInputData()
runSimulation = function() {
  require(PBSmodelling)
  createWin("testPBS.txt")
#  ans = getWinVal(scope="L")
#  print(ans)
}