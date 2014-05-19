
# myMultiStart.R  5/12/2014
# Put on git

require(doParallel)
require(foreach)

myMultiStart = function (par, fn, control = list()) 
{
  pmat <- matrix(NA, nrow(par), ncol(par))
  for (k in 1:nrow(par)) {
      ans <- try(BBsolve(par[k, ], 
                         fn = fn , 
                         quiet = TRUE,
                         control = list()), silent = TRUE)
    if (inherits(ans, "try-error")) 
      next
    pmat[k, ] <- ans$par
  }
    pmat
}

myMultiStartParallel = function (par, fn, control = list(), ...) 
{
  foreach(k = 1:nrow(par),.combine=rbind) %do%
    BBsolve(par[k, ], 
            fn = fn , 
            quiet = TRUE,
            control = list())$par
  }
