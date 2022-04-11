source("evaluateRhsControlledODE.R")
getOdeSolution <- 
  function(
    rhs = evaluateRhsODE,
    timeline = seq(0, 365, 1),
    par, 
    init
  ) {
  y <- 
    ode(
        func = rhs,
        y = init,
        times = timeline,
        parms = par,
        method = "lsoda",
        hmax=0.01
    )
  columnames <-
    list('time', 'xS', 'xI', 'xV', 'xR', 'xC', 'xF',  'xJ')
  colnames(y) <- columnames
  return(y)
}
