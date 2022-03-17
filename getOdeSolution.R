source("evaluateRhsODE.R")
getOdeSolution <- function(rhs = evaluateRhsODE,
                             timeline = seq(0, 365, 1),
                             par, init
                    ) {
  y <-
    as.matrix(
      ode(
        func = rhs,
        y = init,
        times = timeline,
        parms = par,
        method = "lsoda")
      )
  columnames <-
  list('time', 'xS', 'xI', 'xV', 'xR', 'xC', 'xF')
  df <- data.frame(y)
  names(df) <- columnames
  return(df)
}
