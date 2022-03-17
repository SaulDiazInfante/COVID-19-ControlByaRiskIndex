
#' Evaluates the right hand side of the 
#' controlled ode y'=f(t, x, u) with control
#' time u = u(x(t))
#'
#' @param t 
#' @param x 
#' @param par 
#'
#' @return rhs
#' @export
#'
#' @examples
evaluateRhsODE <- function(t, x, par) {
  xS <- x[1]
  xI <- x[2]
  xV <- x[3]
  xR <- x[4]
  xC <- x[5]
  
  k <- as.numeric(par["u_k"])
  a <- as.numeric(par["a"])
  beta <- as.numeric(par["u_beta"])
  mu <- as.numeric(par["mu"])
  phi <- as.numeric(par["phi"])
  omega <- as.numeric(par["omega"])
  theta <- as.numeric(par["theta"])
  sigma <- as.numeric(par["sigma"])
  gamma <- as.numeric(par["gamma"])
  
  ### Infection force and others ###
  nN <- xS + xI + xV + xR
  foi <- (1.0 + a * cos(2 * pi * t / 365.0)) * beta * (xI / nN) * xC
  dS <- mu * nN - (foi + phi + mu) * xS + omega * xV + theta * xR
  dI <- foi * xS + (1 - sigma) * foi * xV - (mu + gamma) * xI
  dV <- phi * xS - (mu + omega + (1 - sigma) * foi) * xV
  dR <- gamma * xI - (mu + theta) * xR
  dC <- (k / (1 - (xI / nN))) *
    ((1 - xC) / nN) *
    (
      foi * xS +
        (1 - sigma) * foi * xV -
        (mu + gamma) * xI
    )
  dF <- foi * xS + (1 - sigma) * foi * xV
  rhs <- list(c(dS, dI, dV, dR, dC, dF))
  return(rhs)
}