source("get_semaphore_actions.R")
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
  xF <- x[6]
  xJ <- x[7]
  k <- as.numeric(par["k"])
  a <- as.numeric(par["a"])
  beta <- as.numeric(par["beta"])
  mu <- as.numeric(par["mu"])
  phi <- as.numeric(par["phi"])
  omega <- as.numeric(par["omega"])
  theta <- as.numeric(par["theta"])
  sigma <- as.numeric(par["sigma"])
  gamma <- as.numeric(par["gamma"])
  a_I <- as.numeric(par["a_I"])
  a_beta <- as.numeric(par["a_beta"])
  a_k <- as.numeric(par["a_k"])
  light <- par[["semaphore"]]
  ### Update actions according to light state
  light_actions <- get_semaphore_actions(light)
  u_beta <- light_actions$u_beta
  u_k <- light_actions$u_k
  beta_u <- beta * (1.0 - u_beta)
  k_u <- k * (1.0 - u_k)
  ### Infection force and others ###
  nN <- xS + xI + xV + xR
  foi <- (1.0 + a * cos(2 * pi * t / 365.0)) * beta_u * (xI / nN) *(1- xC)
  dS <- mu * nN - (foi + phi + mu) * xS + omega * xV + theta * xR
  dI <- foi * xS + (1 - sigma) * foi * xV - (mu + gamma) * xI
  dV <- phi * xS - (mu + omega + (1 - sigma) * foi) * xV
  dR <- gamma * xI - (mu + theta) * xR
  dC <- (k_u / (nN - xI )) *
    ((1 - xC)) *
    (
      foi * xS +
        (1 - sigma) * foi * xV -
        (mu + gamma) * xI
    )
  dF <- foi * xS + (1 - sigma) * foi * xV
  dJ <- a_I * dF + a_beta * u_beta ^ 2 + a_k * u_k ^ 2
  rhs <- list(c(dS, dI, dV, dR, dC, dF, dJ))
  return(rhs)
}