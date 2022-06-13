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
  control <-as.logical( par[["control"]])
  u_beta <- as.numeric(par["u_beta"])
  u_k <- as.numeric(par["u_k"])
  a_I <- as.numeric(par["a_I"])
  a_beta <- as.numeric(par["a_beta"])
  a_k <- as.numeric(par["a_k"])
  a_c <- as.numeric(par[["a_c"]])
  ligth <- par[["semaphore"]]
  ### Update actions according to light state
  
  if (control){
    light_actions <- 
      get_semaphore_actions(
        par, 
        ligth)  
    u_beta <- light_actions$u_beta
    u_k <- light_actions$u_k
  }
  
  beta_u <- beta * (1.0 - u_beta)
  k_u <- k * (1.0 - u_k)
  #
  ### Infection force and others ###
  nN <- xS + xI + xV + xR
  foi <- (1 + a * cos(2 * pi * t / 365)) * beta_u * (1 / nN) * (1 - xC)
  #
  dS <- mu * nN + omega * xV + theta * xR - (foi * xI + phi + mu) * xS 
  dI <- (xS + (1 - sigma) * xV) * foi * xI - (mu + gamma) * xI
  dV <- phi * xS - (mu + omega + (1 - sigma) * foi * xI) * xV
  dR <- gamma * xI - (mu + theta) * xR
  dC <- 
    k_u * (1 - xC) ^ (1 - 1 / k_u) * (
      1 - ((1 - xC) ^ (1 / k_u))
    ) * (foi * (xS + (1 - sigma) * xV) - (mu + gamma))
  dF <- (xS + (1 - sigma) * xV) * foi * xI
  
  # dJ <- a_I * dF + a_c * dC + a_beta * u_beta ^ 2 + a_k * u_k ^ 2
  dJ <- a_I * xI + a_c * xC + a_beta * u_beta ^ 2 + a_k * u_k ^ 2
  rhs <- list(c(dS, dI, dV, dR, dC, dF, dJ))
  return(rhs)
}

#TODO: implement flag to switch the control