updatePolicy <- function(x, par) {
  xS <- x[1]
  xI <- x[2]
  xV <- x[3]
  xR <- x[4]
  xC <- x[5]
  k1 <- as.numeric(par["k1"])
  k2 <- as.numeric(par["k2"])
  a <- as.numeric(par["a"])
  beta <- as.numeric(par["beta"])
  mu <- as.numeric(par["mu"])
  phi <- as.numeric(par["phi"])
  omega <- as.numeric(par["omega"])
  theta <- as.numeric(par["theta"])
  sigma <- as.numeric(par["sigma"])
  gamma <- as.numeric(par["gamma"])
  # Set capacity with the Mexico###
  capacity <- c(
    "green" = 200,
    "amber" = 50,
    "red" = 10
  )
  #traffic signal policy
  ligth = ''
  if (xC <= 0.5) {
    ligth <- 'green'
  } else if (xC > 0.5 && xC <= 0.7) {
    ligth <- 'amber'
  } else if (xC > 0.7) {
    ligth <- 'red'
  }
  k <- as.numeric(capacity[ligth])
  par$k <- k
  return(par)
}