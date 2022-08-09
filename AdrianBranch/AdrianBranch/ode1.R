ode1 <- function(t, x1, par1) {
  S <- x1[1]
  I <- x1[2]
  V <- x1[3]
  R <- x1[4]
  C <- x1[5]

  k <- par1[1]
  uk <- par1[2]
  a <- par1[3]
  beta <- par1[4]
  mu <- par1[5]
  phi <- par1[6]
  omega <- par1[7]
  theta <- par1[8]
  sigma <- par1[9]
  gamma <- par1[10]

  # ### Set afore ###
  # if (C <= 0.5){
  #   k <- k1
  # }else{
  #   k <- k2
  # }

  ### Infection force and others ###
  N1 <- S + I + V + R
  FOI <- (1 + a * cos(2 * pi * t / 365)) * beta * (I / N1) * (1 - C) # Si el riesgo es cercano a 0, la prob de infec debe ser mayor
  ku <- k * (1 - uk)

  dS <- mu * N1 - (FOI + phi + mu) * S + omega * V + theta * R
  dI <- FOI * S + (1 - sigma) * FOI * V - (mu + gamma) * I
  dV <- phi * S - (mu + omega + (1 - sigma) * FOI) * V
  dR <- gamma * I - (mu + theta) * R
  dC <- (ku / (1 - (I / N1))) * ((1 - C) / N1) * (FOI * S + (1 - sigma) * FOI * V - (mu + gamma) * I)
  dF <- FOI * S + (1 - sigma) * FOI * V

  return(list(c(dS, dI, dV, dR, dC, dF)))
}
