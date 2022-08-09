get_cost <- function(t, df_sol, par) {
  xS <- df_sol["xS"]
  xI <- df_sol["xI"]
  xV <- df_sol["xV"]
  xR <- df_sol["xR"]
  xC <- df_sol["xC"]

  k <- as.numeric(par["u_k"])
  a <- as.numeric(par["a"])
  beta <- as.numeric(par["u_beta"])
  mu <- as.numeric(par["mu"])
  phi <- as.numeric(par["phi"])
  omega <- as.numeric(par["omega"])
  theta <- as.numeric(par["theta"])
  sigma <- as.numeric(par["sigma"])
  gamma <- as.numeric(par["gamma"])
  actions <- c(0.05, 0.25, 0.5, 0.75, 0.95)

  # TODO: Compute cost functional according to the fitness

  # Here we have to choose the best response
  for (i in 1:length(actions)) {
    Func <- ai * Y1[, 3] + au * uk[i]^2
    Val_F <- simp38(Func)

    if (Val_F < temp1) {
      temp1 <- Val_F
      Y2 <- Y1[, 2:7]

      # YS[,(j + 1)] <- Y1[,2]
      # YI[,(j + 1)] <- Y1[,3]
      # YV[,(j + 1)] <- Y1[,4]
      # YR[,(j + 1)] <- Y1[,5]
      # YC[,(j + 1)] <- Y1[,6]
      # YF[,(j + 1)] <- Y1[,7]

      VF <- uk[i]
    }
  }
}
