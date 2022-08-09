library(vistime)
library(dplyr)
library(data.table)
library(vistime)
library(lubridate)
library(tidyverse)
library(tidyquant)

compute_Rt <- function(state_Xt, par, policy_df = NULL, controll_flag = FALSE) {
  beta <- as.numeric(par["beta"])
  # beta_u <- beta * (1 - u_beta)
  mu <- as.numeric(par["mu"])
  N <- as.numeric(par["N"])
  sigma <- as.numeric(par["sigma"])
  gamma <- as.numeric(par["gamma"])
  a <- as.numeric(par["a"])
  #
  # df_sol_file <- 'reference_solution.csv'
  # time_sereies_sol <- read_csv(df_sol_file)
  state_Xt <- data.frame(state_Xt)
  time_ <- state_Xt[, "time"]
  xS <- state_Xt[, "xS"]
  xV <- state_Xt[, "xV"]
  xC <- state_Xt[, "xC"]
  beta_t <- (1.0 + a * cos(2 * pi / 365.0 * time_)) * beta
  fact_svc <- (N * (mu + gamma))^(-1) * (1 - xC) * (xS + (1.0 - sigma) * xV)
  if (controll_flag) {
    action <- policy_df[nrow(policy_df), ]
    u_beta <- action["u_beta"]
    beta_u <- as.numeric(1 - u_beta) * beta
    beta_u_t <- (1.0 + a * cos(2 * pi * time_ / 365.0)) * beta_u
    controlled_r_t <- beta_u_t * fact_svc
    return(controlled_r_t)
  } else {
    r_t <- beta_t * fact_svc
    return(r_t)
  }
}

# refSol_Xt <- refSol %>%  select(time, xS, xV, xC)
# state_Xt <- refSol_Xt
# par <- refPar
# Rt <- compute_Rt(state_Xt, refPar)
