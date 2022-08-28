library(deSolve)
library(rjson)
library(reshape2)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(plotly)
library(classInt)
library("styler")
library(lubridate)
#
source("loadInitialConditions.R")
source("loadTransferParameters.R")
source("evaluateRhsControlledODE.R")
source("getOdeSolution.R")
source("updatePolicy.R")
source("get_time_stamp_sate_solution.R")
source("get_time_stamp_policy.R")
source("get_contrafactual_solution.R")
source("get_controlled_solution.R")
source("get_timeline_policy_transitions.R")
source("get_best_response_solution.R")
source("plot_scene.R")
#
par <-
  loadTransferParameters(file_name = "scene01.json")
initialConditions <-
  loadInitialConditions(file_name = "scene01.json")
timeLine <- seq(0, 156, 1)
parameter_grid <- list(1e-4, 5e-4, 1e-3, 5e-3)
res <- list()
header <- list()
for (a_I_ii in parameter_grid)
{
  par$a_I <- a_I_ii
  header <- 
    append(
      header,
      paste("a_I_", as.character(a_I_ii), sep='')
    )
  cat(
    paste("Computing data with a_I: ", as.character(par$a_I))
  )
  cat("\n-------------------------------------------------\n")
  sim_a_I_ii <- get_best_response_solution(
    par,
    initialConditions,
    timeLine = seq(0, 156, 1),
    data_path="./simulated_data",
    suffix=paste("_a_I_", as.character(a_I_ii), sep='')
  )
  res <- append(res, list(sim_a_I_ii))
}
names(res) <- header

