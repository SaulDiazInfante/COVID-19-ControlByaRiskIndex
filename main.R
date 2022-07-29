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
source("plot_scene.R")
#
# default parameters in modelParameters.json
#
#
par <-
  loadTransferParameters(file_name = 'scene01.json')
initialConditions <- 
  loadInitialConditions(file_name = 'scene01.json')
timeLine <- seq(0, 156, 1)
#
# Contrafactual and controlled solutions
refeSol <- 
  get_contrafactual_solution(
    par,
    initialConditions = initialConditions,
    time_line = timeLine
  )
controlledSol <- 
  get_controlled_solution(
    par,
    initialConditions,
    time_line = timeLine,
    decision_period_lenght = 2
    )
time_line_events <- get_timeline_policy_transitions()
# plotting
#
fig <- plot_scene()
fig
#
# TODO: Prepare the package directory tree and rebuild accordingly
# TODO: Implement the phase portrait between Risk C(t) and R_t
# Perform animations