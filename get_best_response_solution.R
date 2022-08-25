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


get_best_response_solution <- 
  function(
    parameter_file = "scene01.json",
    data_path="./simulated_data",
    sufix='controlled_path'
  ){
    #
    # default parameters in modelParameters.json
    #
    par <-
      loadTransferParameters(file_name = parameter_file)
    initialConditions <-
      loadInitialConditions(file_name = parameter_file)
    # 156 week accordingly to 3 years
    timeLine <- seq(0, 156, 1)
    # Contrafactual and controlled solutions
    refeSol <-
      get_contrafactual_solution(
        par,
        initialConditions = initialConditions,
        time_line = timeLine,
        outputh_path = data_path
      )
    controlledSol <-
      get_controlled_solution(
        par,
        initialConditions,
        time_line = timeLine,
        decision_period_lenght = 2,
        outputh_path = data_path
      )
    time_line_events <- 
      get_timeline_policy_transitions(
        policy_file_name = controlledSol$policy_path,
        solution_file = controlledSol$trajectory_path
      )
  }
