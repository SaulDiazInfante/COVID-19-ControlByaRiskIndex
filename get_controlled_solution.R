library(deSolve)
library(rjson)
library(reshape2)
library(dplyr)
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
#

get_controlled_solution <- 
  function(par_file_name = 'scene01.json', rhs = evaluateRhsODE, time_line
  ){
    par <-
      loadTransferParameters(file_name = 'scene01.json')
    initialConditions <- 
      loadInitialConditions(file_name = 'reference_parameters.json')
    
    grid <- classIntervals(timeLine, 156)
    intervalIndex <- 1
    #
    currentTimeWeek <-
      seq(from = grid$brks[intervalIndex], to = grid$brks[intervalIndex + 1])
    currentState <- initialConditions
    par <- updatePolicy(currentState, par)
    date_policy_idx <- as.integer(0)
    u_beta <- as.numeric(par["beta"])
    u_k <- as.numeric(par["k1"])
    u_semaphore <- par["semaphore"]
    policy_df <- data.frame(date_policy_idx, u_beta, u_k, u_semaphore)
    names(policy_df) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
    # First inteval solution
    currentSolution <- getOdeSolution(
      rhs,
      timeline = currentTimeWeek,
      par = par,
      init = initialConditions
    )
    for (idx in 2:(length(grid$brks) - 1)){
      # Select interval of integration
      currentTimeWeek <-
        seq(from = grid$brks[idx], to = grid$brks[idx + 1])
      # Update parameters (making a decision)
      currentState <- tail(currentSolution[2:7], n = 1)
      par <- updatePolicy(currentState, par)
      new_action <- data.frame(
        date_policy_idx = as.integer(date_policy_idx + idx - 1),
        u_beta = as.numeric(par["u_beta"]),
        u_k = as.numeric(par["u_k"]),
        u_semaphore = par$semaphore
      )
      policy_df <-bind_rows(policy_df, new_action)
      newSolution <- getOdeSolution(
        rhs,
        timeline = currentTimeWeek,
        par = par,
        init = as.numeric(currentState)
      )
      currentSolution <- bind_rows(currentSolution, newSolution)
    }
    controlledSolution <- currentSolution
    # timeline stamp tagging
    start_date <- ymd(20200101)
    controlled_state_time_line_idx <- controlledSolution["time"]
    controlled_state_time_line_date_in_days <- 
      get_time_stamp_state_solution(
        start_date=start_date, 
        controlled_state_time_line_idx
      )
    controlledSolution["date"] <- controlled_state_time_line_date_in_days
    #
    #
    policy_time_line_idx <- policy_df["date_policy_idx"]
    policy_time_line_date_in_weeks <- 
      get_time_stamp_policy(start_date=start_date, policy_time_line_idx)
    policy_df["dates"] <- policy_time_line_date_in_weeks
    controlledSolution_df <- data.frame(controlledSolution)
    write.csv(policy_df,"light_traffic_policy.csv", row.names = FALSE)
    write.csv(controlledSolution_df,
              "controlled_solution.csv", row.names = FALSE)
    res <- list()
    res$controlled_solution <- controlledSolution_df
    res$policy <- policy_df
    return(res)
  }
timeLine <- seq(0, 1092, 1)
policy_sol <- get_controlled_solution(time_line=timeLine)
