library(deSolve)
library(rjson)
library(reshape2)
library(dplyr)
library(tidyquant)
library(plotly)
library(classInt)
library("styler")
library(lubridate)
library(progress)
#
source("loadInitialConditions.R")
source("loadTransferParameters.R")
source("evaluateRhsControlledODE.R")
source("getOdeSolution.R")
source("updatePolicy.R")
source("get_semaphore_actions.R")
source("get_time_stamp_sate_solution.R")
source("get_time_stamp_policy.R")
#

get_controlled_solution <- 
  function(par_file_name = 'scene01.json', rhs = evaluateRhsODE, time_line
  ){
    par <-
      loadTransferParameters(file_name = par_file_name)
    initialConditions <- 
      loadInitialConditions(file_name = 'reference_parameters.json')
    grid <- classIntervals(timeLine, length(time_line) - 1)
    intervalIndex <- 1
    #
    decision_time_scale <- 7
    currentTimeWeek <-
      seq(from = grid$brks[intervalIndex] * decision_time_scale,
          to = grid$brks[intervalIndex + 1] * decision_time_scale)
    currentState <- initialConditions
    ### TODO:Implement best response control here
    # First inteval solution
    semaphore_states <- list("green", "yellow", "red")
    best_cost <- 10 ^ 10
    best_light <- "green"
    for (light in semaphore_states){
      par["semaphore"] <- light
      candidate_current_sol <- getOdeSolution(
        evaluateRhsODE,
        timeline = currentTimeWeek,
        par = par,
        init = initialConditions
      )
      candidate_cost <- tail(candidate_current_sol[,'xJ'], n=1)
      if (candidate_cost < best_cost){
        best_cost <- candidate_cost
        best_light <- light
        best_current_sol <- candidate_current_sol
        best_par <- par
      }
    }
    currentSolution <- best_current_sol
    u_semaphore <- best_par["semaphore"]
    light_actions <- get_semaphore_actions(u_semaphore[[1]])
    date_policy_idx <- as.integer(1)
    u_beta <- light_actions$u_beta
    u_k <- light_actions$u_k
    policy_df <- data.frame(date_policy_idx, u_beta, u_k, u_semaphore[[1]])
    names(policy_df) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
    
    pb <- progress_bar$new(
      format = "computing [:bar] :percent eta: :eta",
      total = 100, clear = FALSE, width= 60)
    
    for (idx in 2:(length(grid$brks) - 1)){
      # Select interval of integration
      pb$tick()
      currentTimeWeek <-
        seq(from = grid$brks[idx] * decision_time_scale,
            to = grid$brks[idx + 1] * decision_time_scale)
      # Update parameters (making a decision)
      currentState <- tail(best_current_sol[, 2:8], n = 1)
      initialConditions <- currentState
      initialConditions[1, 'xJ'] <- 0.0
      semaphore_states <- list("green", "yellow", "red")
      best_cost <- 10^10
      best_light <- "green"
      for (light in semaphore_states){
        par["semaphore"] <- light
        candidate_current_sol <- getOdeSolution(
          evaluateRhsODE,
          timeline = currentTimeWeek,
          par = par,
          init = initialConditions
        )
        candidate_cost <- tail(candidate_current_sol[,'xJ'], n=1)
        if (candidate_cost < best_cost){
          best_cost <- candidate_cost
          best_light <- light
          best_current_sol <- candidate_current_sol
          best_par <- par
        }
      }
      light_actions <- get_semaphore_actions(best_light)
      u_beta <- light_actions$u_beta
      u_k <- light_actions$u_k
      date_policy_idx = as.integer(idx )
      new_action <- data.frame(date_policy_idx, u_beta, u_k, best_light)
      names(new_action) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
      policy_df <-bind_rows(policy_df, new_action)
      newSolution <- best_current_sol
      currentSolution <- rbind(head(currentSolution, -1), newSolution)
    }
    controlledSolution_df <- data.frame(currentSolution)
    # timeline stamp tagging
    start_date <- ymd(20200101)
    controlled_state_time_line_idx <- controlledSolution_df["time"]
    controlled_state_time_line_date_in_days <- 
      get_time_stamp_state_solution(
        start_date=start_date, 
        controlled_state_time_line_idx
      )
    controlledSolution_df["date"] <- controlled_state_time_line_date_in_days
    #
    #
    policy_time_line_idx <- policy_df["date_policy_idx"]
    policy_time_line_date_in_weeks <- 
      get_time_stamp_policy(start_date=start_date, policy_time_line_idx)
    policy_df["dates"] <- policy_time_line_date_in_weeks
    write.csv(policy_df,"light_traffic_policy.csv", row.names = FALSE)
    write.csv(controlledSolution_df,
              "controlled_solution.csv", row.names = FALSE)
    res <- list()
    res$controlled_solution <- controlledSolution_df
    res$policy <- policy_df
    return(res)
  }
# timeLine <- seq(0, 52, 1)
# policy_sol <- get_controlled_solution(time_line=timeLine)
# policy_sol$controlled_solution
# policy_sol$policy
