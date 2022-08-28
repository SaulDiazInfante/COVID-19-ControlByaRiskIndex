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
source("compute_Rt.R")

get_controlled_solution <-
  function(par_,
           init,
           rhs = evaluateRhsODE,
           time_line = seq(0, 156, 1),
           decision_period_lenght = 1,
           outputh_path='./simulated_data',
           suffix="") {
    n_classes <- (length(time_line) - 1) / decision_period_lenght
    grid <- classIntervals(time_line, n_classes)
    intervalIndex <- 1
    #
    decision_time_scale <- 7
    currentTimePeriod <-
      seq(
        from = grid$brks[intervalIndex] * decision_time_scale,
        to = grid$brks[intervalIndex + 1] * decision_time_scale
      )
    currentState <- initialConditions
    # First interval solution
    semaphore_states <- list("green", "yellow", "orange", "red")
    best_cost <- 10^10
    best_light <- "green"
    par <- par_
    par$control <- TRUE
    for (light in semaphore_states) {
      par["semaphore"] <- light
      candidate_current_sol <- getOdeSolution(
        evaluateRhsODE,
        timeline = currentTimePeriod,
        par = par,
        init = initialConditions
      )
      candidate_cost <- tail(candidate_current_sol[, "xJ"], n = 1)
      if (candidate_cost < best_cost) {
        best_cost <- candidate_cost
        best_light <- light
        best_current_sol <- candidate_current_sol
        best_par <- par
      }
    }
    currentSolution <- data.frame(best_current_sol)
    u_semaphore <- best_par["semaphore"]
    light_actions <- get_semaphore_actions(par, u_semaphore[[1]])
    date_policy_idx <- as.integer(1)
    u_beta <- light_actions$u_beta
    u_k <- light_actions$u_k
    policy_df <- data.frame(date_policy_idx, u_beta, u_k, u_semaphore[[1]])
    names(policy_df) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
    R_t <-
      compute_Rt(
        currentSolution,
        par,
        policy_df,
        TRUE
      )
    currentSolution["R_t"] <- R_t
    #
    pb <- progress_bar$new(
      format = "computing controlled sol. [:bar] :percent",
      total = length(grid$brks) - 1, clear = FALSE, width = 60
    )
    for (idx in seq(from = 2, to = (length(grid$brks) - 1))) {
      # Select interval of integration
      pb$tick()
      currentTimePeriod <-
        seq(
          from = grid$brks[idx] * decision_time_scale,
          to = grid$brks[idx + 1] * decision_time_scale
        )
      # Update parameters (making a decision)
      currentState <- tail(best_current_sol[, 2:8], n = 1)
      initialConditions <- currentState
      # initialConditions[1, 'xJ'] <- 0.0
      semaphore_states <- list("green", "yellow", "orange", "red")
      best_cost <- 10^10
      best_light <- "green"
      for (light in semaphore_states) {
        par["semaphore"] <- light
        candidate_current_sol <- getOdeSolution(
          evaluateRhsODE,
          timeline = currentTimePeriod,
          par = par,
          init = initialConditions
        )
        candidate_cost <- tail(candidate_current_sol[, "xJ"], n = 1)
        if (candidate_cost < best_cost) {
          best_cost <- candidate_cost
          best_light <- light
          best_current_sol <- candidate_current_sol
          best_par <- par
        }
      }
      light_actions <- get_semaphore_actions(par, best_light)
      u_beta <- light_actions$u_beta
      u_k <- light_actions$u_k
      date_policy_idx <- as.integer(idx)
      new_action <- data.frame(date_policy_idx, u_beta, u_k, best_light)
      names(new_action) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
      policy_df <- bind_rows(policy_df, new_action)
      newSolution <- data.frame(best_current_sol)
      R_t <-
        compute_Rt(
          newSolution,
          par,
          policy_df,
          TRUE
        )
      newSolution["R_t"] <- R_t
      currentSolution <- rbind(head(currentSolution, -1), newSolution)
    }
    controlledSolution_df <- data.frame(currentSolution)
    # timeline stamp tagging
    start_date <- ymd(20200101)
    controlled_state_time_line_idx <- controlledSolution_df["time"]
    controlled_state_time_line_date_in_days <-
      get_time_stamp_state_solution(
        start_date = start_date,
        controlled_state_time_line_idx
      )
    controlledSolution_df["date"] <- controlled_state_time_line_date_in_days
    #
    policy_time_line_idx <- policy_df["date_policy_idx"]
    policy_time_line_date_in_periods <-
      get_time_stamp_policy(
        start_date = start_date,
        policy_time_line_idx,
        decision_period_lenght
      )
    policy_df["dates"] <- policy_time_line_date_in_periods
    
    policy_file_name_ = "/light_traffic_policy"
    controlled_path_file_name_ = "/controlled_solution"
    path_policy = paste(
      outputh_path,
      policy_file_name_,
      suffix,
      ".csv",
      sep=''
    )
    path_trajectory = paste(
      outputh_path,
      controlled_path_file_name_,
      suffix,
      ".csv",
      sep=''
    )
    write.csv(
      policy_df,
      path_policy,
      row.names = FALSE
    )
    
    write.csv(
      controlledSolution_df,
      path_trajectory,
      row.names = FALSE
    )
    res <- list()
    res$controlled_solution <- controlledSolution_df
    res$policy <- policy_df
    res$policy_path <- path_policy
    res$trajectory_path <- path_trajectory
    return(res)
  }