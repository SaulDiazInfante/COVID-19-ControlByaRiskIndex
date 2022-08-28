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
source("get_time_stamp_sate_solution.R")
source("compute_Rt.R")

get_contrafactual_solution <-
  function(par,
           initialConditions,
           rhs = evaluateRhsODE,
           time_line = seq(0, 156, 1),
           outputh_path='./simulated_data',
           suffix=""
  ) {
    time_line_per_day <- seq(0, 7 * (length(time_line) - 1))
    ref_par <- par
    ref_par$control <- FALSE
    refSol <-
      getOdeSolution(
        evaluateRhsODE,
        timeline = time_line_per_day,
        par = ref_par,
        init = initialConditions
      )
    refSol <- data.frame(refSol)
    # reference solution time stamp tagging
    start_date <- ymd(20200101)
    refSolution_state_time_line_idx <- refSol["time"]
    refSolution_state_time_line_date_in_days <-
      get_time_stamp_state_solution(
        start_date = start_date,
        refSolution_state_time_line_idx
      )
    refSol["date"] <- refSolution_state_time_line_date_in_days
    refSol["R_t"] <- compute_Rt(refSol, ref_par)
    refSol_df <- data.frame(refSol)
    path = paste(outputh_path,"/reference_solution", suffix, ".csv", sep='')
    write.csv(refSol_df, path, row.names = FALSE)
    res <- list()
    res$refSol <-refSol_df
    res$conterfactual_path <- path
    return(res)
  }
