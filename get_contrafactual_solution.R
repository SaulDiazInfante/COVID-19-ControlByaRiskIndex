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

get_contrafactual_solution <- 
  function(
    ref_par = 'reference_parameters.json', rhs = evaluateRhsODE, time_line
  ){
    refPar <-
      loadTransferParameters(file_name = 'reference_parameters.json')
    initialConditions <- 
      loadInitialConditions(file_name = 'reference_parameters.json')
    refSol <- 
      getOdeSolution(
        evaluateRhsODE,
        timeline = time_line,
        par = refPar,
        init = initialConditions
      )
    # reference solution time stamp tagging
    start_date <- ymd(20200101)
    refSolution_state_time_line_idx <- refSol["time"]
    refSolution_state_time_line_date_in_days <- 
      get_time_stamp_state_solution(
        start_date=start_date, 
        refSolution_state_time_line_idx
      )
    refSol["date"] <- refSolution_state_time_line_date_in_days
    refSol_df <- data.frame(refSol)
    write.csv(refSol_df,"reference_solution.csv", row.names = FALSE)
  return(refSol_df)
}
#timeLine <- seq(0, 1092, 1)
# refeSol <- get_contrafactual_solution(grid=timeLine)