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

get_contrafactual_solution <- 
  function(
    ref_par = 'reference_parameters.json', rhs = evaluateRhsODE, grid
  ){
    refPar <-
      loadTransferParameters(file_name = 'reference_parameters.json')
    initialConditions <- 
      loadInitialConditions(file_name = 'reference_parameters.json')
    refSol <- getOdeSolution(
      evaluateRhsODE,
      timeline = grid,
      par = refPar,
      init = initialConditions
  )  
  return(refSol)
}
timeLine <- seq(0, 1092, 1)
refeSol <- get_contrafactual_solution(grid=timeLine)