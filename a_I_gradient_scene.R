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
source("compute_parameter_gradient_solutions.R")
source("plot_scene.R")
#
par <-
  loadTransferParameters(file_name = "scene01.json")
initialConditions <-
  loadInitialConditions(file_name = "scene01.json")
gradient_a_I <- compute_parameter_gradient_solutions(
  par,
  initialConditions,
  timeLine = seq(0, 156, 1),
  target_par = "a_I",
  parameter_grid = list(5e-5, 5e-4, 5e-3, 5e-2)
)
#
grad_a_I_controlled_sol_path <- list()
grad_a_I_policy_transitions_path <- list()
header <- list()
for (g_a_ii in gradient_a_I){
  g_a_ii_path <- g_a_ii[["data_path"]]
  grad_a_I_controlled_sol_path <- 
    append(grad_a_I_controlled_sol_path, g_a_ii_path[2])  
  grad_a_I_policy_transitions_path <- 
    append(grad_a_I_policy_transitions_path, g_a_ii_path[4])
}
#
plot_gradient_scene(
  contrafactual_sol_file_name = "reference_solution.csv",
  fig_file_name = "figure_04",
  file_solutions_list = grad_a_I_controlled_sol_path,
  file_events_list = grad_a_I_policy_transitions_path, 
  # color_palette = 'PuBu'
  color_palette = 'Accent'
)