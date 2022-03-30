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
# default parameters in modelParameters.json
#
refPar <-
  loadTransferParameters(file_name = 'reference_parameters.json')
par <-
  # loadTransferParameters(file_name = 'scene01.json')
  loadTransferParameters(file_name = 'scene01.json')
initialConditions <- 
  loadInitialConditions(file_name = 'reference_parameters.json')
#
#
### Solve system ###
#
#
timeLine <- seq(0, 1092, 1)
grid <- classIntervals(timeLine, 156)
intervalIndex <- 1
#
currentTimeWeek <-
    seq(from = grid$brks[intervalIndex], to = grid$brks[intervalIndex + 1])
currentState <- initialConditions
#
par <- updatePolicy(currentState, par)
#
# Contrafactual solution
#
date_policy_idx <- as.integer(0)
u_beta <- as.numeric(par["beta"])
u_k <- as.numeric(par["k1"])
u_semaphore <- par["semaphore"]
policy_df <- data.frame(date_policy_idx, u_beta, u_k, u_semaphore)
names(policy_df) <- c("date_policy_idx", "u_beta", "u_k", "u_semaphore")
refSol <- getOdeSolution(
  evaluateRhsODE,
  timeline = timeLine,
  par = refPar,
  init = initialConditions
  )

currentSolution <- getOdeSolution(
  evaluateRhsODE,
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
      evaluateRhsODE,
      timeline = currentTimeWeek,
      par = par,
      init = as.numeric(currentState)
  )
  currentSolution <- bind_rows(currentSolution, newSolution)
}
controlledSolution <- currentSolution
# timeline stamp tagging

start_date <- ymd(20200101)

# reference solution time stamp tagging

refSolution_state_time_line_idx <- refSol["time"]
refSolution_state_time_line_date_in_days <- 
  get_time_stamp_state_solution(
    start_date=start_date, 
    refSolution_state_time_line_idx
  )
refSol["date"] <- refSolution_state_time_line_date_in_days

#
#
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
write.csv(policy_df,"light_traffic_policy.csv", row.names = FALSE)

# plotting
fig <- plot_ly(
  refSol,
  type = "scatter", 
  mode = "none"
)%>% 
  add_trace(
    x = ~date,
    y = ~xI,
    mode = "lines",
    line = 
      list(
        color = 'black',
        dash = "dash"
      ),
    showlegend = TRUE,
    name = 'without control'
  )
cont <- 1
fig <- fig %>%
  add_trace(
    data = controlledSolution,
    x = ~date,
    y = ~xI,
    mode = "lines",
    line = 
      list(
        #color = 'green',
        dash = "solid"
      ),
    showlegend = TRUE,
    name = paste('scene0', cont, sep = '')
  )
fig
htmlwidgets::saveWidget(as_widget(fig), "figure.html")
