library(deSolve)
library(rjson)
library(reshape2)
library(dplyr)
library(plotly)
#
source("loadInitialConditions.R")
source("loadTransferParameters.R")
source("evaluateRhsODE.R")
source("getOdeSolution.R")
source("updatePolicy.R")
# default parameters in modelParameters.json

refPar <- 
  loadTransferParameters(file_name = 'reference_parameters.json')
par <- 
  loadTransferParameters(file_name = 'scene01.json')
initialConditions <- 
  loadInitialConditions(file_name = 'reference_parameters.json')
### Solve system ###
timeLine = seq(0, 1000, 1)
splitedTimeLine <-
  split(
    timeLine,
    ceiling(seq_along(timeLine)/7)
  )
# First iteration
currentTimeWeek <- unlist(splitedTimeLine[[1]])
currentState <- initialConditions
# TODO:update policy
par <- updatePolicy(currentState, par)

# Contrafactual solution
refSol <- getOdeSolution(
  evaluateRhsODE,
  timeline = timeLine,
  par = par,
  init = initialConditions
  )

currentSolution <- getOdeSolution(
  evaluateRhsODE, 
  timeline = currentTimeWeek,
  par = par,
  init = initialConditions
)
#
for (idx in 2:length(splitedTimeLine)) {
  # Select interval of integration
  currentTimeWeek <- unlist(splitedTimeLine[[idx]])
  # Update parameters (making a decision)
  currentState <- tail(currentSolution[2:7], n = 1)
  par <- updatePolicy(currentState, par)
  newSolution <- getOdeSolution(
      evaluateRhsODE, 
      timeline = currentTimeWeek,
      par = par,
      init = as.numeric(currentState)
  )
  currentSolution <- bind_rows(currentSolution, newSolution)
}
cotrolledSolution <- currentSolution
# plotting
fig <- plot_ly(
  type = "scatter", 
  mode = "none")  %>% 
  add_trace(
    x = timeLine,
    y = refSol[, 5],
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
    x = timeLine,
    y = cotrolledSolution[, 5],
    mode = "lines",
    line = 
      list(
        #color = 'green',
        dash = "solid"
      ),
    showlegend = TRUE,
    name = paste('scene0', cont, sep = '')
  ) %>%
  layout(
    xaxis = list(
      title = list(text = 'time (days)')
      ),
    yaxis = list(
      title = list(text = 'Prevalence')
    )
  )
fig
htmlwidgets::saveWidget(as_widget(fig), "figure.html")



