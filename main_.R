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
# default parameters in modelParameters.json
refPar <-
  loadTransferParameters(file_name = "reference_parameters.json")
par <-
  loadTransferParameters(file_name = "scene01.json")
initialConditions <-
  loadInitialConditions(file_name = "reference_parameters.json")
### Solve system ###
timeline <- seq(0, 1092, 1)
refSol <-
  getOdeSolution(
    evaluateRhsODE,
    timeline = timeline,
    par = refPar,
    init = initialConditions
  )
scenarios <- list(
  "scene01.json",
  "scene02.json",
  "scene03.json",
  "scene04.json"
)
fig <- plot_ly(
  type = "scatter",
  mode = "none"
) %>%
  add_trace(
    x = timeline,
    y = refSol[, 5],
    mode = "lines",
    line =
      list(
        color = "black",
        dash = "dash"
      ),
    showlegend = TRUE,
    name = "without control"
  )

cont <- 1
for (scene in scenarios) {
  print(scene)
  init <-
    loadInitialConditions(file_name = scene)
  sol <-
    getOdeSolution(
      evaluateRhsODE,
      timeline = timeline,
      par = par,
      init = init
    )
  fig <- fig %>%
    add_trace(
      x = sol[, 1],
      y = sol[, 5],
      mode = "lines",
      line =
        list(
          # color = 'green',
          dash = "solid"
        ),
      showlegend = TRUE,
      name = paste("scene0", cont, sep = "")
    )
  cont <- cont + 1
}
### Figures ###
fig
htmlwidgets::saveWidget(as_widget(fig), "figure.html")
