library(rjson)
library(reshape2)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(plotly)
library(classInt)
library("styler")
library(lubridate)
library(tidyverse)
#
plot_scene <- 
  function(policy_file_name = 'light_traffic_policy.csv',
    controlled_sol_file_name = 'controlled_solution.csv',
    contrafactual_sol_file_name = 'reference_solution.csv',
    fig_file_name = 'figure.html'){
  #
  policy <- read_csv(policy_file_name)
  controlledSolution <- read_csv(controlled_sol_file_name)
  refSol <- read_csv(contrafactual_sol_file_name)
  #
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
      name = fig_file_name
    )
  htmlwidgets::saveWidget(as_widget(fig), "figure.html")
  return(fig)
  }