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
source("get_time_policy_plot.R")
#
plot_scene <- 
  function(
    policy_file_name = 'light_traffic_policy.csv',
    controlled_sol_file_name = 'controlled_solution.csv',
    contrafactual_sol_file_name = 'reference_solution.csv',
    fig_file_name = 'figure.html'){
  #
  policy <- read_csv(policy_file_name)
  controlledSolution <- read_csv(controlled_sol_file_name)
  refSol <- read_csv(contrafactual_sol_file_name)
  #
  fig_1 <- plot_ly(
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
  fig_1 <- fig_1 %>%
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
#  
  fig_2 <- get_time_policy_plot()
  # subplot
  fig <- subplot(fig_1, fig_2, nrows = 2, heights = c(0.8,0.2), shareX = TRUE)
  fig <- 
    fig %>% 
      layout(
        title = "Contrafactual vs controlled dynamics",
        xaxis = list(domain=list(x=c(0,1),y=c(0.2, 1))),
        xaxis2 = list(domain=list(x=c(0, 1),y=c(0.0, 0.2))),
        showlegend=TRUE,
        showlegend2=FALSE
      )
  htmlwidgets::saveWidget(as_widget(fig), "figure.html")
  return(fig)
  }