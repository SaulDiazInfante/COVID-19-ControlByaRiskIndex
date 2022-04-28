library(rjson)
library(reshape2)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(plotly)
library(classInt)
library("styler")
library(lubridate)
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
  ) %>%
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
#
  fig_1 <- 
    fig_1 %>%
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
      name = "Controlled"
    )
#  
  fig_2 <- plot_ly(
    refSol,
    type = "scatter", 
    mode = "none"
  ) %>% 
    add_trace(
      x = ~date,
      y = ~R_t,
      mode = "lines",
      line = 
        list(
          color = 'red',
          dash = "dash"
        ),
      showlegend = TRUE,
      name = 'NC R_t'
    )
#
  fig_2 <- fig_2 %>%
    add_trace(
      data = controlledSolution,
      x = ~date,
      y = ~R_t,
      mode = "lines",
      line = 
        list(
          color = 'green',
          dash = "solid"
        ),
      showlegend = TRUE,
      name = "WC R_t"
    )
#
  fig_3 <- plot_ly(
    refSol,
    type = "scatter", 
    mode = "none"
  ) %>% 
  add_trace(
    data = refSol,
    x = ~date,
    y = ~xC,
    mode = "lines",
    line = 
      list(
        color = 'blue',
        dash = "dash"
      ),
    showlegend = TRUE,
    name = "WC Risk"
  )
  fig_3 <- fig_3 %>%
    add_trace(
      data = controlledSolution,
      x = ~date,
      y = ~xC,
      mode = "lines",
      line = 
        list(
        color = 'blue',
        dash = "solid"
      ),
    showlegend = TRUE,
    name = "WC R_t"
  )
#
  fig_4 <- get_time_policy_plot()
# subplot
  fig <- 
    subplot(
    fig_1, fig_2, fig_3, fig_4,
    nrows = 4,
    heights = c(0.45, 0.25, 0.25, 0.05),
    shareX = TRUE
    #titleY=TRUE,
    # titleX=FALSE,
    #margin = 0.1 
  )
  fig <- 
    fig %>% 
      layout(
        title = "Contrafactual vs controlled dynamics"
      )
  htmlwidgets::saveWidget(as_widget(fig), "figure.html")
  return(fig)
}
fig <- plot_scene()