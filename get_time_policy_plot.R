library(vistime)
library(dplyr)
library(data.table)
library(lubridate)
source("get_timeline_policy_transitions.R")

trs = get_timeline_policy_transitions()
timelineData <- 
  data.frame(
    event = trs["u_semaphore"],
    start = trs["start"],
    end = trs["end"],
    group = "semaphore",
    color = c('#b81d13', '#008450' ),
    fontcolor = c("white", "white")
  ) 

names(timelineData) <-c("event", "start", "end", "group","color", "fontcolor")
timeline_fig <- 
  vistime(timelineData, 
    col.event = "event",
    col.start = "start",
    col.end = "end",
    title = "Risk Semaphore",
    col.group="group",
    col.color= "color",
    optimize_y = TRUE, 
    show_labels = TRUE
)
#htmlwidgets::saveWidget(as_widget(timeline_fig), "timeline_figure.html")


