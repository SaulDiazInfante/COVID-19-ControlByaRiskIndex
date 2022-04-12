library(vistime)
library(dplyr)
library(data.table)
library(lubridate)
library(progress)
source("get_timeline_policy_transitions.R")

get_time_policy_plot <- 
  function(file_events = "light_traffic_policy_transitions.csv"){
    # trs <- get_timeline_policy_transitions()
    # 
    trs <- read_csv(file_events)
    
    timelineData <-
      data.frame(
        event = trs["u_semaphore"],
        start = trs["start"],
        end = trs["end"],
        #group = "u_semaphore",
        color = trs["color"]
        #fontcolor = c("white", "white", "white")
      )
    names(timelineData) <- c("event", "start", "end", "color")
    timeline_fig <-
      vistime(timelineData,
              #col.event = "event",
              col.start = "start",
              col.end = "end",
              title = "Risk Semaphore",
              #col.group = "group",
              col.color = "color",
              optimize_y = TRUE,
              show_labels = FALSE
      )
    return(timeline_fig)
  }
#timeline_fig <- get_time_policy_plot()
#htmlwidgets::saveWidget(as_widget(timeline_fig), "timeline_figure.html")
