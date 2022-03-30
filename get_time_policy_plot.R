library(vistime)
library(dplyr)
library(data.table)

get_timeline_policy_plot <- function(timeline_data){
  #
  transitions <- timeline_data[1, ]
  current_policy <- timeline_data[1, "u_semaphore"] 
  # TODO: Fix time line for start and end dates of each event
  for (i in 2:nrow(timeline_data)){
     if (current_policy != timeline_data[i, 4]){
       record <- timeline_data[i, ]
       transitions <-bind_rows(transitions, record)
       current_policy <- timeline_data[i, 4]
     }
  }
  
  transitions <-bind_rows(transitions, timeline_data[nrow(timeline_data), ])
  transitions_shift_lag <- shift(transitions[, "dates"], type="lag")
  transitions_shift_lead <- shift(transitions[, "dates"], type="lead")
  timeline_events <- transitions[1 : nrow(transitions) - 1, ]
  timeline_events["start"] <- 
    transitions_shift_lag[2:length(transitions_shift_lag)]
  timeline_events["end"] <- 
    transitions_shift_lead[1:length(transitions_shift_lag)- 1]
  return(timeline_events)
}
timeline_data <- policy_df
trs = get_timeline_policy_plot(timeline_data=timeline_data)
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
htmlwidgets::saveWidget(as_widget(timeline_fig), "timeline_figure.html")


