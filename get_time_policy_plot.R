library(vistime)
library(dplyr)
library(data.table)
library(lubridate)
library(progress)
library(plotly)
source("get_timeline_policy_transitions.R")

get_time_policy_plot <-
  function(file_events = "light_traffic_policy_transitions.csv") {
    # trs <- get_timeline_policy_transitions()
    #
    trs <- read_csv(file_events)
    color_span <- difftime(
      lubridate::ymd(trs$end),
      lubridate::ymd(trs$start)
    )
    trs$color_span <- color_span

    timelineData <-
      data.frame(
        event = trs["color_span"],
        start = trs["start"],
        end = trs["end"],
        # group = "u_semaphore",
        color = trs["color"],
        fontcolor = c(rep("black", length(trs$color_span)))
      )
    names(timelineData) <- c("event", "start", "end", "color", "fontcolor")
    p <-
      vistime(timelineData,
        col.event = "event",
        col.start = "start",
        col.end = "end",
        title = "Risk Semaphore",
        # col.group = "group",
        col.color = "color",
        optimize_y = TRUE,
        show_labels = TRUE,
        background_lines = 17,
        linewidth = 25
      )
    pp <- plotly::plotly_build(p)
    for (i in seq_along(pp$x$data)) {
      if (pp$x$data[[i]]$mode == "text") {
        pp$x$data[[i]]$textfont$size <- 6
      }
    }
    return(pp)
  }
# timeline_fig <- get_time_policy_plot()
# htmlwidgets::saveWidget(as_widget(timeline_fig), "timeline_figure.html")
