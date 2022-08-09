library(vistime)
library(dplyr)
library(data.table)
library(lubridate)
library(progress)
library(plotly)
source("get_timeline_policy_transitions.R")

get_time_multi_policy_plot <-
  function() {
    data_folder <- 'simulated_data'
    file_events = list(
      "light_traffic_policy_transitions_a_beta_1.csv",
      "light_traffic_policy_transitions_a_beta_2.csv",
      "light_traffic_policy_transitions_a_beta_3.csv"
    )
    count <- 1
    path <- paste(data_folder, file_events[1], sep='/')
    df_aux <- read_csv(path)
    color_span <- difftime(
      lubridate::ymd(df_aux$end),
      lubridate::ymd(df_aux$start)
    )
    df_aux$color_span <- color_span
    df_aux$group <- paste("Policy", count)
    #
    for (file in file_events[2: length(file_events)]) {
      path <- paste(data_folder, file, sep='/')
      trs <- read_csv(path)
      color_span <- difftime(
        lubridate::ymd(trs$end),
        lubridate::ymd(trs$start)
      )
      count <- count + 1
      trs$color_span <- color_span
      trs$group <- paste("Policy",count)
      df_aux = bind_rows(df_aux, trs)
    }
    df_trs <- df_aux
    timelineData <-
      data.frame(
        event = df_trs["color_span"],
        start = df_trs["start"],
        end = df_trs["end"],
        group = df_trs["group"],
        color = df_trs["color"],
        fontcolor = c(rep("black", length(df_trs$color_span)))
      )
    names(timelineData) <- 
      c("event", "start", "end", "group", "color", "fontcolor")
    p <-
      vistime(timelineData,
        col.event = "event",
        col.start = "start",
        col.end = "end",
        title = "Risk Semaphore",
        col.group = "group",
        col.color = "color",
        optimize_y = TRUE,
        show_labels = TRUE,
        #background_lines = 17,
        linewidth = 25
      )
    pp <- plotly::plotly_build(p)
    for (i in seq_along(pp$x$data)) {
      if (pp$x$data[[i]]$mode == "text") {
        pp$x$data[[i]]$textfont$size <- 6
      }
    }
    pp <- pp %>% 
        layout(
          xaxis = list(
            showticklabels=T,
            zerolinecolor = 'ffff',
            linecolor='white',
            showline= FALSE,
            mirror = FALSE,
            ticktext=list("Start", "End"),
            tickvals=list("2020-01-01", "2022-12-28")
          ),
          yaxis = list(
            showticklabels=T,
            fixedrange=TRUE,
            tickfont=list(size=14, color="black"),
            showgrid=FALSE,
            showline= F,
            mirror = F
          )
        )
    return(pp)
  }
# timeline_fig <- get_time_policy_plot()
# htmlwidgets::saveWidget(as_widget(timeline_fig), "timeline_figure.html")
