library(vistime)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(tidyquant)


get_timeline_policy_transitions <- 
  function(policy_file_name = 'light_traffic_policy.csv',
           solution_file = "controlled_solution.csv"){
    #
    policy <- read_csv(policy_file_name)
    controlledSol <- read_csv(solution_file)
    solution_time_line <- controlledSol["date"]
    solution_last_time <- solution_time_line$date[nrow(solution_time_line)]
    first_record <- policy[1, ]
    
    # first_record[1, 5] <- ymd(20200101)
    # transitions <-bind_rows(first_record, policy[1, ])
    transitions <- first_record
    current_policy <- policy[1,'u_semaphore']
    for (i in 2:nrow(policy)){
      if (current_policy != policy[i, 4]){
        record <- policy[i, ]
        transitions <-bind_rows(transitions, record)
        current_policy <- policy[i, 4]
      }
    }
    last_record <- policy[nrow(policy), ]
    transitions <-bind_rows(transitions, last_record)
    transitions_shift_lag <- shift(transitions[, "dates"], type="lag")[[1]]
    transitions_shift_lead <- shift(transitions[, "dates"], type="lead")[[1]]
    timeline_events <- transitions[1 : nrow(transitions) - 1, ]
    timeline_events["start"] <- 
      transitions_shift_lag[2:length(transitions_shift_lag)]
    timeline_events["end"] <- 
      transitions_shift_lead[1:length(transitions_shift_lag)- 1]
    if (last_record$dates != solution_last_time){
      timeline_events$end[nrow(timeline_events)] <- solution_last_time
    }
    timeline_events <- timeline_events %>% 
      mutate(color = 
          ifelse(
            u_semaphore == "green", 
              "#008450",
              ifelse(
                u_semaphore == "yellow",
                  "#ffd966",
                  ifelse(
                    u_semaphore == "orange",
                    "#FF5733",
                    ifelse(
                      u_semaphore == "red",
                      "#b81d13",
                      "---"
                    )
                  )
              )
          )
      )
    write.csv(timeline_events, "light_traffic_policy_transitions.csv",
              row.names = FALSE)
    return(timeline_events)
  }