library(vistime)
library(dplyr)
library(data.table)
library(lubridate)

get_timeline_policy_transitions <- 
  function(policy_file_name = 'light_traffic_policy.csv'){
    #
    policy <- read_csv(policy_file_name)
    
    first_record <- policy[1, ]
    first_record[1, 5] <- ymd(20200101)
    transitions <-bind_rows(first_record, policy[1, ])
    current_policy <- policy[1,'u_semaphore']
    # TODO: Fix time line for start and end dates of each event
    for (i in 2:nrow(policy)){
      if (current_policy != policy[i, 4]){
        record <- policy[i, ]
        transitions <-bind_rows(transitions, record)
        current_policy <- policy[i, 4]
      }
    }
    last_record <- policy[nrow(policy), ]
    transitions <-bind_rows(transitions, last_record)
    # last_record <- policy[nrow(policy), ]
    transitions_shift_lag <- shift(transitions[, "dates"], type="lag")[[1]]
    transitions_shift_lead <- shift(transitions[, "dates"], type="lead")[[1]]
    timeline_events <- transitions[1 : nrow(transitions) - 1, ]
    timeline_events["start"] <- 
      transitions_shift_lag[2:length(transitions_shift_lag)]
    timeline_events["end"] <- 
      transitions_shift_lead[1:length(transitions_shift_lag)- 1]
    # TODO: save transitions to a csv data file
    return(timeline_events)
  }