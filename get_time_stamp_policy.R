get_time_stamp_policy <- 
  function(start_date = ymd("2020-01-01"), time_line, decision_period_lenght){
  delta_time <- decision_period_lenght * weeks(time_line[1, 1])
  new_date <- start_date + delta_time
  line_time_stamps <- data.frame(start_date) 
  names(line_time_stamps) <- c("date")
  for (idx in time_line[2: nrow(time_line) , 1]){
    delta_time <- decision_period_lenght * weeks(as.integer(idx - 1))
    new_date <- start_date + delta_time
    line_time_stamps[nrow(line_time_stamps) + 1, 1] = new_date 
  }
  return(line_time_stamps)
}