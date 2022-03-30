get_time_stamp <- function(start_date = ymd("2020-01-01"),  time_line){
  delta_time <- days(time_line[1, 1])
  new_date <- start_date + delta_time
  new_week_date <- week(start_date + delta_time)
  line_time_stamps <- data.frame(new_date) 
  names(line_time_stamps) <- c("date")
  for (idx in time_line[2:nrow(time_line) , 1]){
    delta_time <- days(as.integer(idx + 0.5))
    new_date <- start_date + delta_time
    line_time_stamps[nrow(line_time_stamps) + 1, 1] = new_date 
  }
  return(line_time_stamps)
}