

fix_touchdown <- function(selected_sample) {
  
  touchdown <-
    ifelse(selected_sample$yards_gained >= selected_sample$yardline_100,
            1,
            0)
  if (touchdown == 1) {
    selected_sample$touchdown <- touchdown
    pass <- selected_sample$qb_dropback == 1
    if (pass) {
      selected_sample$pass_touchdown <- 1
    } else {
      selected_sample$rush_touchdown <- 1
    }
    return(selected_sample)
  } else {
    # correct for non-touchdown
    selected_sample$touchdown <- 0
    return(selected_sample)
  }
  
}


fix_turnover_on_downs <- function(selected_sample) {
  validate_play(selected_sample)
  made_first <- selected_sample$yards_gained >= selected_sample$ydstogo
  
  if (made_first) {
    selected_sample$fourth_down_converted <- 1
  } else {
    selected_sample$fourth_down_failed <- 1
  }
return(selected_sample)
}



