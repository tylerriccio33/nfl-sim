half_seconds_to_game_seconds <- function(hs, qtr) {
  assert("Half seconds should be single numeric." = is.numeric(hs))
  
  half <- if_else(qtr < 3, 2, 1)
  
  hs <- hs * half
  
  return(hs)
  
}

raw_time_to_qtr_seconds <- function(time) {
  assert("Time should be in character form, MM:SS." = is.character(time))
  m <- as.numeric(sub(":.*", "", time))
  s <- as.numeric(sub(".*:", "", time))
  qtr_seconds <- m * 60 + s
  return(qtr_seconds)
}

qtr_seconds_to_game_seconds <- function(qtr_seconds, qtr) {
  (5 - qtr) * qtr_seconds
}
