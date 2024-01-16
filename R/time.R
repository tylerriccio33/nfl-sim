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


add_time_variables <- function(data) {
  assert("Data should not be grouped when passed to time variable adder." =
           !is_grouped_df(data))
  assert_cols(data, half_seconds_remaining, qtr, time, end_clock_time)

  # Add Game Seconds #
  # data$game_seconds <-
  #   half_seconds_to_game_seconds(hs = data$half_seconds_remaining,
  #                                qtr = data$qtr)

  # Add QTR Seconds #
  data$start_play_qtr_seconds <- raw_time_to_qtr_seconds(data$time)

  # Format End Clock Time #
  data$end_play_qtr_seconds <-
    raw_time_to_qtr_seconds(data$end_clock_time)


  # Add Play Run Time #
  data$play_run_time <-
    data$start_play_qtr_seconds - data$end_play_qtr_seconds

  data <- calculate_total_play_time(data)

  return(data)

}


calculate_total_play_time <- function(data) {
  assert_cols(data, id_game, game_seconds_remaining)
  assert("Grouped df passed to play time calculater." = !is_grouped_df(data))

  with_time <- data %>%
    # arrange
    group_by(id_game) %>%
    arrange(id_play, .by_group = T) %>%
    # time from next play start
    mutate(total_play_time = .data$game_seconds_remaining - lead(.data$game_seconds_remaining)) %>%
    # if total play time is missing, game is over
    mutate(total_play_time = if_else(
      is.na(.data$total_play_time),
      .data$game_seconds_remaining,
      .data$total_play_time
    )) %>%
    ungroup()

  return(with_time)

}
