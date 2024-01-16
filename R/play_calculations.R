
# ! prefix these with calc_

flip_team <- function(play, yards_added = 0, touchback_on_invalid_yards = F) {
  # yards added for interception, fumble returns
  # in the case of yards lost, yards_added is just set to negative


  stored_posteam <- play$id_posteam

  # negate differential
  play$score_differential <- -play$score_differential
  play$spread_line <- -play$spread_line
  # 1st and 10
  play$down <- 1
  play$ydstogo <- 10
  # reverse yardline with new yards added on play
  play$yardline_100 <- 100 - (play$yardline_100 - yards_added)
  # run off 6 seconds
  play$half_seconds_remaining <- play$half_seconds_remaining - 6
  play$game_seconds_remaining <- play$game_seconds_remaining - 6
  # don't let seconds go negative
  play$half_seconds_remaining <- ifelse(play$half_seconds_remaining < 0, 0, play$half_seconds_remaining)
  play$game_seconds_remaining <- ifelse(play$game_seconds_remaining < 0, 0, play$game_seconds_remaining)

  if (touchback_on_invalid_yards) {
    cur_yardline <- play$yardline_100
    if (cur_yardline > 0) {
      play$yardline_100 <- 20
    }
  }

  play <- play %>%
    fswap_columns("posteam_score",
                  "defteam_score")
  play <- play %>%
    fswap_columns("id_posteam",
                  "id_defteam")



  return(play)

}

calculate_next_play_values <- function(post_play) {
  # assume everything is swapped
  # here you can re-calculate vector operations and cols

  post_play$game_seconds_remaining <-
    post_play$game_seconds_remaining - post_play$total_play_time

  post_play$receive_2h_ko = ifelse(post_play$id_home_team == post_play$id_posteam, 1, 0)

  post_play <- add_wp(post_play)

  return(post_play)
}

adjust_down_distance <- function(simulated_play) {

  ydstogo <- simulated_play$ydstogo - simulated_play$yards_gained

  # Made first down #
  if (ydstogo <= 0) {
    simulated_play$down <- 1
    simulated_play$ydstogo <- 10
  } else if (ydstogo >= 1) {
    simulated_play$down <- simulated_play$down + 1
    simulated_play$ydstogo <-
      simulated_play$ydstogo - simulated_play$yards_gained
  } else {
    rlang::abort("Breakdown in logic for adjust down distance.")
  }

  simulated_play$yardline_100 <-
    simulated_play$yardline_100 - simulated_play$yards_gained

  return(simulated_play)

}





