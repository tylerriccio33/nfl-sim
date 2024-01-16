
simulate_touchback <- function(play) {
  # reset yardline to touchback
  play$yardline_100 <- 25
  # flip sides
  play <- flip_team(play)
  return(play)
}


simulate_fourth_down_decision <- function(sample_fourth_downs) {
  # summarize average decision
  assert("No samples  passed to 4d simulator" = fnrow(sample_fourth_downs) != 0)
  assert("All samples should be fourth down" = all(sample_fourth_downs$down == 4))


  pa <- fsum(sample_fourth_downs$punt_attempt)
  fg <- fsum(sample_fourth_downs$field_goal_attempt)
  go <- fsum(
    sample_fourth_downs$fourth_down_converted == 1 |
      sample_fourth_downs$fourth_down_converted == 1
  )
  order_names <- c('pa', 'fg', 'go')

  sums <- c(pa, fg, go)
  names(sums) <- order_names

  i <- whichv(sums, value = max(sums))[1]

  decision <- order_names[i]

  return(decision)

}


simulate_dropback_decision <- function(samples){
  avg_dropback <- mean(samples$qb_dropback)
  s <- sample(c(0, 1), size = 1, prob = c(1 - avg_dropback, avg_dropback))
  dropback <- s == 1
  return(dropback)
}



select_special_sample <- function(samples, decision) {
  assert_cols(samples,
              punt_attempt,
              field_goal_attempt,
              id_season,
              id_week)
  if (decision == 'pa') {
    correct_decision_samples <- vctrs::vec_slice(samples, samples$punt_attempt == 1)
  } else if (decision == 'fg') {
    correct_decision_samples <-
      vctrs::vec_slice(samples, samples$field_goal_attempt == 1)
  } else {
    cli_abort(glue("Decision not in pa or fg -> {decision}"))
  }
  # arrange recent
  # no need to get complicated here
  selected_sample <- collapse::roworder(correct_decision_samples, -id_season, -id_week)
  selected_sample <- ss(selected_sample, i = 1)

  return(selected_sample)

}


simulate_pos_score <- function(data, score) {

  data$posteam_score <- data$posteam_score + score
  # recalculate differential
  data$score_differential <- data$posteam_score - data$defteam_score
  # reset yardline to touchback
  data$yardline_100 <- 25
  # touchdown team
  data$td_team <- data$id_posteam
  # flip sides
  data <- flip_team(data)
  return(data)
}


simulate_def_score <- function(play) {

  play$posteam_score <- play$posteam_score + 7
  # recalculate differential
  play$score_differential <- play$posteam_score - play$defteam_score
  # reset yardline to touchback
  play$yardline_100 <- 25
  # td team
  data$td_team <- data$id_defteam
  # no flipping sides

  return(play)

}

simulate_punt <- function(play) {
  assert_cols(play, yardline_100, touchback, kick_distance)

  play <- play %>%
    mutate(yardline_100 = if_else(
      .data$touchback == 1,
      25,
      .data$yardline_100 - .data$kick_distance
    ))

  assert("Some yardlines were negative in simulate punt." = all(play$yardline_100 >=
                                                                  1))
  assert("Some yardlines were impossible in simulate punt." = all(play$yardline_100 <=
                                                                    100))

  # Flip Teams #
  play <- flip_team(play)

  return(play)

}



# simulate_qb_dropback <- function(play) {
#   assert_cols(play, xpass)
#   assert("Xpass can't be NA to simulate dropback." = !is.na(play$xpass))
#   play$qb_dropback <-
#     sample(c(0, 1),
#            size = 1,
#            prob = c(1 - play$xpass, play$xpass))
#   return(play)
# }












