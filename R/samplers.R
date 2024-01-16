initialize_game <-
  function(matchup) {
    # Home starts automatically

    matchup <- add_vars(matchup, id_posteam = matchup$id_home_team)
    matchup <- add_vars(matchup, id_defteam = matchup$id_away_team)
    matchup$id_passer <- matchup$id_home_qb    # sometimes doesn't exist
    matchup <- add_vars(matchup, yardline_100 = 75)
    matchup <- add_vars(matchup, game_seconds_remaining = 3600)
    matchup <- add_vars(matchup, qtr = 1)
    matchup <- add_vars(matchup, half_seconds_remaining = 1800)
    matchup <- add_vars(matchup, down = 1)
    matchup <- add_vars(matchup, ydstogo = 10)
    matchup <- add_vars(matchup, posteam_score = 0)
    matchup <- add_vars(matchup, defteam_score = 0)
    matchup <- add_vars(matchup, score_differential = 0)
    matchup <- add_vars(matchup, posteam_timeouts_remaining = 3)
    matchup <- add_vars(matchup, defteam_timeouts_remaining =  3)
    matchup <- add_vars(matchup, receive_2h_ko = 0)
    # these are meant to be overwritten
    matchup <- add_vars(matchup, wp = .5)
    matchup <- add_vars(matchup, vegas_wp = .5)

    return(matchup)

  }


slice_posteam_samples <- function(samples, id_posteam) {
  assert_cols(samples, id_posteam)
  samples <- vec_slice(samples, samples$id_posteam == id_posteam)
  return(samples)
}

slice_defteam_samples <- function(samples, id_defteam) {
  assert_cols(samples, id_defteam)
  samples <- vec_slice(samples, samples$id_defteam == id_defteam)
  return(samples)
}

slice_no_dropback <-
  function(samples) {
    assert_cols(samples, qb_dropback)
    data <-
      vctrs::vec_slice(
        samples, samples$qb_dropback == 0
      )
    assert("Pre allocated was 0" = nrow(data) != 0)
    return(data)
  }

pre_allocate_pass_samples <- function(sample_pbp_data, id_passer) {
  assert_cols(sample_pbp_data, id_passer)

  data <- collapse::na_omit(sample_pbp_data, cols = .c(id_passer))

  data <- vec_slice(data, data$id_passer == id_passer)

  if (collapse::fnrow(data) == 0) {
    cli_abort("No passer samples found.")
  }
  return(data)
}

pre_allocate_team_pass_samples <- function(sample_pbp_data, id_defteam) {
  assert_cols(sample_pbp_data, id_defteam, qb_dropback)

  data <- collapse::na_omit(sample_pbp_data, cols = .c(id_defteam, qb_dropback))

  data <- vec_slice(data, data$id_defteam == id_defteam)
  data <- vec_slice(data, data$qb_dropback == 1)

  if (collapse::fnrow(data) == 0) {
    cli_abort("No passer samples found.")
  }
  return(data)
}

append_roster_relevance <- function(samples, similarity_lookup) {
  assert_cols(samples, id_game, id_posteam)
  assert_cols(similarity_lookup, id_game, id_posteam)

  samples <- samples %>%
    left_join(
      similarity_lookup,
      by = c("id_game", "id_posteam")
    )
  expect_resolved_suffix(samples)
  return(samples)
}

slice_specials <- function(samples) {
  assert_cols(samples, field_goal_attempt, punt_attempt)
  samples <-
    vec_slice(
      samples,
      samples$field_goal_attempt == 1 | samples$punt_attempt == 1
    )
  assert("No special samples found" = nrow(samples != 0))
  return(samples)
}

slice_reg_down <- function(samples) {
  assert_cols(samples, down)
  samples <- vctrs::vec_slice(samples, samples$down <= 3)
  return(samples)
}

slice_fourth_down <- function(samples) {
  assert_cols(samples, down)
  samples <- vctrs::vec_slice(samples, samples$down == 4)
  return(samples)
}

slice_fourth_down_samples_to_go <- function(fourth_down_samples, assert_empty = T) {
  assert_cols(fourth_down_samples)
  samples <- vec_slice(fourth_down_samples, fourth_down_samples$fourth_down_converted == 1 |
                         fourth_down_samples$fourth_down_failed == 1)
  if (assert_empty) {
    assert("No to-go samples in fourth down slicer" = nrow(samples) != 0)
  }
  return(samples)
}
