
decode_sample <- function(sample,
                          next_play) {
  # copy IDs from your game into the sample
  # replaces the context of the sample with your context
  # copies anything from post play into the new sample

  COPY_COLS <- exprs(
    # basic
    id_game,
    id_week,
    id_season,
    roof,
    # game_date,
    # team
    id_posteam,
    id_defteam,
    id_home_team,
    id_away_team,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    # time
    game_seconds_remaining,
    half_seconds_remaining,
    # game_half,
    # start_play_qtr_seconds,
    # scores
    posteam_score,
    defteam_score,
    score_differential,
    spread_line,
    # elements of situation sampler that can vary
    ydstogo,
    wp,
    yardline_100
  )

  assert_cols(next_play, !!!COPY_COLS)

  cols <- COPY_COLS %>% as.character()

  collapse::get_vars(sample, cols) <- collapse::get_vars(next_play, cols)

  return(sample)

}







