
collect_depth_charts <- function(SAFE_SEASONS) {
  # cleaning up the horrifically messy depth chart endpoint
  
  raw <- nflreadr::load_depth_charts(SAFE_SEASONS)
  clean <- vctrs::vec_slice(raw, raw$position %in% c('QB', 'WR', 'RB', 'TE'))
  clean <- clean %>%
    apply_name_conventions() %>%
    select(
      id_season,
      id_posteam = club_code,
      game_type,
      id_week,
      id_gsis,
      position,
      full_name,
      rank = depth_team
    ) %>%
    fix_team_names(id_posteam) %>%
    drop_na() %>%
    unique()
  
  # NOTES on Superbowl weeks:
  # when game_type == post, it's the Superbowl
  # this is across all seasons, not just newer
  # this week has to be row binded by itself, with the new week
  superbowls <- vctrs::vec_slice(clean, clean$game_type == 'POST')
  superbowls$id_week <- if_else(superbowls$id_week <= 2020, 21, 22)
  non_superbowls <- vctrs::vec_slice(clean, clean$game_type != 'POST')
  reordered_weeks <- bind_rows(superbowls, non_superbowls)
  assert_same_sum(reordered_weeks, clean)
  
  # NOTES on end-of-season charts:
  # there are normal charts for the last week
  # then there are charts for the end of season
  # this is probably to allow teams resting starters to reset before post season
  # for our case, we have the ESPN charts for up-to-date starters
  # so we can get rid of the end-of-season charts
  SHORT_SEASON_EOS <- expr(!(id_season <= 2020 & id_week == 18))
  LONG_SEASONS_EOS <- expr(!(id_season >= 2021 & id_week == 19))
  eos_removed <- reordered_weeks %>%
    filter(!!SHORT_SEASON_EOS, !!LONG_SEASONS_EOS)
  
  # now super bowls are re-arranged and eos are dropped
  # we can subtract 1 week from all postseason games
  subtracted_weeks <- eos_removed %>%
    mutate(id_week = case_when(id_season <= 2020 & id_week >= 19 ~ id_week - 1,
                               id_season >= 2021 & id_week >= 20 ~ id_week - 1,
                               TRUE ~ id_week))
  expect_id_completion(subtracted_weeks)
  
  # NOTES on the duplicate player phenom:
  # it appears the depth charts natively come with duplicated players
  # most times the rank is also duplicated
  # we'll group slice for now
  deduplicated_players <- subtracted_weeks %>%
    group_by(id_season, id_week, id_gsis) %>%
    slice(1) %>%
    ungroup()
  
  with_game_id <- deduplicated_players %>%
    left_join(select(game_team_data, id_game, id_week , id_season, id_posteam),
              by = c('id_season','id_posteam','id_week')) %>%
    # some bye weeks ??
    drop_na(id_game) %>%
    relocate(id_game)
  expect_id_completion(with_game_id)
  expect_resolved_suffix(with_game_id)
  
  if (2020 %in% SAFE_SEASONS) {
    no_qb_game <- tibble(
      id_game = '2020_13_DEN_KC',
      id_season = 2022,
      id_week = 13,
      id_posteam = 'DEN',
      rank = as.character(1),
      position = 'QB',
      full_name = 'unknown',
      id_gsis = 'MISSING'
    )
    with_game_id <- bind_rows(with_game_id, no_qb_game)
    
  }

  assert_team_representation(with_game_id, warn = T)
  
  return(with_game_id)
  
}
