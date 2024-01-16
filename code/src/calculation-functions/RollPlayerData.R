RollPlayerData <- function(player_epa_profile, xwar_predicted_data) {
  # function to take game-level player data and roll them
  # return should join directly before interactions
  
  assert_cols(player_epa_profile,
              id_game,
              id_posteam,
              id_season,
              id_week,
              id_gsis,
              epa)
  assert_cols(xwar_predicted_data, id_season, id_week, id_gsis)
  expect_id_completion(xwar_predicted_data)
  
  # combine player level data
  # coalesce for position
  combined_data <- player_epa_profile %>%
    powerjoin::power_left_join(
      xwar_predicted_data,
      by = c('id_season', 'id_week', 'id_posteam', 'id_gsis'),
      # relationship = 'one-to-one',
      conflict = powerjoin::coalesce_xy
    )
  
  expect_resolved_suffix(combined_data)
  expect_id_completion(combined_data, id_position)
  
  # collect more nuanced stats
  # e.g. yacoe to denote yac ability, ints to indicate int tendancies
  QB_STATS <-
    exprs(dakota, cpoe = completion_percentage_above_expectation, interceptions, sacks)
  WR_TE_STATS <-
    exprs(racr, target_share, yacoe = avg_yac_above_expectation)
  RB_STATS <-
    exprs(ryoe = rush_yards_over_expected_per_att)
  assert_cols(combined_data, !!!QB_STATS)
  assert_cols(combined_data, !!!WR_TE_STATS)
  assert_cols(combined_data, !!!RB_STATS)
  
  # create separate qb table
  qb_table <- combined_data %>% filter(id_position == 'QB') %>%
    select(starts_with('id_'), id_gsis, xwar, epa, !!!QB_STATS)
  
  # create separate wr/te table
  wr_table <- combined_data %>% filter(id_position %in% c('WR','TE')) %>%
    select(starts_with('id_'), id_gsis, xwar, epa, !!!WR_TE_STATS)
  
  # create separate rb table
  rb_table <- combined_data %>% filter(id_position == 'RB') %>%
    select(starts_with('id_'), id_gsis, xwar, epa, !!!RB_STATS)
  
  # create list of tables
  # roll them
  tables <- list('qb' = qb_table,
                 'wr' = wr_table,
                 'rb' = rb_table) %>%
    map( ~ {
      .x %>%
        group_by(id_gsis) %>%
        arrange(id_season, id_week, .by_group = T) %>%
        mutate(across(
          c(where(is.numeric),-starts_with('id_')),
          ~ if_else(is.na(.x), 0, .x) %>%
            nflModeler::rolling_average(window_size = 15) %>%
            dplyr::lag()
        )) %>%
        ungroup()
    }) %>%
    list_rbind()
  
  
  return(tables)
  
  
}
