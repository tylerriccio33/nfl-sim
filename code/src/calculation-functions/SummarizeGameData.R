SummarizeGameData <- function(raw_data, game_team_data, remove_defensive_stats = T) {
  
  future_games <- anti_join(game_team_data, raw_data, by = c('id_game'))
  
  ## Clean ##
  clean_data <- raw_data %>%
    apply_name_conventions() %>%
    drop_na(id_game,
            id_posteam,
            id_defteam,
            id_season,
            id_week) %>%
    fix_team_names(id_posteam, id_defteam)
  expect_resolved_suffix(clean_data)
  assert_team_representation(clean_data)
  
  
  ## Collapse Drives ##
  drive_collapsed_data <- clean_data %>%
    # Collapse PBP to drive level
    collapse_drives(drive_wpa = sum(last(wp, na_rm = T), last(wpa, na_rm = T), na.rm = T) - first(wp)) %>%
    # Collapsing by game
    group_by(id_game,
             id_posteam,
             id_defteam,
             id_week,
             id_season) %>%
    summarize(
      sum_drive_points = sum(drive_points),
      mean_drive_points = mean(drive_points),
      mean_drive_wpa = mean(drive_wpa),
      mean_drive_points_oe = mean(drive_points_oe),
      .groups = 'drop'
    )
  expect_resolved_suffix(drive_collapsed_data)
  assert_team_representation(drive_collapsed_data)
  
  ## Collapse PBP ##
  pbp_collapsed_data <- clean_data %>%
    group_by(id_game,
             id_posteam,
             id_defteam,
             id_season,
             id_week) %>%
    summarize(
      
      ##  ADD GAME FEATURES HERE ##
      
      n = n(),
      pass_success = mean(success[qb_dropback == 1]),
      rush_success = mean(success[qb_dropback == 0]),
      pass_epa = mean(epa[qb_dropback == 1]),
      rush_epa = mean(epa[qb_dropback == 0]),
      epa = mean(epa),
      team_wp = mean(wp),
      sr = mean(success),
      .groups = 'drop'
    ) %>% 
    
    ## JOIN TO OTHER DATA HERE ##
    
    left_join(drive_collapsed_data) %>%
    
    reverse_feature(c(
      where(is.numeric),
      -starts_with('id_'),
      -starts_with('team_')
    )) %>%
    over_expected(where(is.numeric),
                  -n,
                  -starts_with('team_'),
                  -starts_with('id_'),
                  team_stat = F) %>%
    over_expected(starts_with('team_'), team_stat = T)
  expect_resolved_suffix(pbp_collapsed_data)
  assert_team_representation(pbp_collapsed_data)
  assert_no_duplicates(pbp_collapsed_data, id_game, id_posteam)
  
  
  ## Roll and Lag ##
  
  # add new games
  pbp_collapsed_data <- pbp_collapsed_data %>% 
    bind_rows(select(future_games, any_of(colnames(pbp_collapsed_data))))
  assert_no_duplicates(pbp_collapsed_data, id_game, id_posteam)
  
  rolled_data <- pbp_collapsed_data %>%
    # remove_clinched_teams()
    group_by(id_posteam) %>%
    arrange(id_season, id_week, .by_group = T) %>%
    mutate(across(c(where(is.numeric), -c(
      starts_with('id_'), n
    )),
    ~ dplyr::lag(rolling_average(.x, window_size = 12)))) %>%
    ungroup() %>%
    select(-n, -n_allowed)
  expect_resolved_suffix(rolled_data)
  assert_no_duplicates(rolled_data, id_game, id_posteam)
  
  ## Pivot Wider ##
  pivoted_data <- rolled_data %>%
    inner_join(y =  select(., -c(
      starts_with('id_'), -id_game, -id_defteam
    )) %>%
      rename(id_posteam = id_defteam) %>%
      rename_with(~ glue("{.x}_opponent"), -c(id_game, id_posteam))) 
  expect_resolved_suffix(pivoted_data)
  expect_id_completion(pivoted_data)
  assert_no_duplicates(pivoted_data, id_game, id_posteam)
  assert_team_representation(pivoted_data)
  
  ## Interact ##
  interaction_data <- pivoted_data %>%
    interact_metrics(c(
      where(is.numeric),
      -c(
        starts_with('team_'),
        starts_with('id_'),
        starts_with('outcome_'),
        ends_with('_opponent')
      )
    ),
    mode = 'unit') %>%
    interact_metrics(c(starts_with('team_'), -ends_with('_opponent')),
                     mode = 'team')
  expect_resolved_suffix(interaction_data)
  assert_team_representation(interaction_data)
  assert_no_duplicates(interaction_data, id_game, id_posteam)
  expect_id_completion(interaction_data)
  
  if (remove_defensive_stats) {
    interaction_data <- interaction_data %>%
      select(-ends_with('_allowed'))
  }
  
  return(interaction_data)
  
  
}
