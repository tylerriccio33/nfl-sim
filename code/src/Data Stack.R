

filter_data_stack <- function(data_stack, matchup) {
  assert(
    "Data stack passed to filter isn't a tibble." = is_tibble(data_stack),
    "Matchup passed to filter isn't a tibble." = is_tibble(matchup),
    "Matchup should be a 1 row tibble." =
      nrow(matchup == 1)
  )
  assert_cols(data_stack, id_game)
  assert_cols(matchup, id_game)
  
  filtered_stack <-
    vctrs::vec_slice(data_stack, data_stack$id_game == matchup$id_game)
  assert("2 rows could not be found for the matchup." = nrow(filtered_stack))
  return(filtered_stack)
}

integrate_pbp <- function(prepped_data_stack, pbp) {
  assert("Data stack passed to filter isn't a tibble." = is_tibble(prepped_data_stack))
  
  formatted_pbp <- pbp %>%
    # pull in all cols NOT including post play metrics
    # yards gained can be pulled in if it exists
    as_tibble() %>%
    fix_team_names(id_posteam) %>%
    select(
      id_game,
      id_season,
      id_week,
      id_posteam,
      id_defteam,
      any_of(c('yards_gained')),
      qb_dropback,
      yardline_100,
      half_seconds_remaining,
      game_seconds,
      qtr,
      down,
      ydstogo,
      xpass,
      wp
      # id_passer
    ) %>%
    drop_na(down, ydstogo)
  
  joined_data <- formatted_pbp %>%
    powerjoin::power_left_join(
      prepped_data_stack,
      by = c(
        'id_game',
        'id_defteam',
        'id_posteam',
        'id_week',
        'id_season'
      ),
      conflict = powerjoin::coalesce_xy
    )
  
  expect_resolved_suffix(joined_data)
  # expect_id_completion(joined_data, id_passer)
  
  return(joined_data)
  
  
}

filter_team_data_stack <- function(filtered_data_stack, posteam) {
  filtered_team_data_stack <- vctrs::vec_slice(filtered_data_stack,
                                               filtered_data_stack$id_posteam == posteam)
  
  assert("Filtered team data stack returned 2 rows." = nrow(filtered_team_data_stack) ==
           1)
  return(filtered_team_data_stack)
}
