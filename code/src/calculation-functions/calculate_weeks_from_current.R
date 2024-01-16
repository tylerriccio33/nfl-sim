calculate_weeks_from_first_group_row <- function(game_team_data) {
  
  game_team_data %>%
    group_by(id_posteam) %>%
    arrange(-id_season, -id_week, .by_group = T) %>%
    mutate(weeks_from_current = row_number() - 1) %>%
    ungroup()
  
}
