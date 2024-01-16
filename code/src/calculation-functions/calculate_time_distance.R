


calculate_time_distance <- function(cur_gameday, game_team_data) {
  assert_cols(game_team_data, gameday, id_game)
  
  assert(
    "cur_gameday should be character yyyy-mm-dd" = is.character(cur_gameday),
    "game_team_data$gameday should be character yyyy-mm-dd" = is.character(game_team_data$gameday)
  )
  
  game_team_data$gameday <- lubridate::ymd(game_team_data$gameday)
  
  cur_gameday <- ymd(cur_gameday)
  
  distances <- cur_gameday - game_team_data$gameday
  
  lookup <- select(game_team_data, id_game)
  lookup$time_distance <- as.numeric(distances)
  # rescale to higher numbers as closer
  lookup$weighted_time_distance <- scales::rescale(-lookup$time_distance)
  
  if (any(vec_detect_missing(lookup$time_distance))) {
    cli_alert_danger("Some values failed to pass the time distance converter.")
  }
  
  lookup <- unique(lookup)
  
  return(lookup)
  
  
}

