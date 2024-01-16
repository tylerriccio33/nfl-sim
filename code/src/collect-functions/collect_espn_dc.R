collect_espn_dc <- function(season, posteam, espn_team_name_tibble) {
  # use a posteam to query the most recent dc
  
  # espn/nfl/nflverse team name inconsistency conversion
  if(!posteam %in% espn_team_name_tibble$team_abb) {
    team_name_lookup <- fix_team_names(espn_team_name_tibble, team_abb) %>%
      select(nfl_team = team_abb) %>%
      bind_cols(espn_team_name_tibble)
    posteam <- vctrs::vec_slice(team_name_lookup, team_name_lookup$nfl_team == posteam)
    posteam <- pull(posteam, team_abb)
  }
  
  cur_dc <-
    espnscrapeR::get_depth_chart(season = season, team = posteam) %>%
    # filter down needs
    filter(pos_abb %in% c('WR', 'QB', 'TE', 'RB')) %>%
    filter(
      !(pos_abb == 'QB' &
          pos_rank != 1),
      !(pos_abb == 'TE' & pos_rank > 2),
      !(pos_abb == 'WR' &
          pos_rank > 3),
      !(pos_abb == 'RB' & pos_rank > 2)
    ) %>%
    # collect athlete information
    mutate(player_full_name = map(athlete_id, ~ get_athlete_safely(.x))) %>%
    select(athlete_id, pos_abb, pos_rank, player_full_name) %>%
    # Clean ID tibble
    mutate(player_full_name = nflreadr::clean_player_names(as.character(player_full_name)))
  
  return(cur_dc)
  
}
