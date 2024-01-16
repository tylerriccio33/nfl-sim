calculate_game_desirability <- function(matchup_data, 
                                        home_safety_qb_samples,
                                        away_safety_qb_samples) {
  # find home team and away team desirability
  
  assert_cols(matchup_data, 
              home_roster_relevances,
              away_roster_relevances,
              # id_home_team, 
              # id_away_team,
              time_distance)
  
  time_distance <- matchup_data$time_distance[[1]]
  home_team <- matchup_data$id_home_team[[1]]
  away_team <- matchup_data$id_away_team[[1]]
  
  # home teams
  home_master_table <- list(
    mutate(time_distance, id_posteam = home_team),
    matchup_data$home_roster_relevances[[1]]
  )
  
  # away teams
  away_master_table <- list(
    mutate(time_distance, id_posteam = away_team),
    matchup_data$home_roster_relevances[[1]]
  ) 
  
  tables <- list(home_master_table,
                 away_master_table) %>%
    set_names(c('home','away')) %>%
    map( ~ {
      .x %>%
        reduce(full_join, by = c('id_game', 'id_posteam')) %>%
        # fill in time distance for games
        group_by(id_game) %>%
        fill(time_distance, .direction = "downup") %>%
        ungroup() %>%
        # fill in relevance as 0 if missing
        replace_na(list(roster_relevance = 0)) %>%
        # some duplicate rows post filling in
        unique() 
      
    })
  
  tables$home <- tables$home[[1]] %>%
    mutate(roster_relevance = case_when(id_game ))
  
  
  return(tables$home)
  
  return(tables)
  
  
  # calculate desirability
  mutate(d_time = desirability2::d_min(time_distance, 
                                       low = min(time_distance),
                                       high = max(time_distance)),
         d_roster = desirability2::d_max(roster_relevance,
                                         low = min(roster_relevance),
                                         high = max(roster_relevance)),
         d_both = desirability2::d_overall(d_time, d_roster))
  
  
  
}

# 
# 
# filtered_matchups %>%
#   slice(1) %>%
#   mutate(t = pmap(list(matchup_data, 
#                        home_safety_qb_samples,
#                        away_safety_qb_samples), 
#                   ~ calculate_game_desirability(..1, ..2, ..3))) %>%
#   pull(t) %>%
#   .[[1]]
# .[[1]]
# 
# 
# filtered_matchups %>%
#   slice(1) %>%
#   select(home_safety_qb_samples) %>%
#   pull() %>%
#   .[[1]] %>% 
#   view()