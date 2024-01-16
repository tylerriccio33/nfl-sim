

sample_o_personnel <- function(all_pbp_data=NULL,
                               all_participation_data=NULL,
                               cur_pbp_data=NULL,
                               static_data=NULL,
                               state) {
  
  if(state == 'static') {
    
    ## Find Season and Week ##
    ## Assemble PBP Data
    all_pbp_data <- all_pbp_data %>%
      select(id_game = game_id,
             id_season = season,
             id_week = week) %>%
      drop_na() %>%
      distinct()
    
    ## Construct Prior Distributions ##
    static_data <- all_participation_data %>%
      select(
        game_id = nflverse_game_id,
        id_posteam = possession_team,
        formation = offense_formation,
        offense_personnel
      ) %>%
      nflModeler::apply_name_conventions() %>%
      # Fix Names
      fix_team_names() %>%
      left_join(all_pbp_data) %>%
      # Final drop na
      drop_na() %>%
      # Summarize game, personnel, formation average
      group_by(id_posteam, id_season, id_week, id_game, formation, offense_personnel) %>%
      summarize(n = n(), .groups = 'drop') %>%
      # Count formation as percentage of total plays in game
      group_by(id_posteam, id_season, id_week, id_game) %>%
      mutate(n = n / sum(n)) %>%
      # Arrange and count roll last 12 game's average
      group_by(id_posteam, formation) %>%
      arrange(id_season, id_week, .by_group = T) %>%
      mutate(n = slide_dbl(
        n,
        .f = mean,
        .before = 12,
        .after = -1
      )) %>%
      # Pivot longer by game, team
      select(id_posteam, id_game, formation,offense_personnel, n) %>%
      drop_na() %>%
      pivot_wider(names_from = c(formation, offense_personnel),names_sep = "__", values_from = n) %>%
      mutate(across(-starts_with('id_'), ~ if_else(is.na(.x), 0, .x))) %>%
      rename_with(~ glue::glue("prior_{.x}"), -starts_with('id_')) %>%
      ungroup()
    
    return(static_data)
    
  }
  
  if(state == 'serve') {
    
    # Find current ids
    cur_game <- cur_pbp_data$id_game
    cur_posteam <- cur_pbp_data$id_posteam
    
    probs <- static_data %>%
      # Filter static data to current game
      ungroup() %>%
      filter(id_game == cur_game,
             id_posteam == cur_posteam) %>%
      select(starts_with('prior_')) %>%
      # List probabilities
      unlist(.[1, ])
    # Sample from named list
    pred_personnel <- sample(names(probs), size = 1, prob = unlist(probs))
    return(pred_personnel)
    
  }
  
  
}



