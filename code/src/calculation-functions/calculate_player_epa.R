calculate_player_epa <- function(data, coerce_missing = T) {
  # optional coercion of missing values to 0
  
  # select down
  data <- data %>%
    select(id_game,
           id_posteam,
           id_season,
           id_week,
           down,
           epa,
           id_receiver = id_receiver_player, # this one includes all targets
           id_rusher,
           id_passer) %>%
    drop_na(id_posteam, down, epa)
  
  # aggregate passer EPA
  passer_profile <- data %>%
    drop_na(id_passer) %>%
    group_by(id_game, id_posteam, id_season, id_week, id_passer)  %>%
    summarize(pass_epa = mean(epa)) %>%
    ungroup() %>%
    rename(id_gsis = id_passer)
  
  # create play target profile
  target_data <-
    vctrs::vec_slice(data,!(is.na(data$id_receiver) &
                              is.na(data$id_rusher)))
  target_data$target <- coalesce(target_data$id_receiver, target_data$id_rusher)
  target_data <- select(target_data,-c(id_receiver, id_rusher, id_passer)) %>% drop_na()
  
  # aggregate target EPA
  target_profile <- target_data %>%
    group_by(id_game, id_posteam, id_season, id_week, target) %>%
    summarize(target_epa = mean(epa)) %>%
    ungroup() %>%
    rename(id_gsis = target)
  
  # coerce missing
  if (coerce_missing) {
    target_profile <- target_profile %>%
      mutate(target_epa =  if_else(is.na(target_epa), 0, target_epa))
  }
  
  
  # bind rows
  combined_profiles <- bind_rows(passer_profile, target_profile) %>%
    group_by(id_game, id_gsis) %>%
    fill(pass_epa, target_epa, .direction = 'downup') %>%
    ungroup() %>%
    unique()
  
  return(combined_profiles)
  
}
