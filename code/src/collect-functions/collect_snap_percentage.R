

collect_snap_pct <- function(SAFE_SEASONS, summarize = T) {
  data <-
    nflreadr::load_participation((max(SAFE_SEASONS) - 2):max(SAFE_SEASONS), include_pbp = F) %>%
    as_tibble() %>%
    apply_name_conventions() %>%
    # filter(id_play %in% sample_data$id_play) %>%
    select(id_game = id_nflverse_game,
           id_posteam = possession_team,
           id_play,
           offense_players) %>%
    separate_rows(offense_players, sep = ";") %>%
    rename(id_gsis = offense_players) %>%
    mutate(id_gsis = na_if(id_gsis, '')) %>%
    drop_na()
  if (summarize) {
    data <- data  %>%
      group_by(id_game, id_posteam) %>%
      mutate(plays = n_distinct(id_play)) %>%
      ungroup() %>%
      group_by(id_game, id_posteam, id_gsis) %>%
      summarize(
        snaps = n(),
        snap_pct = snaps / first(plays),
        .groups = 'drop'
      )
  }
  
  return(data)
}
