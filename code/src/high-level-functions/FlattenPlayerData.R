FlattenPlayerData <- function(rolled_player_data) {
  
  # drop players without known rank and XWAR
  data <- drop_na(rolled_player_data, rank) %>%
    replace_na(list(xwar = 0)) %>%     # new players have no xwar
    relocate(rank) %>%
    select(-c(game_type, full_name, position, id_gsis))
  
  nested <- data %>%
    group_by(id_position) %>%
    nest()
  
  clean <- nested %>%
    # keep only position specific statistics
    # slice ranks again for unusual circumstances
    # filter ranks again for unusual circumstances
    mutate(data = map2(data, id_position, ~ {
      if (.y == 'QB') {
        select(.x, -c(racr, target_share, yacoe, ryoe)) %>%
          group_by(id_game, id_season, id_week, id_posteam, rank) %>%
          slice(1) %>%
          ungroup() %>%
          group_by(id_game, id_season, id_week , id_posteam) %>%
          slice_max(order_by = rank, n = 1) %>%
          mutate(rank = 1) %>%
          ungroup()
      } else if (.y == 'RB') {
        select(.x,
               -c(
                 dakota,
                 cpoe,
                 interceptions,
                 sacks,
                 racr,
                 yacoe,
                 target_share
               )) %>%
          group_by(id_game, id_season, id_week, id_posteam, rank) %>%
          slice(1) %>%
          ungroup() %>%
          group_by(id_game, id_season, id_week , id_posteam) %>%
          slice_max(order_by = rank, n = 2) %>%
          mutate(rank = row_number()) %>%
          ungroup()
      } else if (.y == 'TE') {
        select(.x, -c(dakota, cpoe, interceptions, sacks, ryoe)) %>%
          group_by(id_game, id_season, id_week, id_posteam, rank) %>%
          slice(1) %>%
          ungroup() %>%
          group_by(id_game, id_season, id_week , id_posteam) %>%
          slice_max(order_by = rank, n = 1) %>%
          mutate(rank = 1) %>%
          ungroup()
      } else if (.y == 'WR') {
        select(.x, -c(dakota, cpoe, interceptions, sacks, ryoe)) %>%
          group_by(id_game, id_season, id_week, id_posteam, rank) %>%
          slice(1) %>%
          ungroup() %>%
          group_by(id_game, id_season, id_week , id_posteam) %>%
          slice_max(order_by = rank, n = 3) %>%
          mutate(rank = row_number()) %>%
          ungroup()
      }
    }))
  
  flattened <- clean %>%
    # flatten each position by pivoting wider using the rank
    mutate(data = map(data, ~ {
      pivot_wider(
        .x,
        names_from = c(rank),
        values_from = -c(rank, id_game, id_season, id_week, id_posteam)
      )
    })) %>%
    ungroup() %>%
    # rename with position prefix
    mutate(data = map2(data, id_position, ~ {
      rename_with(.x,
                  .fn = \(nms) glue("{.y}_{nms}"),
                  .cols = -starts_with('id_'))
    })) %>%
    ungroup() %>%
    pull(data) 
  
  # expect_id_completion(flattened)
  
  return(flattened)
  
}
