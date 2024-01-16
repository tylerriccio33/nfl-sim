calculate_total_roster_relevance <- function(cur_dc, ordered_dc) {
  
  # Why not current play roster and current game? :
  # the current play rosters will reward light personnel
  # importance varies by play (lineman on 3rd&1 and receivers on 3rd&15)
  
  # Plan:
  # use the ordered dc to measure similarity between games
  # use some positional importance + rank here
  # this will smooth over some receiver bias on a per play basis
  
  # Why QB is removed from roster relevance
  # QB is so determinant of outcomes, they should be filtered directly
  # the situation sampler will explicitly look for the QB plays
  # if the QB is 100% new, we'll rely on roster similarity
  # if the QB is not new, roster similarity will only help guide the sampler
  
  ordered_dc %>%
    left_join(select(cur_dc,
                     cur_pos = pos_abb,
                     cur_rank = pos_rank,
                     id_gsis),
              by = 'id_gsis') %>%
    drop_na(cur_pos) %>%
    # rank positional importance
    filter(cur_pos != 'QB') %>%
    mutate(pos_importance = case_match(cur_pos,
                                       "QB" ~ 5,
                                       "WR" ~ 4,
                                       "RB" ~ 3,
                                       "TE" ~ 2,
                                       .default = 1)) %>%
    # multiply by existing rank
    mutate(importance = cur_rank * pos_importance) %>%
    # sum each play total importance
    # the higher the number, the more common important players
    group_by(id_game, id_posteam) %>%
    summarize(roster_relevance = sum(importance), .groups = 'drop')
  
}




