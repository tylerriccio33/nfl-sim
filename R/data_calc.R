calculate_weeks_from_first_group_row <- function(game_team_data) {

  game_team_data %>%
    group_by(id_posteam) %>%
    arrange(-id_season, -id_week, .by_group = T) %>%
    mutate(weeks_from_current = row_number() - 1) %>%
    ungroup()

}


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


calculate_matchup_samples <- function(matchups) {
  matchups %>%
    # calculate dc based elements
    mutate(home_dc = map(matchup_data, \(x) collect_dc(x, id_home_team_dc)),
           away_dc = map(matchup_data, \(x) collect_dc(x, id_away_team_dc)),
           home_qb_list = map(home_dc, \(x) collect_qb(x)),
           away_qb_list = map(away_dc, \(x) collect_qb(x)),
           home_qb_gsis = map_chr(home_qb_list, \(x) ifelse(is.null(x$gsis), na_chr, x$gsis)),
           away_qb_gsis = map_chr(away_qb_list, \(x) ifelse(is.null(x$gsis), na_chr, x$gsis)),
           home_team = map_chr(matchup_data, \(x) x$id_home_team),
           away_team = map_chr(matchup_data, \(x) x$id_away_team),
           home_roster_relevance = map(matchup_data,\(x)  collect_roster_relevance_from_dc(x, home_roster_relevances)),
           away_roster_relevance = map(matchup_data,\(x)  collect_roster_relevance_from_dc(x, away_roster_relevances))) %>%
    # pre allocating data
    mutate(
      home_team_all_samples = map(home_team, \(x) slice_posteam_samples(clean_sample_data, x)),
      home_team_all_def_samples = map(home_team, \(x) slice_defteam_samples(clean_sample_data, x)),
      home_team_pass_samples = map2(home_qb_gsis, home_roster_relevance, ~ {
        pre_allocate_pass_samples(clean_sample_data, .x) %>%
          append_roster_relevance(.y)
      }),
      home_team_def_pass_samples = map(home_team, \(x) pre_allocate_team_pass_samples(clean_sample_data, x)),
      home_team_rush_samples = map2(home_team_all_samples, home_roster_relevance, ~ {
        slice_no_dropback(.x) %>%
          append_roster_relevance(.y)
      }),
      home_team_def_rush_samples = map(home_team_all_def_samples, \(x) slice_no_dropback(x)),
      home_team_specials = map(home_team_all_samples, \(x) slice_specials(x)),
      # AWAY
      away_team_all_samples = map(away_team, \(x) slice_posteam_samples(clean_sample_data, x)),
      away_team_all_def_samples = map(away_team, \(x) slice_defteam_samples(clean_sample_data, x)),
      away_team_pass_samples = map2(away_qb_gsis, away_roster_relevance, ~ {
        pre_allocate_pass_samples(clean_sample_data, .x) %>%
          append_roster_relevance(.y)
      }),
      away_team_def_pass_samples = map(away_team, \(x) pre_allocate_team_pass_samples(clean_sample_data, x)),
      away_team_rush_samples = map2(away_team_all_samples, away_roster_relevance, ~ {
        slice_no_dropback(.x) %>%
          append_roster_relevance(.y)
      }),
      away_team_def_rush_samples = map(away_team_all_def_samples, \(x) slice_no_dropback(x)),
      away_team_specials = map(away_team_all_samples, \(x) slice_specials(x))
    ) %>%
    # adding qb samples
    mutate(home_safety_qb_samples = map(matchup_data, \(x) collect_safety_qb_samples(sample_data = sample_data,
                                                                                     elo_data = elo_data,
                                                                                     matchup_data = x,
                                                                                     qb_col = 'id_home_qb'))) %>%
    mutate(away_safety_qb_samples = map(matchup_data, \(x) collect_safety_qb_samples(sample_data = sample_data,
                                                                                     elo_data = elo_data,
                                                                                     matchup_data = x,
                                                                                     qb_col = 'id_away_qb'))) %>%
    mutate(home_team_pass_samples = map2(home_team_pass_samples, home_safety_qb_samples, \(x, y) bind_rows(x, y)),
           away_team_pass_samples = map2(away_team_pass_samples, away_safety_qb_samples, \(x, y) bind_rows(x, y)))

}


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
