
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
