 
## PBP Data ##
raw_data <- get_data(seasons = SAFE_SEASONS) %>%
  as_tibble() %>%
  fsubset(play_type != 'no_play')
data <- raw_data %>%
  apply_name_conventions() %>%
  fix_team_names() %>%
  add_time_variables() %>%
  # rare an unusual problem w/the api
  fsubset(total_play_time > 0) %>%
  # create binaries
  mutate(field_goal_blocked = if_else(field_goal_result == "blocked", 1, 0),
         field_goal_missed = if_else(field_goal_result == "missed", 1, 0),
         field_goal_made = if_else(field_goal_result == "made", 1, 0),
         across(c(field_goal_blocked, field_goal_missed, field_goal_made),
                ~ if_else(is.na(.x), 0, .x)))
assert("Some negative values in total time added" = !any(data$total_play_time < 0, na.rm = T))

## Schedule and Matchup Data ##
game_team_data <- get_game_team_data(seasons = SEASONS) %>%
  apply_name_conventions() %>%
  fix_team_names(team) %>%
  rename(id_posteam = team) %>%
  filter(!(id_season == max(SEASONS) & id_week > CURRENT_WEEK)) %>%
  group_by(id_game) %>%
  arrange(id_game) %>%
  mutate(id_defteam = case_when(
    id_posteam == unique(id_posteam)[1] ~ unique(id_posteam)[2],
    id_posteam == unique(id_posteam)[2] ~ unique(id_posteam)[1]
  )) %>%
  ungroup() %>%
  calculate_weeks_from_first_group_row()
games <- nflreadr::load_schedules(SEASONS)
espn_team_name_tibble <- espnscrapeR::get_nfl_teams()
all_players <- nflreadr::load_players() %>%
  filter(status != 'RET') %>%
  select(display_name, id_gsis = gsis_id, id_posteam = team_abbr, id_position = position) %>%
  mutate(display_name = nflreadr::clean_player_names(display_name))
# get expected roster for future games
# NOTES on posteam consistency:
# if a player comes to a new team, the nfl data won't track this
# ESPN however will track the new team with updated depth charts
future_games <- game_team_data %>%
  filter(is.na(score),
         id_week == CURRENT_WEEK,
         id_season == max(SEASONS)) %>%
  mutate(x_dc = map2(
    id_posteam,
    id_season,
    ~ collect_espn_dc(season = .y, posteam = .x, espn_team_name_tibble)
  ,.progress = T)) %>%
  mutate(x_dc = pmap(
    list(id_posteam, id_season, x_dc),
    ~ join_espn_team_dc(
      posteam = ..1,
      season = ..2,
      cur_espn_dc = ..3,
      all_players = all_players
    ),.progress = T
  ))
assert("No future games" = nrow(future_games) != 0)
CUR_GAMEDAY <- slice_min(future_games, gameday, n = 1, with_ties = F)$gameday

## DC Data ##
raw_depth_charts <- collect_depth_charts(SAFE_SEASONS)
raw_participation_data <-
  collect_snap_pct(SAFE_SEASONS, summarize = F)
expect_id_completion(raw_participation_data)
summarized_participation_data <- collect_snap_pct(SAFE_SEASONS, summarize = T)
expect_id_completion(summarized_participation_data)

ordered_dc <-
  reorder_dc_ranks(raw_depth_charts, participation_data = summarized_participation_data) %>%
  # bind future game dc to past ordered
  bind_rows(
    future_games %>%
      select(id_season, id_week, id_posteam_game = id_posteam, id_game, x_dc) %>%
      unnest(x_dc) %>%
      relocate(starts_with('id_')) %>%
      select(-c(athlete_id, id_posteam)) %>%
      rename(id_posteam = id_posteam_game) %>%
      rename(
        position = pos_abb,
        rank = pos_rank,
        full_name = player_full_name
      )
  )
assert_team_representation(ordered_dc)
assert_completion(ordered_dc, rank)
elo_data <- nflModeler::get_elo(.season = SEASONS) %>%
  select(id_game, id_posteam, qb_value, qb_adj, qb) %>%
  left_join(
    nflreadr::load_players() %>%
      filter(position == 'QB') %>%
      select(qb = display_name, id_passer = gsis_id) %>%
      unique()
  ) %>%
  select(-qb)

## Sample Data ##
distance_lookup <- calculate_time_distance(cur_gameday = CUR_GAMEDAY, 
                        game_team_data = game_team_data)
sample_data <- data %>%
  drop_na(down) %>%
  filter(play_type %in% c('run', 'pass', 'punt', 'field_goal')) %>%
  # filter weird situations (for now)
  filter(!(interception == 1 & fumble == 1),
         safety == 0) %>%
  # append dc rank and position
  append_dc_ranks(ordered_dc) %>%
  left_join(distance_lookup,
            relationship = 'many-to-one')
expect_resolved_suffix(sample_data)
assert_no_duplicates(sample_data, id_play, id_game)
assert_completion(sample_data, total_play_time)
clean_sample_data <-
  na_omit(sample_data, cols = .c(down, ydstogo, yardline_100, wp)) %>%
  select(-id_play)

## Matchup Data ##
matchups <- games %>%
  as_tibble() %>%
  apply_name_conventions() %>%
  fix_team_names(id_home_team, id_away_team) %>%
  filter(.data$id_season == max(SEASONS),
         .data$id_week == CURRENT_WEEK) %>%
  select(
    -c(
      away_score,
      home_score,
      result,
      total,
      overtime,
      id_old_game,
      gsis,
      id_nfl_detail,
      pfr,
      pff,
      espn,
      away_qb_name,
      home_qb_name,
      stadium
    )
  ) %>%
  # grab DC from future games
  mutate(
    id_home_team_dc = map(
      id_home_team,
      \(x) get_dc_from_future_games(x, future_games = future_games)
      ,
      .progress = T
    ),
    id_away_team_dc = map(
      id_away_team,
      \(x) get_dc_from_future_games(x, future_games = future_games)
      ,
      .progress = T
    )
  ) %>%
  filter(!(lengths(id_home_team_dc) == 0 |
             is.null(id_home_team_dc))) %>%
  filter(!(lengths(id_away_team_dc) == 0 |
             is.null(id_away_team_dc))) %>%
  # correct any empty or old teams
  reassign_teams(id_home_team, id_home_team_dc) %>%
  reassign_teams(id_away_team, id_away_team_dc) %>%
  # calculate roster relevances
  mutate(
    home_roster_relevances = map(
      id_home_team_dc,
      \(x) calculate_total_roster_relevance(cur_dc = x, ordered_dc)
      ,
      .progress = T
    )
  ) %>%
  mutate(
    away_roster_relevances = map(
      id_away_team_dc,
      \(x) calculate_total_roster_relevance(cur_dc = x, ordered_dc)
      ,
      .progress = T
    )
  ) %>%
  # # calculate distance from game
  # mutate(time_distance = map(
  #   gameday,
  #   \(x) calculate_time_distance(cur_gameday = x, game_team_data)
  # )) %>%
  # nest all of this into matchup data
  group_by(matchup = row_number()) %>%
  nest(.key = 'matchup_data') %>%
  ungroup()
  

# matchups$matchup_data[[1]] %>%
#   colnames()
  
  
  
  
  
  
