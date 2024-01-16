get_athlete_safely <- function(athlete_id, return_raw = F) {
  season <- Sys.Date() %>% substr(1, 4)

  base_url <-
    "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/{season}/athletes/{athlete_id}"

  raw_get  <- base_url %>%
    glue::glue() %>%
    httr::GET()

  # httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  athlete_df <- raw_json %>%
    tibble::enframe()

  if (return_raw) {
    return(athlete_df)
  }

  # pull full name out
  athlete_df <- athlete_df %>%
    filter(name == 'fullName') %>%
    pull(value) %>%
    unlist()


  return(athlete_df)

}

join_espn_team_dc <- function(posteam, season, cur_espn_dc, all_players) {

  # filter down raw depth charts to team data
  # there can be multiple name matches (Josh Allen QB + Josh Allen DE)
  # the chances of a same team match, especially within offense is none
  filtered_players <- select(all_players,
                             id_gsis, display_name, id_posteam) %>%
    unique()
  filtered_players$display_name <- as.character(filtered_players$display_name)

  # join on name
  cur_espn_dc$player_full_name <- as.character(cur_espn_dc$player_full_name)
  joined <- cur_espn_dc %>%
    left_join(
      filtered_players,
      by = c('player_full_name' = 'display_name'),
      relationship = 'many-to-many'
    )

  # if multiple name matches occur, keep it to current team
  joined <- joined %>%
    group_by(player_full_name) %>%
    arrange(id_posteam != posteam, id_posteam, .by_group = T) %>%
    slice(1) %>%
    ungroup()

  return(joined)

}

append_dc_ranks <- function(data, depth_charts) {
  # append gsis IDs to depth chart positions #

  # join to id rusher
  joined_rusher <-
    left_join(
      data,
      select(
        depth_charts,
        id_season,
        id_week,
        id_gsis,
        rusher_pos = position,
        rusher_rank = rank
      ),
      by = c('id_rusher' = 'id_gsis', 'id_season', 'id_week'),
      relationship = 'many-to-one',
      na_matches = 'never'
    )

  # join to id passer
  joined_receiver <-
    left_join(
      joined_rusher,
      select(
        depth_charts,
        id_season,
        id_week,
        id_gsis,
        receiver_pos = position,
        receiver_rank = rank
      ),
      by = c('id_receiver_player' = 'id_gsis', 'id_season', 'id_week'),
      relationship = 'many-to-one',
      na_matches = 'never'
    )

  return(joined_receiver)

}

get_last_dc <-
  function(.id_posteam,
           depth_charts) {

    # gets the last available dc associated with the team
    queried_dc <- vctrs::vec_slice(depth_charts, depth_charts$id_posteam == .id_posteam)
    queried_dc <- vctrs::vec_slice(queried_dc, queried_dc$id_season == max(queried_dc$id_season))
    queried_dc <- vctrs::vec_slice(queried_dc, queried_dc$id_week == max(queried_dc$id_week))

    assert("No cols found for most recent dc." = nrow(queried_dc) != 0)
    return(queried_dc)

  }

reorder_dc_ranks <- function(dc, participation_data) {
  # join game participation data to re-order ranks
  # WARNING: this includes posterior participation data
  assert_cols(participation_data, snap_pct)

  dc <- mutate(dc, rank = as.integer(rank))

  # slice top QB
  qb_slices <- vctrs::vec_slice(dc, dc$position == 'QB')
  qb_slices <- qb_slices %>%
    group_by(id_game,id_posteam) %>%
    arrange(id_posteam, rank, .by_group = T) %>%
    slice(1)

  # reorder WR by snap
  wr_reordered <- vctrs::vec_slice(dc, dc$position == 'WR')

  wr_reordered <- wr_reordered %>%
    left_join(participation_data) %>%
    group_by(id_game, id_posteam, position) %>%
    arrange(-snap_pct, .by_group = T) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    select(-c(snaps, snap_pct))
  expect_resolved_suffix(wr_reordered)
  wr_reordered <- vctrs::vec_slice(wr_reordered, wr_reordered$rank <= 3)

  # slice top TE
  te_slices <- vctrs::vec_slice(dc, dc$position == 'TE')
  te_slices <- vctrs::vec_slice(te_slices, te_slices$rank <= 2)

  # slice top RB
  rb_slices <- vctrs::vec_slice(dc, dc$position == 'RB')
  rb_slices <- vctrs::vec_slice(rb_slices, rb_slices$rank <= 2)

  # Bind all
  ordered_dc <- bind_rows(qb_slices, wr_reordered, te_slices, rb_slices) %>%
    arrange(id_season, id_week, id_posteam, position, rank) %>%

    # final rank clean up
    group_by(id_game, id_posteam, position, rank) %>%
    slice(1) %>%
    ungroup()

  # assert
  expect_id_completion(ordered_dc)
  assert_completion(ordered_dc, rank)
  return(ordered_dc)

}


get_dc_from_future_games <- function(id_home_team, future_games) {
  assert_cols(future_games, id_posteam)
  team_dc <- tryCatch({
    filtered <-
      vctrs::vec_slice(future_games, future_games$id_posteam == id_home_team)
    if (nrow(filtered) == 0) {
      cli_abort()
    }
    filtered$x_dc[[1]]
  }, error = function(e) {
    NULL
  })
  if (is.null(team_dc)) {
    msg <- glue("Failed to get dc for {id_home_team}. Returning empty tibble.")
    rlang::warn(msg)
    return(tibble())
  }

  return(team_dc)

}

reassign_teams <- function(games, team_col, dc_col) {
  # new depth charts will have the correct players but NOT the correct teams
  # newly added veterans will have the incorrect posteam
  # newly drafted players will have no GSIS and posteam
  # this will reassign the teams using the matchup unit
  # this will NOT do anything to the GSIS

  games %>%
    mutate({{dc_col}} := map2({{team_col}}, {{dc_col}}, ~ {
      .y$id_posteam <- .x
      return(.y)
    }
    ))

}

collect_dc <- function(matchup, dc_col, clean = T) {
  # collect and clean dc from a matchup

  dc <- pull(matchup, {{dc_col}}) %>% .[[1]]

  if (clean) {
    dc <- dc %>%
      fselect(id_gsis,
              position = pos_abb,
              rank = pos_rank,
              id_posteam,
              name = player_full_name)

  }

  return(dc)

}

collect_roster_relevance_from_dc <- function(matchup, roster_relevance_col) {
  rr <- pull(matchup, {{roster_relevance_col}}) %>% .[[1]]
  return(rr)
}

append_player_id <- function(game_plays, dc, pos) {
  # use rank to replace correct player in depth chart
  # pos must be in rusher, receiver

  if (pos == 'rusher') {
    dc <- frename(
      dc,
      id_gsis = id_rusher_player,
      position = rusher_pos,
      rank = rusher_rank,
      name = rusher_name)
    joined <- fpowerjoin(
      x = game_plays,
      y = dc,
      on = c('id_posteam', 'rusher_pos', 'rusher_rank'),
      how = 'left'
    )
  } else if (pos == 'receiver') {
    dc <- frename(dc,
                  id_gsis = id_receiver_player,
                  position = receiver_pos,
                  rank = receiver_rank,
                  name = receiver_name)
    joined <- fpowerjoin(
      game_plays,
      dc,
      on = c('id_posteam','receiver_pos','receiver_rank'),
      how = 'left'
    )
  }  else {
    abort("Pos must be in rusher, receiver, passer.")
  }


  return(joined)

}



collect_qb <- function(dc) {
  qb_dc <- vctrs::vec_slice(dc, dc$position == 'QB')

  gsis <- qb_dc$id_gsis

  name <- qb_dc$name

  return(list(gsis = gsis, name = name))

}

