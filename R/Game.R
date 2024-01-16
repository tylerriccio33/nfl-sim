
Game <-
  function(matchup,
           max_plays = 150,
           quiet = F) {
    assert(
      "Non-tibble passed into matchup" = is_tibble(matchup),
      "Matchup should be a single game matchup." = nrow(matchup) == 1
    )
    
    # pull out some players
    home_dc <- matchup$home_dc[[1]]
    away_dc <- matchup$away_dc[[1]]
    home_qb_list <- matchup$home_qb_list[[1]]
    home_qb_gsis <- matchup$home_qb_gsis[[1]]
    away_qb_list <- matchup$away_qb_list[[1]]
    away_qb_gsis <- matchup$away_qb_gsis[[1]]
    home_team <- matchup$home_team[[1]]
    away_team <- matchup$away_team[[1]]
    # pre allocation
    home_team_all_samples <- matchup$home_team_all_samples[[1]]
    home_team_pass_samples <- matchup$home_team_pass_samples[[1]]
    home_team_def_pass_samples <- matchup$home_team_def_pass_samples[[1]]
    home_team_rush_samples <- matchup$home_team_rush_samples[[1]]
    home_team_def_rush_samples <- matchup$home_team_def_rush_samples[[1]]
    home_team_specials <- matchup$home_team_specials[[1]]
    # AWAY
    away_team_all_samples <- matchup$away_team_all_samples[[1]]
    away_team_pass_samples <- matchup$away_team_pass_samples[[1]]
    away_team_def_pass_samples <- matchup$away_team_def_pass_samples[[1]]
    away_team_rush_samples <- matchup$away_team_rush_samples[[1]]
    away_team_def_rush_samples <- matchup$away_team_def_rush_samples[[1]]
    away_team_specials <- matchup$away_team_specials[[1]]
    
    
    # Initialize Game #
    next_play <- initialize_game(matchup = matchup$matchup_data[[1]])
    
    next_play <- add_wp(next_play)
    
    # Start Prediction Loop #
    game_plays <- new_tibble(x = list(), nrow = 0)
    n_play <- 1
    
    if (!quiet) {
      cli::cli_h2("Starting Game")
      cli_progress_bar("Playing game", total = max_plays)
    }
    
    cur_team <- home_team
    cur_qb <- home_qb_gsis
    cur_all_samples <- home_team_all_samples
    cur_pass_samples <- home_team_pass_samples
    cur_rush_samples <- home_team_rush_samples
    cur_special_samples <- home_team_specials
    cur_def_pass_samples <- away_team_def_pass_samples
    cur_def_rush_samples <- away_team_def_rush_samples
    
    # benchmark ~ 4.37ms
    while (n_play <= max_plays) {
      if (!quiet) {
        cli_progress_update()
      }
      # Set Current Team #
      if (next_play$id_posteam != cur_team) {
        cur_team <- next_play$id_posteam
        cur_qb <- ifelse(cur_team == home_team, home_qb_gsis, away_qb_gsis)
        cur_all_samples <- ifelse(cur_team == home_team,
                                  list(home_team_all_samples),
                                  list(away_team_all_samples)
        )[[1]]
        cur_pass_samples <- ifelse(cur_team == home_team,
                                   list(home_team_pass_samples),
                                   list(away_team_pass_samples)
        )[[1]]
        cur_rush_samples <- ifelse(cur_team == home_team,
                                   list(home_team_rush_samples),
                                   list(away_team_rush_samples)
        )[[1]]
        cur_special_samples <- ifelse(cur_team == home_team,
                                      list(home_team_specials),
                                      list(away_team_specials)
        )[[1]]
        # go to away def samples
        cur_def_pass_samples <- ifelse(cur_team == home_team,
                                       list(away_team_def_pass_samples),
                                       list(home_team_def_pass_samples)
        )[[1]]
        cur_def_rush_samples <- ifelse(cur_team == home_team,
                                       list(away_team_def_rush_samples),
                                       list(home_team_def_rush_samples)
        )[[1]]
      }
      
      # Make Fourth Down Decision #
      if (next_play$down == 4) {
        fourth_down_situational_samples <-
          Situation(
            samples = cur_all_samples,
            next_play)
        play_decision <-
          simulate_fourth_down_decision(fourth_down_situational_samples)
      } else {
        play_decision <- "go"
      }
      
      # Sample dropback #
      if (play_decision == "go") {
        dropback_decision_samples <- Situation(
          samples = cur_all_samples, next_play)
        dropback_decision <- simulate_dropback_decision(dropback_decision_samples)
      }
      
      # Select Sample #
      if (play_decision == 'go') {
        if (dropback_decision) {
          pass_samples <- Situation(cur_pass_samples, next_play)
          def_pass_samples <- Situation(cur_def_pass_samples, next_play)
          selected_sample <- SelectSample(off_samples = pass_samples,
                                          def_samples = def_pass_samples)
        } else {
          rush_samples <- Situation(cur_rush_samples, next_play)
          def_rush_samples <- Situation(cur_def_rush_samples, next_play)
          selected_sample <- SelectSample(off_samples = rush_samples,
                                          def_samples = def_rush_samples)
        }
        if (next_play$down == 4) {
          selected_sample <- fix_turnover_on_downs(selected_sample)
        }
      } else {
        samples <- Situation(cur_special_samples, next_play)
        selected_sample <- select_special_sample(samples, decision = play_decision)
      }
      # fix issues that come with variable yardline
      selected_sample <- fix_touchdown(selected_sample)
      
      # Merge current game context with sample
      play <-
        decode_sample(sample = selected_sample, next_play)
      validate_core_play_equality(play, next_play)
      
      # Log Play #
      game_plays <- vec_rbind(game_plays, play)
      
      # End game #
      if (play$game_seconds_remaining <= 0) break
      
      # Describe Possession Changes #
      next_play <- Play(play)
      next_play <- calculate_next_play_values(next_play)
      n_play <- n_play + 1
      
    }
    
    if (!quiet) {
      cli_progress_done()
      cli_alert_success("End game.")
    }
    
    clean_game <- game_plays
    get_vars(clean_game,
             vars = .c(id_rusher_player, id_receiver_player, id_passer)) <- NULL
    # remove IDs of rusher and receiver
    # passer is kept since
    # fselect(-c(id_rusher_player, id_receiver_player, id_passer)) %>%
    clean_game <- clean_game %>%
      # append rusher
      append_player_id(dc = home_dc, pos = "rusher") %>%
      append_player_id(dc = away_dc, pos = "rusher") %>%
      # append receiver
      append_player_id(dc = home_dc, pos = "receiver") %>%
      append_player_id(dc = away_dc, pos = "receiver")
    
    clean_game <- clean_game %>%
      # replace QB
      fmutate(id_passer = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1) ~ home_qb_list$gsis,
        (id_posteam == id_away_team &
           qb_dropback == 1) ~ away_qb_list$gsis,
        .default = NA
      )) %>%
      fmutate(id_passer_name = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1) ~ home_qb_list$name,
        (id_posteam == id_away_team &
           qb_dropback == 1) ~ away_qb_list$name,
        .default = NA
      )) %>%
      # rushers for scrambles
      fmutate(id_rusher = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1 & qb_scramble == 1) ~ home_qb_list$gsis,
        (id_posteam == id_away_team &
           qb_dropback == 1 & qb_scramble == 1) ~ away_qb_list$gsis,
        .default = rusher_name
      )) %>%
      fmutate(rusher_name = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1 & qb_scramble == 1) ~ home_qb_list$name,
        (id_posteam == id_away_team &
           qb_dropback == 1 & qb_scramble == 1) ~ away_qb_list$name,
        .default = rusher_name
      ))
    
    clean_game$id_play_new <- seq_row(clean_game)
    
    
    return(clean_game)
    
  }


# 
# bench::mark(Game(dt, max_plays = 1, quiet = T),
#             iterations = 10)


# t <- Game(dt, max_plays = 150, quiet = F)



# dt <- group_by(filtered_matchups, matchup) %>%
#   nest(.key = "matchup_profile") %>%
#   ungroup() %>%
#   pull(matchup_profile) %>%
#   .[[1]]
# 
# 
# 
# dt %>%
#   Game(max_plays = 150, quiet = F) %>%
#   select(id_posteam, yardline_100, down, ydstogo, yards_gained, desc) %>%
#   view()




