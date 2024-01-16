

add_wp <- function(play) {

  # rename already existing
  play <- add_vars(play, posteam = play$id_posteam)
  play <- add_vars(play, home_team = play$id_home_team)
  
  # with_wp <- play %>%
  #   nflfastR::calculate_win_probability()

  wp <- fcalculate_win_probability(play)
  
  play <- add_vars(play, wp = wp)
  play <- add_vars(play, vegas_wp = wp)
  
  return(play)
  
}

fcalculate_win_probability <- function(pbp_data) {
  
  # returns wp as int

  get_vars(pbp_data, vars = c("wp", "vegas_wp")) <- NULL
  
  model_data <- pbp_data
  
  model_data <-
    add_vars(model_data,
             home = ifelse(model_data$posteam == model_data$home_team, 1, 0))
  model_data <-
    add_vars(
      model_data,
      posteam_spread = ifelse(
        model_data$home == 1,
        model_data$spread_line,
        -1 * model_data$spread_line
      )
    )
  model_data <-
    add_vars(model_data,
             elapsed_share = (3600 - model_data$game_seconds_remaining) / 3600)
  model_data <-
    add_vars(model_data,
             spread_time = model_data$posteam_spread * exp(-4 * model_data$elapsed_share))
  model_data <-
    add_vars(model_data, Diff_Time_Ratio =  model_data$score_differential / (exp(-4 * model_data$elapsed_share)))
  
  wp <- fget_preds_wp_partial(model_data) 
  
  return(wp)
  
}

fwp_spread_model_select <- function(pbp) {
  
  #get the columns needed for wp predictions
  #making sure they're in the right order
  
  # pbp <- pbp %>%
  #   fselect(
  #     "receive_2h_ko",
  #     "spread_time",
  #     "home",
  #     "half_seconds_remaining",
  #     "game_seconds_remaining",
  #     "Diff_Time_Ratio",
  #     "score_differential",
  #     "down",
  #     "ydstogo",
  #     "yardline_100",
  #     "posteam_timeouts_remaining",
  #     "defteam_timeouts_remaining"
  #   )
  pbp <- pbp %>%
    fselect(
      receive_2h_ko,
      home,
      half_seconds_remaining,
      game_seconds_remaining,
      Diff_Time_Ratio,
      score_differential,
      down,
      ydstogo,
      yardline_100,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining
    )
  
  return(pbp)
  
}

fget_preds_wp <- function(model_data, model) {
  
  model_data <- fwp_spread_model_select(model_data)
  model_data <- as.matrix(model_data)
  
  pred <- predict(object = model, model_data)
  
  return(pred)
  
}

wp_model <- fastrmodels::wp_model
butched_wp_model <- butcher::butcher(wp_model)

fget_preds_wp_partial <- partial(fget_preds_wp, model = butched_wp_model)


