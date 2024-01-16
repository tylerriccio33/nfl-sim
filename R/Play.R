
Play <- function(merged_sample) {
  assert("Data passed to Play should be 1 row" = fnrow(merged_sample) == 1)
  
  play <- merged_sample
  
  # catch large events w/first order
  # catch mini events w/second order
  
  # First Order Events:
  # touchdown (any)
  # fumble lost
  # interception
  # field goal made
  # any touchback
  # punt not returned at all
  # punt blocked
  if (play$touchdown == 1) {
    # who scored it ?
    if (play$rush_touchdown | play$pass_touchdown) {
      # offense
      post <- simulate_pos_score(play, 7)
      return(post)
    } else {
      # defense
      post <- simulate_def_score(play)
      return(post)
    }
  } else if (play$fumble_lost == 1) {
    uncaught_touchdown <- (play$yardline_100 - play$return_yards) <= 0
    if (uncaught_touchdown) {
      play$touchdown <- 1
      post <- simulate_def_score(play)
      return(post)
    }
    post <- flip_team(play, yards_added = play$yards_gained)        # fumbles are yards gained
    return(post)
  } else if (play$interception == 1) {
    # need to simulate def score if return yards were greater than yardline
    uncaught_touchdown <- (play$yardline_100 - play$return_yards) <= 0
    if (uncaught_touchdown) {
      play$touchdown <- 1
      post <- simulate_def_score(play)
      return(post)
    }
    post <- flip_team(play, yards_added = play$return_yards)        # interceptions are return yards
    return(post)
  } else if (play$field_goal_made == 1) {
    post <- simulate_pos_score(data = play, score = 3)
    return(post)
  } else if (play$touchback == 1) {
    post <- simulate_touchback(play)
    return(post)
  } else if (play$punt_out_of_bounds == 1 | play$punt_fair_catch == 1) {
    post <- flip_team(play, 
                      yards_added = play$kick_distance, 
                      touchback_on_invalid_yards = T)
    return(post)
  } else if (play$punt_blocked == 1) {
    post <- flip_team(play, yards_added = play$return_yards)
    return(post)
  }
  
  # Second Order Events:
  # fourth down failed
  # punt returned
  # field goal not made
  if (play$fourth_down_failed == 1) {
    post <- flip_team(play, yards_added = play$yards_gained)
    return(post)
  } else if (play$punt_attempt == 1) {
    yards_added <- play$kick_distance - play$return_yards
    post <-
      flip_team(play, yards_added = yards_added,touchback_on_invalid_yards = T)
    return(post)
  } else if (play$field_goal_missed == 1) {
    post <- flip_team(play, yards_added = play$return_yards)
    return(post)
  } else if (play$field_goal_blocked == 1) {
    post <- flip_team(play, yards_added = play$return_yards)
    return(post)
  }
  
  # Normal Events:
  post <- adjust_down_distance(play)
  
  # validate
  validate_play(post)
  
  return(post)
  
}


















