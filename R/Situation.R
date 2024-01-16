

Situation <-
  function(samples,
           cur_play,
           correct_window_vars = T) {
    # find next play candidates
    
    # Variable Windows #
    WP_WINDOW <- .1
    YDSTOGO_WINDOW <- 1
    YARDLINE_100_WINDOW <- 10
    down <- cur_play$down
    d_down <- down
    ydstogo <- cur_play$ydstogo
    wp <- cur_play$wp
    yardline_100 <- cur_play$yardline_100
    
    i <- whichv(samples$down, value = d_down)
    samples <- ss(samples, i)
    
    # Notes:
    # - In order of most to least likely to effect the play
    #   - ydstogo, wp, yardline
    # - If a common sample isn't found, update all of them
    
    loop_iter <- 1
    max_iter <- 20
    while (loop_iter < max_iter) {
      avail_data <- samples
      
      # Yards to Go #
      cur_lower_ydstogo <- ydstogo - YDSTOGO_WINDOW
      cur_upper_ydstogo <- ydstogo + YDSTOGO_WINDOW
      ydstogo_between <-
        between_cpp(avail_data$ydstogo, cur_lower_ydstogo, cur_upper_ydstogo)
      yard_slice <- ss(avail_data, ydstogo_between)
      if (fnrow(yard_slice) == 0) {
        YDSTOGO_WINDOW <- YDSTOGO_WINDOW + 1
        loop_iter <- loop_iter + 1
        next
      }
      
      # WP #
      cur_lower_wp <- wp - WP_WINDOW
      cur_upper_wp <- wp + WP_WINDOW
      wp_between <-
        between_cpp(yard_slice$wp, cur_lower_wp, cur_upper_wp)
      wp_slice <- ss(yard_slice, wp_between)
      if (fnrow(wp_slice) == 0) {
        WP_WINDOW <- WP_WINDOW + .05
        loop_iter <- loop_iter + 1
        next
      }
      
      # Yard-line #
      cur_lower_yardline <- yardline_100 - YARDLINE_100_WINDOW
      cur_upper_yardline <- yardline_100 + YARDLINE_100_WINDOW
      yardline_between <-
        between_cpp(wp_slice$yardline_100,
                    cur_lower_yardline,
                    cur_upper_yardline)
      yardline_slice <- ss(wp_slice, yardline_between)
      if (fnrow(yardline_slice) == 0) {
        YARDLINE_100_WINDOW <- YARDLINE_100_WINDOW + 5
        loop_iter <- loop_iter + 1
        next
      }
      
      # Try to get at least 5 samples #
      if (fnrow(yardline_slice) <= 3 & loop_iter <= max_iter) {
        YDSTOGO_WINDOW <- YDSTOGO_WINDOW + 1
        WP_WINDOW <- WP_WINDOW + .05
        YARDLINE_100_WINDOW <- YARDLINE_100_WINDOW + 5
        loop_iter <- loop_iter + 1
        next
      }
      
      # Exit Loop #
      if (correct_window_vars) {
        yardline_slice$yardline_100 <- yardline_100
        yardline_slice$ydstogo <- ydstogo
        yardline_slice$wp <- wp
      }
      
      return(yardline_slice)
      
    }
    
    cli_abort("Couldn't find similar play, infinity loop triggered.")
    
  }






