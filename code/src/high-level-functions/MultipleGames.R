
MultipleGames <-
  function(matchup_profile,
           n_simulations = 5,
           ...) {
    
    # create list of matchup profiles
    matchup_profiles <- bind_rows(rep(list(matchup_profile), times = n_simulations)) %>%
      group_by(row_number()) %>%
      nest() %>%
      ungroup() %>%
      pull(data)
    
    # create safe game
    partial_game <- partial(Game, ...)
    safe_game <- safely(partial_game, otherwise = NULL, quiet = F)

    # map over list of profiles
    # future::plan(future::multisession(workers = 3))
    replication_results <- matchup_profiles %>%
      purrr::map(\(x) safe_game(matchup = x))
    # future::plan(future::sequential)

      # pull out results
     tryCatch({
       replication_results <- replication_results %>%
        map(\(x) x$result) %>%
        enframe()
    }, error = function(e) {
      cli_alert_warning(glue("An error occured packing up the results -> {e}"))
    })
    
    # write_rds(replication_results, 'replication_results.rds')
    # cli_alert_success("Results saved as matchup_results.rds")
    
    return(replication_results)
  }


