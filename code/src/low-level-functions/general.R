swap_columns <- function(data, col1, col2) {
  temp1 <- pull(data, {{col1}})
  temp2 <- pull(data, {{col2}})
  data <- data %>%
    mutate({{col1}} := temp2,
           {{col2}} := temp1)
  return(data)
}

fswap_columns <- function(data, col1, col2) {
  temp1 <- data[[col1]]
  temp2 <- data[[col2]]
  data[[col1]] <- temp2
  data[[col2]] <- temp1
  return(data)
}

validate_play <- function(play) {
  assert("Yardline is invalid" = play$yardline_100 >= 0 & play$yardline_100 <= 100,
         "Ydstogo is invalid" = play$ydstogo > 0,
         "Down is invalid" = play$down %in% c(1:4))
}


validate_core_play_equality <- function(play1, play2) {
  
  # validate core features of plays are equal
  yardline <- vec_equal(play1$yardline_100, play2$yardline_100)
  down <- vec_equal(play1$down, play2$down)
  ydstogo <- vec_equal(play1$ydstogo, play2$ydstogo)
  
  assert("Core play fields did not match" = all(yardline, down, ydstogo))
  
}


fpowerjoin <- function(x, y, on, ..., suffix = c('.x', '.y')) {
  # ... -> args passed to collapse::join
  
  if (is_named(on)) {
    cli_abort("`fpowerjoin` doesn't support named `on` arg.")
  }

  joined <- collapse::join(x = x, y = y, on, ..., suffix = suffix, verbose = 0)
  
  # loop through cols to coalesce
  common_cols <- intersect(colnames(x), colnames(y))
  common_cols <- purrr::discard(common_cols, ~ .x %in% on)
  
  if (length(common_cols) > 0) {
    for (col in common_cols) {
      suffixed_x <- paste0(col, '.x')
      suffixed_y <- paste0(col, '.y')
      vec <- coalesce(joined[[suffixed_x]], joined[[suffixed_y]])
      vec <- list(vec)
      names(vec) <- col
      # vec <- data.table::fcoalesce(joined[[suffixed_x]], joined[[suffixed_y]])
      get_vars(joined, vars = c(suffixed_x, suffixed_y)) <- NULL
      # joined[[col]] <- vec
      add_vars(joined) <- vec
    }
    
  }

  return(joined)
  
}





