
expect_resolved_suffix <- function(data) {
  unresolved <- ungroup(data) %>%
    select(ends_with('.x'), ends_with('.y')) %>%
    colnames()
  if (length(unresolved) != 0) {
    rlang::inform(glue("Unresolved suffix -> {unresolved}"))
    rlang::abort("Aborting current operation.")
  }
}


assert_at_least_complete <- function(data, ...) {
  cols <- enquos(...) %>% map_chr(quo_name) %>% as.character()
  l <- length(cols)
  
  with_counts <-
    transmute(data, complete_count = rowSums(c_across(c(...)), ~ !is.na(.)))
  
  vec <- with_counts$complete_count
  
  indices <- which(vec < length(l))
  
  if (length(indices) != 0) {
    sample_indices <- sample(indices, x = 5)
    sample_indices_string <- str_c(sample_indices, collapse = ',')
    msg <-
      glue("These indices did not have at least {l} values complete -> {sample_indices_string}")
    rlang::abort(msg)
  }
}

expect_id_completion <- function(data, ...) {
  non_complete <- ungroup(data) %>%
    select(starts_with('id_') & where( ~ any(is.na(.x)))) %>%
    select(-c(...)) %>%
    filter(if_any(everything(), ~ is.na(.x))) %>%
    colnames()
  
  if (length(non_complete) != 0) {
    data_string <- deparse(substitute(data))
    rlang::abort(glue("Mising ID values in -> {non_complete} in -> {data_string}"))
  }
}

assert_completion <- function(data, ...) {
  non_complete <- ungroup(data) %>%
    select(c(...) & where( ~ any(is.na(.x)))) %>%
    filter(if_any(everything(), ~ is.na(.x))) %>%
    colnames()
  
  if (length(non_complete) != 0) {
    data_string <- deparse(substitute(data))
    
    rlang::abort(glue("Mising ID values in -> {non_complete} in -> {data_string}"))
  }
}

assert_same_sum <- function(...) {
  data_strings <-  enquos(...) %>% map_chr(quo_name)
  
  tibbles <- list2(...) %>% set_names(data_strings)
  
  row_numbers <- map_dbl(tibbles, \(x) nrow(x))
  row_numbers_vec <- unname(row_numbers)
  
  all_equal <- length(unique(row_numbers_vec)) == 1
  if (!all_equal) {
    print(row_numbers)
    msg <-
      glue("Some tibbles passed to assert_same_sum were different")
    abort(msg)
  }
  
}

assert_cols <- function(data, ...) {
  
  data_cols <- colnames(data)
  
  needed_cols <- enexprs(...) %>% as.character()
  
  correct_cols <- all(needed_cols %in% data_cols)
  
  if (!correct_cols) {
    cli_abort("Cols assertion not passed")
  }
  
}


assert_existance <- function(data, ...) {
  col_strings <- enexprs(...) %>% as.character()
  # TODO: Check if any \ exists because we can't check those
  
  cols <- data %>%
    select(all_of(col_strings)) %>%
    colnames() %>%
    length()
  
  stopifnot(length(col_strings) == cols)
  
  
}

assert_no_duplicates <- function(data, ...) {
  duplicates <- data %>%
    group_by(...) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicates) > 1) {
    data_string <- deparse(substitute(data))
    
    cols <- enquos(...) %>% map_chr(quo_name) %>% as.character()
    cols <- str_c(cols, collapse = ",")
    
    code_string <- glue("{data_string} %>%
    group_by({cols}) %>%
    filter(n() > 1) %>%
    ungroup()")
    
    writeClipboard(code_string)
    
    msg <-
      glue("Duplicates found in -> {data_string}. Code to find it copied to clip.")
    
    rlang::abort(msg)
    
  }
  
}


assert_team_representation <- function(data, warn = F) {
  # assert each game has 2 teams
  
  assert_cols(data, id_game, id_posteam)
  
  game_summary <- data %>%
    select(id_game, id_posteam) %>%
    unique() %>%
    group_by(id_game) %>%
    summarize(n = n(),
              teams = n_distinct(id_posteam)) %>%
    ungroup()
  
  no_two_teams <- game_summary %>%
    filter(n != 2 | teams != 2)
  
  if (nrow(no_two_teams)) {
    data_string <- deparse(substitute(data))
    game_example <- slice_sample(no_two_teams, n = 5) %>% .$id_game
    game_example_string <- str_c(game_example, collapse = ',')
    
    if (!warn) {
      cli_abort(glue("Two teams were not present in some games. Here are examples -> {game_example_string}"))
    } else {
      cli_alert_danger(glue("Two teams were not present in some games. Here are examples -> {game_example_string}"))
    }
  }
  
}


## General Assert ##
assert <- function(...,
                   message = NULL,
                   class = NULL,
                   not = TRUE,
                   call = caller_env()) {
  params <- list2(...)
  
  if (length(params) > 1) {
    for (x in seq(params)) {
      assert(
        params[[x]],
        message = names(params)[[x]],
        class = class,
        call = call
      )
    }
    
    invisible(return(NULL))
  }
  
  condition <- params[[1]]
  
  if (is.logical(condition) &&
      ((!condition && not) | (condition && !not))) {
    if (is_named(params)) {
      message <- message %||% names(params)
    }
    
    abort(message = message,
          class = class,
          call = call)
  }
  
  invisible(return(NULL))
}
