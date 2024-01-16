collect_safety_qb_samples <- function(sample_data, elo_data, matchup_data, qb_col) {
  
  assert(
    "Argument qb_col should be a character, representing the column in matchups that holds the qb gsis."
  )
  
  cur_qb <- select(matchup_data, all_of(qb_col)) %>% pull()
  assert("Qb does not exist" = length(cur_qb) != 0)
  
  # lookup qb in elo data
  cur_elo <- filter(elo_data,
                    .data$id_passer == cur_qb) %>%
    arrange(desc(id_game)) %>%
    slice(1) %>%
    pull(qb_value)
  
  # find 2 similar qbs
  similar_qbs <- elo_data %>%
    filter(.data$id_passer != cur_qb) %>%
    mutate(elo_dist = abs(cur_elo - .data$qb_value)) %>%
    slice_min(elo_dist, n = 2) %>%
    pull(id_passer)
  # slice qb samples
  clean_samples <- select(sample_data, -id_play)
  sample_group_one <- pre_allocate_pass_samples(clean_samples, similar_qbs[1])
  sample_group_two <- pre_allocate_pass_samples(clean_samples, similar_qbs[2])
  all_new_samples <- bind_rows(sample_group_one, sample_group_two)
  
  return(all_new_samples)
  
}
