PrepDataStack <- function(data_stack){
  
  list2env(data_stack, envir = caller_env())
  
  # preliminary cleaning of qb data
  qb_clean <- qb_data %>%
    group_by(id_game, id_posteam) %>%
    slice_max(order_by = qb_adj, n = 1) %>%
    ungroup() %>%
    select(id_game, id_posteam, qb_value, qb_adj)
  expect_id_completion(qb_clean)
  assert_no_duplicates(qb_clean, id_game, id_posteam)
  assert_team_representation(qb_clean)
  
  # everything starts with the main prepped
  joined_qb <- main_prepped_data %>%
    # join to the pre-rolled ELO data
    left_join(qb_clean , by = c('id_game','id_posteam')) 
  expect_resolved_suffix(joined_qb)
  expect_id_completion(joined_qb)
  assert_no_duplicates(joined_qb, id_game, id_posteam)
  assert_team_representation(joined_qb)
  
  # now we joined to the pre-rolled player data
  joined_elo <- joined_qb %>%
    left_join(flattened_player_data, by = c('id_game','id_season','id_week','id_posteam'))
  expect_resolved_suffix(joined_elo)
  expect_id_completion(joined_elo)
  assert_no_duplicates(joined_elo, id_game, id_posteam)
  assert_team_representation(joined_elo)
  
  ## Final Processing and Appendage ##
  processed <- joined_elo %>%
    mutate(
      era = case_when(
        id_season %in% c(2006:2007) ~ 1,
        id_season %in% c(2008:2013) ~ 2,
        id_season %in% c(2014:2016) ~ 3,
        id_season %in% c(2016:2018) ~ 4,
        TRUE ~ 5
      ),
      era = as.factor(era),
      across(everything(), ~ if_else(is.nan(.x), NA, .x)),
      across(everything(), ~ if_else(is.infinite(.x), NA, .x)),
      across(c(contains('_xwar_')), ~ if_else(is.na(.x), 0, .x))
    )
  expect_resolved_suffix(processed)
  expect_id_completion(processed)
  assert_no_duplicates(processed, id_game, id_posteam)
  assert_team_representation(processed)
  
  assert_same_sum(main_prepped_data, processed)    # should get the same out as you put in
  
  return(processed)
  
}
