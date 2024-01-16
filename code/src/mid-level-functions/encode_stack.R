encode_prepped_stack <- function(prepped_stack, model, prepper) {
  
  assert("Prepped stack should be prepped as a tibble." = is_tibble(prepped_stack))
  
  juiced <- recipes::bake(prepper, new_data = prepped_stack) %>% as.matrix()
  
  encoded <- predict(object = model, x = juiced) %>% as_tibble(.name_repair = 'minimal')
  encode_names <- map_chr(1:ncol(juiced), \(x) glue("col_{x}"))
  names(encoded) <- encode_names
  
  recoded <- select(prepped_stack, starts_with('id_')) %>%
    bind_cols(encoded)
  
  nested <-  recoded %>%
    group_by(id_game, id_posteam, id_defteam, id_season, id_week) %>%
    nest() %>%
    ungroup() %>%
    mutate(data = map(data, as.matrix))
  
  return(nested)
  
}
