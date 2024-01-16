

## Simulate Play ##
serve_prediction <-
  function(filtered_data_stack,
           play,
           model = MODEL) {
    
    assert("Filtered data stack should be 1 row." = nrow(filtered_data_stack) == 1,
           "Filtered data stack should be in tibble form."=is_tibble(filtered_data_stack))

    
    # Integrate Play #
    model_data <- integrate_pbp(filtered_data_stack, play)
    
    assert("Multiple rows passed to simulate play."=nrow(model_data) == 1)
    
    predicted_value <- model %>%
      predict(model_data) %>%
      mutate(.pred = reverse_yards(.pred)) %>%
      pull()
    
    # assert(
    #   length(predicted_value) == 1,
    #   "Somehow multiple predicted values were returned.",
    #   return = model_data
    # )

    assert("Multiple rows passed to simulate play."=length(predicted_value)==1)
    return(predicted_value)
    
  }
