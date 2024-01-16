

SelectSample <- function(off_samples, def_samples) {
  # take many candidates and return a single winner
  
  assert_cols(off_samples, weighted_time_distance, wpa)
  assert_cols(def_samples, weighted_time_distance, wpa)
  
  # if there's 1 sample return it
  if (fnrow(off_samples) == 1) {
    return(off_samples)
  } else if (fnrow(off_samples) <= 5) {
    # if there's too few samples to compute means/scales
    # return the last (usually most recent)
    selected <- ss(off_samples, i = fnrow(off_samples))
    return(selected)
  }
  
  # gather weighted mean of def wpa
  mean_def_yards_gained <- fmean(x = def_samples$yards_gained,
                                 w = def_samples$weighted_time_distance)
  
  # find closest offensive wpa
  off_dif <- abs(off_samples$yards_gained - mean_def_yards_gained)
  off_dif_scaled <- 1 - f_rescale(off_dif)
  
  # no need to rescale this, since they're 2 scaled units
  # the results are properly scaled to probabilities in `sample`
  composite_probs <- off_dif_scaled * off_samples$weighted_time_distance
  
  # sample with probs
  i <- sample(seq_along(composite_probs), size = 1, prob = composite_probs)

  # subset index of sample
  selected <- ss(off_samples, i = i)

  return(selected)
  
}














