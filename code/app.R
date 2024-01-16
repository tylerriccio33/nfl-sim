

#################################################################
##                            Setup                            ##
#################################################################
##---------------------------------------------------------------
##                          Libraries                           -
##---------------------------------------------------------------
library(tidyverse)
library(cli)
library(nflfastR)
library(tidymodels)
library(rlang)
library(glue)
library(nflModeler)
library(vctrs)
library(Rcpp)
library(slider)
library(collapse)
library(tictoc)
conflicted::conflicts_prefer(dplyr::lag)
tidymodels_prefer()
options(error = recover)
options(error = NULL)

##----------------------------------------------------------------
##                            Config                             -
##----------------------------------------------------------------
SEASONS <- 2021:2023
SAFE_SEASONS <- min(SEASONS):2023
CURRENT_WEEK <- 7

##----------------------------------------------------------------
##                            Source                             -
##----------------------------------------------------------------
list.files("src/high-level-functions/", full.names = T) %>% walk(source)
list.files("src/mid-level-functions/", full.names = T) %>% walk(source)
list.files("src/low-level-functions/", pattern = "\\.R$", full.names = T) %>% walk(source)
list.files("src/collect-functions/", full.names = T) %>% walk(source)
list.files("src/calculation-functions/", full.names = T) %>% walk(source)
sourceCpp("src/low-level-functions/copy_cols_cpp.cpp")
sourceCpp("src/low-level-functions/between_cpp.cpp")
source("data/data.R")

# TODO: Safeties
# - Might be best to reduce the yards lost by .2 or something
# - If real yardline is 10 and sim is 2, the playcalling will be very different
# - It should be hard to get a safety
# TODO: Prioritize own qb over safety qbs
# - This can be written into Situation
# - 5 loops for the current qb then another 5 for the safety qbs ?

## Simulate ##
filtered_matchups <- matchups %>%
  mutate(id_game = map_chr(matchup_data, \(x) x$id_game)) %>%
  
  # filter(str_detect(id_game, 'NYJ')) %>%
  
  calculate_matchup_samples()

N_SIMS <- 100
set.seed(1)
tictoc::tic()
results <- group_by(filtered_matchups, matchup) %>%
  nest(.key = "matchup_profile") %>%
  ungroup() %>%
  mutate(res = map(
    matchup_profile,
    \(x) MultipleGames(
      n_simulations = N_SIMS,
      x,
      max_plays = 150,
      quiet = T
    ), .progress = T
  ))
tictoc::toc()

unpacked <- map(results$res, \(x) pull(x, value)) %>%
  list_flatten() %>%
  enframe()

write_rds(unpacked, "unpacked.rds")

beepr::beep(1)

# unpacked %>%
#   filter(!map_lgl(value, is.null)) %>%
#   mutate(value = map(value, \(x) select(x, -any_of(c(
#     'name', 'value'
#   ))))) %>%
#   group_by(name) %>%
#   unnest(value) %>%
#   group_by(name, id_posteam) %>%
#   summarize(
#     score = last(posteam_score),
#     yards = sum(yards_gained),
#     epa = mean(epa),
#     wpa = mean(wpa),
#     pass_tds = sum(touchdown[complete_pass == 1]),
#     pass_yards = sum(passing_yards[complete_pass == 1]),
#     rush_yards = sum(rushing_yards, na.rm = T)
#   ) %>%
#   ungroup() %>%
#   group_by(id_posteam) %>%
#   summarize(across(where(is.numeric), mean))











         