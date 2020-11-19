## ---------------------------
## Purpose of script:
## Author: Ramzy Al-Amine
## Date Created: 2020-11-03
## Copyright (c) Ramzy Al-Amine, 2020
## ---------------------------

library(tidyverse)

# load data
features <- load(file = "data/features.rda")
game_logs <- load(file = "data/nba_game_logs.rda")
game_logs

features
### create modeling df
library(skimr)
skim(games)
games_split <- games %>% 
  select(isWin, locationGame, idGame, slugTeam, isB2BSecond, countDaysRestTeam, plusminusTeam) %>% 
  mutate_if(is.character, factor) %>% # for R to read categorical data
  mutate_if(is.logical, as.integer) %>% 
  mutate(idGame = as.character(idGame)) %>% 
  # creation of lags needs to be here
  # group_by(slugTeam) %>% 
  # arrange(slugTeam, idGame) %>% 
  split(games$locationGame)
 
games_df_h <- games_split$H %>% 
  select(-locationGame)

games_df_a <- games_split$A %>% 
  select(-c(isWin, locationGame)) %>% 
  setNames(paste0('opp_', names(.))) %>% 
  rename(idGame = 1)

games_df <- games_df_h %>% 
  left_join(games_df_a, by = "idGame") %>% 
  select(-idGame) %>% 
  mutate(!!!lags(isWin, 1))
  

games_df 

### modeling recipe
library(tidymodels)
set.seed(123)
games_split <- initial_split(games_df)
games_train <- training(games_split)
games_test <- testing(games_split)

games_rec <- recipe(isWin ~ ., data = games_train) %>%
  # step_lag(plusminusTeam, lag = 1:5) %>% 
  step_naomit() %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% #one-hot encoding
  step_zv(all_numeric()) %>% #take out anything with zero variance
  step_normalize(all_numeric()) %>% 
  prep()

games_rec

  
  