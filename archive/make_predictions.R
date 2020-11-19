## ---------------------------
## Purpose of script: make predictions from trained RF model
## Author: Ramzy Al-Amine
## Date Created: 2020-11-14
## Copyright (c) Ramzy Al-Amine, 2020
## ---------------------------

library(tidyverse)
library(nbastatR)
library(tidymodels)

# folder setup
setwd("~/R/nba_forecasting")
data_folder <- "data"
src_folder <- "src"
output_folder <- "output"

# relevant files
model_file <- file.path(src_folder, "PredictionModel.rds")

# load functions 
source(file.path(src_folder, "functions.R"))

# set date for which to predict games
this_day <- Sys.Date() - 365
this_season <- 2020 # indicates 2019-20 season


# ============================ GET NEW DATA ============================== #
game_ids <- game_logs(seasons = this_season, result_types = "team", season_types = "Regular Season") %>%
  filter(dateGame <= this_day, dateGame >= this_day - 45) %>% 
  select(idGame) %>% 
  unique()

# transform a series of Game IDs into modeling dataste
new_games <- prep_features(game_ids, this_season) %>% 
  mutate_if(is.logical, as.factor) %>% 
  mutate_if(is.character, as.factor)

new_games %>% View()

# predict on new data set
predictions <- readRDS(model_file) %>% 
  predict(new_games)

probabilities <- readRDS(model_file) %>% 
  predict(new_games, "prob")

pred_df <- new_games %>% 
  select(idGame, dateGame, isWin, slugTeam, slugOpponent) %>% 
  bind_cols(predictions) %>% 
  bind_cols(probabilities) %>% 
  mutate(correct = case_when(isWin == .pred_class ~ "Correct", TRUE ~ "Incorrect")) 

# analyze overall performance
pred_df %>% 
  count(correct) %>% 
  mutate(pct = n / sum(n))

# analyze performance for close calls
close_calls <- (.25:.75)
pred_df %>% 
  filter(between(.pred_TRUE, 0.25, 0.75)) %>% 
  count(correct) %>% 
  mutate(pct = n / sum(n))

plot_accuracy(pred_df)
# 
# # compare against market
# market_odds <- nbastatR::win_probability(game_ids = pred_df$idGame, nest_data = FALSE, filter_non_plays = TRUE, return_message = TRUE)
# 
# betting_market <- market_odds %>% filter(numberPeriod == 1, timeQuarter == "12:00", is.na(isHomePossession)) %>%  select(idGame, pctWinProbHome)

# show predictions for this day
pred_df %>% 
  filter(dateGame == this_day)

