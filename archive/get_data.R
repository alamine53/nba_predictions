library(nbastatR)
library(tidyverse)
library(skimr)
library(zoo) 


# folder setup
setwd("~/R/nba_forecasting")
data_folder <- "data"
src_folder <- "src"
output_folder <- "output"

# ============================= DATA GATHERING  ================================= #

# get game ids from nbastatR package
game_ids <- nbastatr::game_logs(seasons = 2019, league = "NBA", result_types = "team",
          season_types = "Regular Season", nest_data = F,
          assign_to_environment = TRUE, return_message = TRUE) %>%
  select(slugSeason, dateGame, idGame, locationGame, slugTeam, slugOpponent, idTeam, isWin) %>% 
  mutate(isWin = as.factor(isWin)) %>% 
  mutate_if(is.character, as.factor)

# pull advanced stats by game ~~ this takes a while to run
advanced_stats <- game_ids %>% 
  select(idGame) %>% 
  unique() %>% 
  # divide into chunks
  mutate(idGroup= row_number() %/% 40) %>% 
  select(idGroup, idGame) %>% 
  nest(data = c(idGame)) %>% 
  mutate(ratings = map(data, ~box_scores(game_ids = .$idGame,
                                         box_score_types = "Advanced",
                                         result_types = "team", 
                                         join_data = TRUE, assign_to_environment = TRUE))) %>% 
  select(ratings) %>% 
  unnest(ratings) %>% 
  unnest(dataBoxScore)

# ============================= DATA PROCESSING  ================================= #

## get features for modeling
features <- advanced_stats %>% 
  select(-typeResult, -slugTeam, -teamName, -cityTeam) %>% 
  arrange(idTeam, idGame) %>% 
  group_by(idTeam) %>% 
  # compute 3 lags & 2 ma's for every indicator 
  mutate_at(. , vars(-starts_with("id")), 
            funs(l1 = lag(., 1), l2 = lag(., 2), l3 = lag(., 3),
                 m3 = rollmean(.,3, na.pad = TRUE), m5 = rollmean(., 5, na.pad = TRUE))) %>% 
  select(idTeam, idGame, ends_with(c("_l1", "_l2", "_m3", "_m5"))) %>% 
  select(-starts_with("pctUSG_"))

# create final dataset by combining game_ids with features
modeling_df <- game_ids %>% 
  select(idTeam, idGame, slugTeam, slugOpponent, locationGame, isWin) %>% 
  left_join(features, by = c("idTeam", "idGame")) %>% 
  # remove_empty() %>% 
  na.omit()

# convert home/away from long to wide
modeling_df_a <- modeling_df %>% 
  filter(locationGame == "A") %>% 
  select(-c("idTeam", "slugTeam", "locationGame", "isWin", "slugOpponent")) %>% 
  rename_at(. , vars(-starts_with("id")), funs(paste0('opp.', .))) 

modeling_df_h <- modeling_df %>% 
  filter(locationGame == "H") %>% 
  left_join(modeling_df_a, by = "idGame") %>% #need to make sure they are being joined correctly
  select(-locationGame, -idTeam) %>% 
  mutate(isWin = as.factor(isWin)) %>% 
  na.omit()

write_csv(modeling_df_h, "data/modeling_df.csv")

# ============================= END OF DATA PROCESSING  ================================= #
