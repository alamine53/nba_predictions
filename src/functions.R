## ---------------------------
## Purpose of script: functions
## Author: Ramzy Al-Amine
## Date Created: 2020-11-15
## Copyright (c) Ramzy Al-Amine, 2020
## ---------------------------

library(tidyverse)
library(nbastatR)
library(zoo)

# function to create a features dataset from a list of game IDs
prep_features <- function(game_ids, season){
  
  # pull stats in chunks
  advanced_stats <- game_ids %>% 
    
    # divide into chunks
    mutate(idGroup= row_number() %/% 40) %>% 
    select(idGroup, idGame) %>% 
    nest(data = c(idGame)) %>% 
    mutate(ratings = map(data, ~box_scores(game_ids = .$idGame,
                                           box_score_types = "Advanced",
                                           result_types = "team", 
                                           join_data = TRUE, assign_to_environment = TRUE))) %>% 
    select(ratings) %>% unnest(ratings) %>% unnest(dataBoxScore)
  
  ## create intermediary datasets
  game_logs <- game_logs(seasons = season, result_types = "team", season_types = "Regular Season") %>% 
    filter(idGame <= max(game_ids), idGame >= min(game_ids))
  game_locations <- game_logs %>% select(idGame, idTeam, locationGame)
  
  ## get features for modeling
  cat_features <- game_logs %>%
    filter(locationGame == "H") %>% 
    select(slugTeam, slugOpponent, dateGame, isWin, isB2BSecond, idGame, idTeam) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.logical, as.factor)
  
  num_features <- advanced_stats %>% 
    select(where(is.double)) %>% 
    arrange(idTeam, idGame) %>% 
    group_by(idTeam) %>% 
    # compute 3 lags & 2 ma's for every indicator 
    mutate_at(. , vars(-starts_with("id")), 
              funs(l1 = lag(., 1), l2 = lag(., 2), l3 = lag(., 3))) %>% 
    mutate_at(., vars(ends_with("_l1")),
              funs(m3 = rollmean(., 3, fill = NA, align = 'right'), 
                   m5 = rollmean(., 5, fill = NA, align = 'right'))) %>% 
    select(idTeam, idGame, ends_with(c("_l1", "_l2", "_m3", "_m5"))) %>% 
    ungroup() %>% 
    select(-starts_with("pctUSG_")) %>% 
    left_join(game_locations, by = c("idTeam", "idGame")) %>% 
    relocate('locationGame', .before = idTeam) %>%
    pivot_wider(names_from = 'locationGame', 
                values_from = ends_with(c("_l1", "_l2", "_m3", "_m5")), 
                id_cols = idGame) %>% 
    arrange(idGame) %>% 
    na.omit()
  
  features <- cat_features %>% 
    rename(idHTeam = 'idTeam') %>% 
    inner_join(num_features, by = 'idGame') %>% 
    arrange(dateGame)
  
  return(features)
  
} 

# function to grab features for a given season and save file
grab_season_data <- function(season, data_folder) {
  game_ids <- game_logs(seasons = season, result_types = "team", season_types = "Regular Season") %>% 
    select(idGame) %>% 
    unique()
  
  features <- prep_features(game_ids, season)
  filename <- paste0("features_", season, ".csv")
  write_csv(features, file.path(data_folder, filename))
  return(features)
}

# plot correctly and incorrectly forecasted games
# plot 1
plot_accuracy <- function(pred_df) {
  pred_df %>% 
  ggplot(aes(x = idGame, y = .pred_TRUE, color = as.factor(correct), label = slugTeam)) +
  geom_rect(aes(xmin = min(idGame), xmax = max(idGame)), 
            ymin = 0.25, ymax = 0.75, 
            fill = "lightyellow", alpha = 0.1) +
  geom_point(size = 1, alpha = 0.5) +
  geom_text(data = subset(pred_df, correct == "Incorrect"), 
            aes(label=slugTeam), size=3, nudge_y = 0.03, 
            check_overlap = TRUE, show.legend = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  # annotate(text = "win", x = 21800000, y = 1, label = "Win") +
  geom_text(aes(x = min(idGame)-30, y = 0.75, label = "Predicted Win"),
            angle = 90, size = 3, color = "grey20") +
  geom_text(aes(x = min(idGame)-30, y = 0.25, label = "Predicted Loss"),
            angle = 90, size = 3, color = "grey20") +
  labs(color = NULL, y = "",
       title = "Predicted Win Probabilities",
       subtitle = "Using a random sample of games from the 2018-19 NBA season",
       caption = "Note: Shaded area indicates close calls.") + 
  scale_color_manual(values = c("gray80", "blue")) +
  theme_light() +
  theme(legend.position = "overlay",
        panel.grid = element_blank(),
        plot.title = element_text(family = "serif", size = 18, face = "bold"))
}
