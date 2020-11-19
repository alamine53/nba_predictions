library(nbastatR)
library(tidyverse)
library(tidymodels)

# folder setup
setwd("~/R/nba_forecasting")
data_folder <- "data"
src_folder <- "src"
output_folder <- "output"


# load functions
source('src/functions.R')

# relevant files
modeling_dataset <- file.path(data_folder, "features_2019.csv")
# Alternatively, uncomment below line if need to get data for a new season
# grab_season_data(2019)
save_final_model <- file.path(src_folder, "PredictionModel.rds")
  
# (option A) load dataset
# df <- read_csv(modeling_dataset) %>% 
#   na.omit() %>% 
#   mutate_if(is.logical, as.factor) %>% 
#   mutate_if(is.character, as.factor)

# # (option B) Alternatively, uncomment below line if need to get data for a new season
df <- grab_season_data(2019, data_folder) %>%
  mutate_if(is.logical, as.factor) %>%
  mutate_if(is.character, as.factor)


# split data
set.seed(123)
games_split <- initial_split(df)
games_train <- training(games_split)
games_test <- testing(games_split)
games_folds <- vfold_cv(games_train)

# define recipe
games_rec <- recipe(isWin ~ ., data = games_train) %>%
  update_role(idGame, dateGame, new_role = "ID") %>%
  update_role(isWin, new_role = "outcome") %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% #one-hot encoding
  step_normalize(all_numeric()) %>% 
  prep()

# define model
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000, 
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

# ======================  Hyper-parameter tuning (START) ================== #

# define workflow for tuning
tune_wf <- workflow() %>% 
  add_recipe(games_rec) %>% 
  add_model(tune_spec)

# set grid to loop over
set.seed(345)
rf_grid <- grid_regular(
  mtry(range = c(25,80)),
  min_n(range = c(2, 15)),
  levels = 5
)

# train model
regular_res <- tune_grid(
  tune_wf,
  resamples = games_folds, # we're tuning the worlkflow on each fold in the dataset
  grid = rf_grid
)

# evaluate hyperparameters
regular_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point()

# evaluate hyperparameters
regular_res %>% 
  collect_metrics() %>% 
  arrange(-mean)

# ======================  Hyper-parameter tuning  (END) ================== #
# ======================  Train best ML model ================== #

# identify model with best evaluation criteria
best_auc <- select_best(regular_res, "roc_auc")

# define "final" model
final_rf <- finalize_model(
  tune_spec, 
  best_auc
)

# set workflow to use the "final" model
final_wf <- workflow() %>% 
  add_recipe(games_rec) %>% 
  add_model(final_rf)

# train model on training set and evaluate on test set
final_res <- final_wf %>% 
  last_fit(games_split) 

# make sure ROC is still around the same (means model is NOT over-fitting)
final_res %>% 
  collect_metrics()

# get predicted values
pred_df <- final_res %>% 
  collect_predictions() %>% 
  mutate(correct = case_when(isWin == .pred_class ~ "Correct", TRUE ~ "Incorrect")) %>% 
  bind_cols(games_test)

# assess visually
plot_accuracy(pred_df)

# ==================== Train final model on full dataset, and save it ========================== #
final_rf <- final_wf %>% 
  fit(df)
saveRDS(final_rf, save_final_model)
