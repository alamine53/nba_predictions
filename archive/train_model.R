library(tidyverse)
library(skimr)
library(janitor)


# folder setup
setwd("~/R/nba_forecasting")
data_folder <- "data"
output_folder <- "output"
df_file <- file.path(data_folder, "modeling_df.csv")

# load data
df <- read.csv(df_file) %>% 
  select(-starts_with("pctUSG_"), -starts_with("opp.pctUSG_")) %>% 
  mutate(isWin = as.factor(isWin))

# ============================= TRAIN MODEL  ================================= #

library(tidymodels)
set.seed(123)
games_split <- initial_split(df)
games_train <- training(games_split)
games_test <- testing(games_split)

games_rec <- recipe(isWin ~ ., data = games_train) %>%
  update_role("idGame", new_role = "ID") %>%
  # step_naomit(isWin, skip = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% #one-hot encoding
  step_normalize(all_numeric()) %>% 
  prep()

juiced <- juice(games_rec)
juiced %>% count(isWin)

# ============================= SPECIFY MODELS & WORKFLOW  ================================= #

# Model 1: Random Forest
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000, 
  min_n = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

# design a workflow
tune_wf <- workflow() %>% 
  add_recipe(games_rec) %>% 
  add_model(tune_spec)

# ============================= BEGIN TRAINING  ================================= #
set.seed(234)

# cross-validation
games_folds <- vfold_cv(games_train)
# doParallel::registerDoParallel()
set.seed(345)
# tune_res <- tune_grid(
#   tune_wf, 
#   resamples = games_folds, # we're tuning the worlkflow on each fold in the dataset
#   grid = 10
# )
start_time <- Sys.time()
rf_grid <- grid_regular(
  mtry(range = c(25,80)),
  min_n(range = c(2, 15)),
  levels = 5
)
regular_res <- tune_grid(
  tune_wf,
  resamples = games_folds, # we're tuning the worlkflow on each fold in the dataset
  grid = rf_grid
)
end_time <- Sys.time()
end_time - start_time

# ============================= ASSESS RESULTS  ================================= #
# start the clock
start_time <- Sys.time()

# tune_res %>% 
#   collect_metrics() %>% 
#   filter(.metric == "roc_auc") %>% 
#   select(mean, min_n, mtry) %>% 
#   pivot_longer(min_n:mtry, 
#                values_to = "value",
#                names_to = "parameter") %>% 
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~ parameter, scales = "free_x")

regular_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point()
  
  
# stop the clock
end_time <- Sys.time()
end_time - start_time


# ============================= IDENTIFY BEST MODEL  ================================= #

# find best metrics
tune_res %>% 
  collect_metrics() %>% 
  arrange(-mean)
regular_res %>% 
  collect_metrics() %>% 
  arrange(-mean) %>% 
best_auc_tune <- select_best(tune_res, "roc_auc")
best_auc_reg <- select_best(regular_res, "roc_auc")

# tune based on best model
final_rf <- finalize_model(
  tune_spec, 
  best_auc_reg
)

start_time <- Sys.time()

library(vip)
final_rf %>% 
  set_engine("ranger", importance = "permutation") %>% 
  fit(isWin ~ ., 
      data = juice(games_rec) %>% 
        select(-idGame)) %>% 
  vip(geom = "col")

final_wf <- workflow() %>% 
  add_recipe(games_rec) %>% 
  add_model(final_rf)

# ============================= TRAIN BEST MODEL ON TRAINING SET  ================================= #
# train model on training set and evaluate on test set
final_res <- final_wf %>% 
  last_fit(games_split) 

# about the same ROC which means we're NOT over-fitting
final_res %>% 
  collect_metrics()

# get predicted values
pred_df <- final_res %>% 
  collect_predictions() %>% 
  mutate(correct = case_when(isWin == .pred_class ~ "Correct", TRUE ~ "Incorrect")) %>% 
  bind_cols(games_test)

write.csv(pred_df, "df_predictions.csv")




# ============================= TRAIN BEST MODEL ON FULL SET  ================================= #
final_rf <- final_wf %>% 
  fit(df)
saveRDS(final_rf, "src/nba_rf_model.rds")
