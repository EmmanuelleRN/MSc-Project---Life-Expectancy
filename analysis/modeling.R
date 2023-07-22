library("tidyverse")
library("tidymodels")
library("parsnip")
library("brotools")
library("mlbench")

library(kknn)
library(ranger)

knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

all_models_wfs <- workflow_set(
  preproc = list(preprocess_recipe),
  models = list(knn = knn_mod, rf = rf_model)
)

all_models_wfs

all_models_fit <- workflow_map(
  all_models_wfs, 
  resamples = vfold_cv(test_data %>% filter(!is.na(life_expectancy_at_birth_total_years))),
  metrics = metric_set(rmse, rsq, mae)
)


all_models_fit %>%
  collect_metrics()

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- 
  rf_wflow %>% 
  fit_resamples(resamples = ames_folds, control = keep_pred)
rf_res

simple_recipe <- function(dataset){
  recipe(life_expectancy_at_birth_total_years ~ ., data = train_data) %>%
    update_role(country_name, country_code, new_role = "ID") %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(na_rm = FALSE) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
}

my_rf <- function(mtry, trees, split, id){
  
  validation_data <- mc_cv(train_data, prop = 0.9, times = 30)
  
  analysis_set <- analysis(split)
  
  analysis_prep <- prep(simple_recipe(analysis_set), training = analysis_set)
  
  analysis_processed <- bake(analysis_prep, newdata = analysis_set)
  
  model <- rand_forest(mtry = mtry, trees = trees) %>%
    set_engine("ranger", importance = 'impurity') %>%
    fit(price ~ ., data = analysis_processed)
  
  assessment_set <- assessment(split)
  
  assessment_prep <- prep(simple_recipe(assessment_set), testing = assessment_set)
  
  assessment_processed <- bake(assessment_prep, newdata = assessment_set)
  
  tibble::tibble("id" = id,
                 "truth" = assessment_processed$price,
                 "prediction" = unlist(predict(model, new_data = assessment_processed)))
}