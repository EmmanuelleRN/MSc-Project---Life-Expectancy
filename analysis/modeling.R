library("tidyverse")
library("tidymodels")
library("parsnip")
library("brotools")
library("mlbench")

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