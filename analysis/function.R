create_workflow <- function(recipe, model){
  workflow() %>% 
    add_model(model) %>% 
    add_recipe(recipe)
}

fit_model <- function(data, recipe, model){
  create_workflow(recipe, model) %>% 
    fit(data)
}

extract_fit_information <- function(data, recipe, model){
  fit_model(data, recipe, model) %>% 
    workflows::extract_fit_parsnip() %>% 
    tidy()
}

fit_model_resample <- function(data, recipe, model, folds = 10){
  folds <- vfold_cv(data, v = folds)
  
  keep_predictions <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
  
  workflow(recipe, model) %>% 
    fit_resamples(resamples = folds, control = keep_predictions)
}

fit_best_model <- function(data_train, recipe, model, data_test, folds = 10, model_fit = NA){
  if(!is.null(model_fit)){
    fitted_model <- model_fit
  }
  else{
    fitted_model <- fit_model_resample(data_train, recipe, model, folds = 10)
  }
  
  best_model <- fit_best(fitted_model, verbose = TRUE)
  
  data_test |>
    bind_cols(predict(best_model, data_test))
}

get_predictions <- function(data_train, recipe, model, data_test, model_fit = NA){
  if(!is.null(model_fit)){
    model_after_fit <- model_fit
  }
  else{
    model_after_fit <- fit_model(data_train, recipe, model)
  }
  
  augment(model_after_fit, data_test)
}

get_performance_metrics <- function(data_train, recipe, model, data_test, model_fit = NA){
  prediction <- get_predictions(data_train, recipe, model, data_test, model_fit = NA)
  
  prediction_metrics <- metric_set(rmse, rsq, mae)
  prediction_metrics(prediction, truth = life_expectancy_at_birth_total_years, estimate = .pred)
}

# unit test

create_workflow(preprocess_recipe, lr_mod)

fit_model(data = train_data[1:1000,], recipe = preprocess_recipe, model = lr_mod)

extract_fit_information(data = train_data[1:500,], recipe = preprocess_recipe, model = lr_mod)

get_performance_metrics(data_train = train_data[1:300,], recipe = preprocess_recipe, model = lr_mod,
                        data_test = test_data[1:300])

fit_model_resample(data = train_data[1:500,], recipe = preprocess_recipe, model = lr_mod)
