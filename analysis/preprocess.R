library(caret)
#library(zoo)
library(tidymodels)
library(rsample)

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(88)
# Put 3/4 of the data into the training set 
data_split <- initial_split(dt_filtered %>% select(-region) %>% filter(!is.na(life_expectancy_at_birth_total_years)), prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Zero- and Near Zero-Variance Predictors
nzv <- nearZeroVar(dt_filtered, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

preprocess_recipe <- recipe(life_expectancy_at_birth_total_years ~ ., data = train_data) %>% 
  update_role(country_name, country_code, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_corr(all_numeric_predictors()) %>%
  step_normalize(na_rm = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

lr_mod <- linear_reg()

knn_mod <- 
  nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

wflow <- workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(preprocess_recipe)

wflow_knn <- workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(preprocess_recipe)

wflow_rf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(preprocess_recipe)

fit_linear_reg <- wflow %>% 
  fit(data = train_data)

save(fit_linear_reg, file = "lin_reg_fit.RData")

fit_knn <- wflow_knn %>% 
  fit(data = train_data)

fit_rf <- wflow_rf %>% 
  fit(data = train_data)

fit_linear_reg %>% 
  workflows::extract_fit_parsnip() %>% 
  tidy()

pred_linear_reg <- augment(fit_linear_reg, test_data)

pred_linear_reg %>% yardstick::rmse(life_expectancy_at_birth_total_years, .pred)

pred_linear_reg_metrics <- metric_set(rmse, rsq, mae)
pred_linear_reg_metrics(pred_linear_reg, truth = life_expectancy_at_birth_total_years, estimate = .pred)

pred_linear_reg_train <- augment(fit_linear_reg, train_data)

pred_linear_reg_metrics_train <- metric_set(rmse, rsq, mae)
pred_linear_reg_metrics_train(pred_linear_reg, truth = life_expectancy_at_birth_total_years, estimate = .pred)


pred_rf <- augment(fit_rf, test_data)

pred_rf %>% yardstick::rmse(life_expectancy_at_birth_total_years, .pred)

pred_rf_metrics <- metric_set(rmse, rsq, mae)
pred_rf_metrics(pred_rf, truth = life_expectancy_at_birth_total_years, estimate = .pred)

#cv

ames_folds <- vfold_cv(train_data, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
lg_res <- 
  wflow %>% 
  fit_resamples(resamples = ames_folds, control = keep_pred)
lg_res

collect_metrics(lg_res)

collect_predictions(lg_res)

collect_predictions(lg_res) %>% 
  ggplot(aes(x = life_expectancy_at_birth_total_years , y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted Life Expectancy")

ames_folds <- vfold_cv(train_data, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

##

## Create a grid of hyperparameter values to test
k_grid <- tibble(neighbors = c(2, 3, 4, 5, 10, 15, 25, 45, 60, 80, 100))

knn_cv <- 
  wflow_knn %>% 
  tune_grid(resamples = ames_folds,
            grid = k_grid)
knn_cv

best_knn <- select_best(knn_cv, "rmse")

final_knn <- finalize_model(
  knn_mod,
  best_knn
)

final_knn_mod <- workflow() %>%
  add_recipe(preprocess_recipe) %>%
  add_model(final_knn)

final_knn_res <- final_knn_mod %>%
  last_fit(data_split)

final_knn_res %>% collect_metrics()

final_knn_res %>% collect_predictions()

collect_predictions(final_knn_res) %>% 
  ggplot(aes(x = life_expectancy_at_birth_total_years , y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted Life Expectancy")

collect_predictions(final_knn_res) %>% 
  mutate(diff_abs = abs(life_expectancy_at_birth_total_years - .pred)) %>% 
  arrange(desc(diff_abs))

rf <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

wflow_rf <- workflow() %>% 
  add_model(rf) %>% 
  add_recipe(preprocess_recipe)

#doParallel::registerDoParallel()

#set.seed(345)
tune_res <- tune_grid(
  wflow_rf,
  resamples = ames_folds,
  grid = 40
)


collect_metrics(tune_res) |>
  filter(.metric == "rmse") |>
  select(mean, mtry, trees, min_n) |>
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) |>
  ggplot() +
  aes(x = value, y = mean, color = parameter) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

best_rf <- select_best(tune_res, "rmse")

final_rf <- finalize_model(
  rf,
  best_rf
)

final_rf_mod <- workflow() %>%
  add_recipe(preprocess_recipe) %>%
  add_model(final_rf)

final_rf_res <- final_rf_mod %>%
  last_fit(data_split)

final_rf_res %>% collect_metrics()

final_rf_res %>% collect_predictions()

collect_predictions(final_rf_res) %>% 
  ggplot(aes(x = life_expectancy_at_birth_total_years , y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted Life Expectancy")

knn_cv <- 
  wflow_knn %>% 
  tune_grid(resamples = ames_folds,
            grid = k_grid)
knn_cv

collect_metrics(knn_cv)

collect_predictions(knn_cv) %>% 
  ggplot(aes(x = life_expectancy_at_birth_total_years , y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted Life Expectancy")


flights_aug1 <- 
  augment(fit_dec_tree, test_data)

#zv <- apply(dt_to_use, 2, function(x) length(unique(x)) == 1)

library(lares)

corr_vars <- corr_cross(dt_filtered, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 400
)

corr_vars_to_drop <- corr_vars$data |> 
  filter(corr >= 0.9) %>% 
  .$mix %>% 
  unique()

dt_no_corr <- dt_filtered |>
  select(-all_of(corr_vars_to_drop))

# correlation
descrCor <-  cor(dt_to_use %>% select(-c(country_name, country_code, region, year)), 
                 use = "pairwise.complete.obs")
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9, na.rm = TRUE)
high_cor <- abs(descrCor[upper.tri(descrCor)]) > .9
filter_cor <- descrCor[-high_cor]
summary(descrCor[upper.tri(descrCor)])

library(reshape2)

#melt the data frame
melted_cor <- melt(descrCor)
melted_cor %>% filter(abs(value) > .9) %>% filter(Var1 != Var2)

lmHeight = lm(life_expectancy_at_birth_total_years~., 
              data = dt_to_use %>% select(-c(country_name, country_code, region))) 

library(randomForest)
set.seed(71)
rf <-randomForest(life_expectancy_at_birth_total_years~.,data=dt_to_use %>% select(-c(country_name, country_code, region, year)), ntree=500,
                  na.action = na.roughfix, importance = TRUE) 
print(rf)
              