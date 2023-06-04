library(caret)
#library(zoo)
library(tidymodels)
library(rsample)

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(88)
# Put 3/4 of the data into the training set 
data_split <- initial_split(dt_filtered, prop = 3/4)

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

lr_mod <- 
  decision_tree(mode = "regression")

wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(preprocess_recipe)


fit_dec_tree <- 
  wflow %>% 
  fit(data = train_data)

fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

predict(fit, test_data)

flights_aug <- 
  augment(fit, test_data)

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
              