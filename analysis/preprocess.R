library(caret)

# Zero- and Near Zero-Variance Predictors
nzv <- nearZeroVar(dt_to_use, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

zv <- apply(dt_to_use, 2, function(x) length(unique(x)) == 1)

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
              