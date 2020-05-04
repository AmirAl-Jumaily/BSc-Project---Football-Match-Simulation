library(car)
library(lars)
library(leaps)

feature_set = read.csv("data/csv_of_json_data/feature/match_stats.csv")
feature_set = feature_set[21:46]
lasso.m <- list()
lasso.m$data <- create.train.test.sets(feature_set)
lasso.m$model <- lars(as.matrix(lasso.m$data$train[,3:26]), as.numeric(lasso.m$data$train[,1]), type="lasso")
lasso.m$pred = list()
lasso.m$data$train$pred <- predict(lasso.m$model, lasso.m$data$train[,3:26], type="fit")$fit[,27]
lasso.m$pred$test <- predict(lasso.m$model, lasso.m$data$test[,3:26], type="fit")$fit[,27]
lasso.m$metrics <- create.model.evaluation.metrics(lasso.m)

leaps.mode <- regsubsets(away_goals~., data=feature_set[,2:26], nbest=1, method="forward")
subsets(leaps.mode, statistics="bic")