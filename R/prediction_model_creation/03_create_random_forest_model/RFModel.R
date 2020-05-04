# NOTE: This file was run adhoc for testing

library(caret)
library(randomForest)
perform.parameter.tuning.by.cv <- function(doBest=F) {
  feature.set <- readRDS("R/all_data/feature_set.RDS")
  feature.set <- feature.set[, c(-1:-4)]
  
  print(paste0("Beginning home tuning: ", Sys.time()))
  home.tuning <- tuneRF(x=feature.set[, 3:64], y=feature.set[, 1],
                        ntreeTry=500, stepFactor=3, improve=0.00001, doBest=doBest)
  print(paste0("Completed home tuning: ", Sys.time()))
  print(paste0("Beginning away tuning: ", Sys.time()))
  away.tuning <- tuneRF(x=feature.set[, 3:64], y=feature.set[, 2],
                        ntreeTry=500, stepFactor=3, improve=0.00001, doBest=doBest)
  print(paste0("Completed away tuning: ", Sys.time()))
  
  return(list(home.tuning=home.tuning, away.tuning=away.tuning))
}

perform.parameter.gridsearch.by.cv <- function(vector.of.mtrys, number.of.cv.folds, repeats, number.of.cols, use.ratings=FALSE) {
  feature.set <- readRDS("R/all_data/feature_set.RDS")
  feature.set <- feature.set[, c(-1:-4)]
  
  if (!use.ratings) {
    feature.set <- feature.set[, 1:34]
  }
  
  control <- trainControl(method="repeatedcv", number=number.of.cv.folds, repeats=repeats, search="grid")
  tunegrid <- expand.grid(.mtry=vector.of.mtrys)
  print(paste0("Beginning home grid search: ", Sys.time()))
  home.rf.gridsearch <- train(home_goals~., data=feature.set[, c(1, 3:number.of.cols)],
                              method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
  print(paste0("Completed home grid search", Sys.time()))
  
  # print(paste0("Beginning away grid search: ", Sys.time()))
  # away.rf.gridsearch <- train(away_goals~., data=feature.set[, c(2, 3:number.of.cols)],
  #                             method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
  # print(paste0("Completed away grid search", Sys.time()))

  return(list(home.rf.gridsearch=home.rf.gridsearch
              # , away.rf.gridsearch=away.rf.gridsearch
              ))
}


create.random.forest.models.for.testing <- function(train.size=0.8) {
  # Get feature set
  feature.set <- readRDS("R/all_data/feature_set.RDS")
  feature.set <- feature.set[, c(-1:-4)]
  
  train.filter <- create.train.test.index(feature.set, train.size=train.size, as.bool=TRUE)
  
  # Create models from feature set
  home.model <- randomForest(home_goals~., data=feature.set[train.filter, -2], ntree=1000)
  away.model <- randomForest(away_goals~., data=feature.set[train.filter, -1], ntree=1000)
  random.forest.models <- list(home.model=home.model, away.model=away.model, train.filter=train.filter)
  
  return(random.forest.models)
}


evaluate.model.performance <- function(random.forest.models, train=NULL, draw.threshold=0.35, predict.training.set=FALSE) {
  feature.set <- readRDS("R/all_data/feature_set.RDS")
  feature.set <- feature.set[, c(-1:-4)]
  
  if (predict.training.set) {
    data.subset <- random.forest.models$train.filter
  } else {
    data.subset <- !random.forest.models$train.filter
  }
  # data.subset <- rep_len(TRUE, length.out = nrow(feature.set))
  predictions <- list()

  predictions$home <- predict(random.forest.models$home, newdata=feature.set[data.subset,], type="response")
  predictions$away <- predict(random.forest.models$away, newdata=feature.set[data.subset,], type="response")

  match.results <- get.result.from.score(predictions$home, predictions$away, threshold=draw.threshold)
  
  actual.winner <- get.result.from.score(feature.set$home_goals, feature.set$away_goals, threshold = draw.threshold)
  feature.set$result <- actual.winner$team.winner

  evaluations <- get.evaluations(as.factor(match.results$team.winner), feature.set[data.subset,"result"])
  score.diff.correlation <- cor(unname(match.results$score_diff),
                                (feature.set[data.subset,"home_goals"] - feature.set[data.subset,"away_goals"]))

  return(list(predictions=predictions, match.results=match.results,
              score.diff.correlation=score.diff.correlation, evaluations=evaluations))
  # Got 74.86% correct predictions of match result this way.
  # 91.02% home wins correctly predicted. 36.51% draws predicted correctly. 80.23% away wins predicted.
  # Also got very high correlation of score.diff in the matches at around 86%.
  # Some incorrect predictions may be on the boundary. Improvements over the last attempt.
}

# RUN THE BELOW AND CHECK THE SCORES THEN SEE IF IT CHANGES MUCH WHEN YOU GO UP TO 0.9
# random.forest.models <- create.random.forest.models.for.testing(train.size=0.9)
# evaluation <- evaluate.model.performance(random.forest.models, draw.threshold = 0.35)
# evaluation$evaluations$correct.pred
# evaluation$evaluations$conf.matrix
# evaluation$score.diff.correlation

# PARAMETER TUNING MTRY
# cv.results <- perform.parameter.tuning.by.cv(doBest=F)
# best.models <- perform.parameter.tuning.by.cv(doBest=T)
# 
# gridsearch.results <- perform.parameter.gridsearch.by.cv(
#   vector.of.mtrys=seq(from=15, to=30, by=3), number.of.cv.folds=2, repeats=1, 64)
# gridsearch.results2 <- perform.parameter.gridsearch.by.cv(
#   vector.of.mtrys=seq(from=30, to=45, by=3), number.of.cv.folds=3, repeats=2, 64)
# 
# gridsearch.results3 <- perform.parameter.gridsearch.by.cv(
#   vector.of.mtrys=seq(from=10, to=50, by=5), number.of.cv.folds=3, repeats=2, 34)

gridsearch.results <- perform.parameter.gridsearch.by.cv(
  vector.of.mtrys=seq(from=10, to=25, by=1), number.of.cv.folds=5, repeats=1, 34)

gridsearch.results2 <- perform.parameter.gridsearch.by.cv(
  vector.of.mtrys=seq(from=27, to=33, by=1), number.of.cv.folds=3, repeats=3, 34)

cv.results <- perform.parameter.tuning.by.cv(doBest=F)










if (FALSE) {
  # # Now we apply the above to home and away team seperately and see if it does better.
  # 
  # home.feature <- split.home.away.data(feature.set, data.to.return="home")
  # home.feature$result <- feature.set$result
  # away.feature <- split.home.away.data(feature.set, data.to.return="away")
  # away.feature$result <- feature.set$result
  # 
  # home.model <- randomForest(goals~., data=home.feature[,1:13], subset=train, ntree=1000)
  # home.predictions <- predict(home.model, newdata=home.feature[-train,2:13], type="response")
  # # home.evaluations <- get.evaluations(home.predictions, home.feature[-train,"result"])
  # 
  # away.model <- randomForest(goals~., data=away.feature[,1:13], subset=train, ntree=1000)
  # away.predictions <- predict(away.model, newdata=away.feature[-train,2:13], type="response")
  # # away.evaluations <- get.evaluations(away.predictions, away.feature[-train,"result"])
  # 
  # match.result <- get.result.from.score(home.predictions, away.predictions)
  # evaluations <- get.evaluations(as.factor(match.result$team.winner), feature.set[-train,"result"])
  # evaluations$correct.pred
  # # Seems correct prediction percentage reduced for both models. Probably because it didn't take into account
  # # stats of the opposition. 53% and 55% for home and away. Again Home highly overpredicted but also Draws
  # # highly underpredicted.
  # 
  # # I NEED TO SEE HOW OTHER RESERACHERS DID THEIR MODELLING. WHOSE STATS DID THEY PUT INTO THE MODEL AND HOW
  # # DID THEY CODE THE HOME EFFECT. WHY AM I NOT GETTING THE SAME PREDICTION RATES AS THEM?
  # 
  # #####################################################################################################################

  # home.feature$result <- as.factor(result)

  # oob.err = double(12)
  # test.err = double(12)
  #
  # for (mtry in 1:12) {
  #   fit=randomForest(goals~.-goals-result, data=home.feature, subset=train, mtry=mtry, ntree=1000)
  #   oob.err[mtry] = fit$mse[400]
  #   pred = predict(fit, home.feature[-train,])
  #   test.err[mtry]=with(home.feature[-train,], mean((goals-pred)^2))
  #   cat(mtry, " ")
  # }
  # matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c("red", "blue"), type="b", ylab="MSE")
  # legend("topright", legend=c("OOB", "Test"), pch=19, col=c("red", "blue"))
}
