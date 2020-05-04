# setwd("")
source("R/modelling_utils.R")

get.predictions <- function(model, all.data, train.filter) {
  
  covariates <- 2:length(names(all.data))
  predictions <- list()
  
  all.data[train.filter,1] <- as.factor(all.data[train.filter,1])
  predictions$train <- predict(model, all.data[train.filter,covariates], type = "response")
  predictions$test <- predict(model, all.data[!train.filter,covariates], type = "response")
}


run.ordered.logit.model <- function(mod.formula, Hess=TRUE) {
  
  feature.set = read.csv("data/csv_of_json_data/feature/match_stats.csv")
  home.feature <- split.home.away.data(feature.set, data.to.return="home")
  away.feature <- split.home.away.data(feature.set, data.to.return="away")
  train.filter <- create.train.test.index(home.feature, seed=1)
  all.train.data <- rbind(home.feature[train.filter,], away.feature[train.filter,])
  all.test.data <- rbind(home.feature[!train.filter,], away.feature[!train.filter,])
  
  model <- polr(mod.formula, all.data[train.filter,], Hess=Hess)
  home.predictions <- get.predictions(model, home.feature, train.filter)
  away.predictions <- get.predictions(model, away.feature, train.filter)
  
  train.pred <- get.result.from.score(home.predictions$train, away.predictions$train)
  train.true <- get.result.from.score(home.feature[train.filter,1], away.feature[train.filter,1])
  train.pred.int <- get.result.from.score(home.predictions$int.pred$train, away.predictions$int.pred$train)
  
  test.pred <- get.result.from.score(home.predictions$test, away.predictions$test)
  test.true <- get.result.from.score(home.feature[!train.filter,1], away.feature[!train.filter,1])
  test.pred.int <- get.result.from.score(home.predictions$int.pred$test, away.predictions$int.pred$test)
  
  
  all.results <- list()
  all.results$model <- model
  all.results$home.predictions <- home.predictions
  all.results$away.predictions <- away.predictions
  
  all.results$match.results <- list(train=list(), test=list())
  
  all.results$match.results$train$pred <- train.pred
  all.results$match.results$train$pred.int <- train.pred.int
  all.results$match.results$train$true <- train.true
  
  all.results$match.results$test$pred <- test.pred
  all.results$match.results$test$pred.int <- test.pred.int
  all.results$match.results$test$true <- test.true
  
  all.results$evaluations <- get.all.evaluations(all.results$match.results)
  
  return(all.results)
}