# setwd("")
source("R/modelling_utils.R")

get.predictions <- function(model, all.data, train.filter) {
  
  covariates <- 2:length(names(all.data))
  predictions <- list()
  
  predictions$train <- as.vector(predict(model, all.data[train.filter, covariates], type = "response"))
  predictions$test <- as.vector(predict(model, all.data[!train.filter, covariates], type = "response"))
  
  for (trte in c("train", "test")){
    integer.pred <- predictions[[trte]]
    integer.pred[integer.pred < 0.5] <- 0
    for (val in 1:9) {integer.pred[(integer.pred >= (val-0.5)) & (integer.pred < (val+0.5))] <- val}
    predictions$int.pred[[trte]] <- integer.pred
  }
}

run.poisson.model <- function(mod.formula, link.type="log") {
  
  feature.set = read.csv("data/csv_of_json_data/feature/match_stats.csv")
  home.feature <- split.home.away.data(feature.set, data.to.return="home")
  away.feature <- split.home.away.data(feature.set, data.to.return="away")
  train.filter <- create.train.test.index(home.feature, seed=1)
  all.train.data <- rbind(home.feature[train.filter,], away.feature[train.filter,])
  all.test.data <- rbind(home.feature[!train.filter,], away.feature[!train.filter,])
  
  model <- glm(mod.formula, all.train.data, family=poisson(link=link.type))
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
  all.results$pseudo.r2 <- ((model$null.deviance/-2)-(model$deviance/-2))/(model$null.deviance/-2)
  
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