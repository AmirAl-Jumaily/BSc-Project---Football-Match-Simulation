library(stringr)
library(tidyr)

split.home.away.data <- function(all.data, data.to.return="both") {
  
  remove.keep.col.names.containing.str <- function(df, substrs.to.find, keep.or.drop="drop") {
    new.df = data.frame(df)
    for (substr.to.find in substrs.to.find) {
      indexes <- grep(substr.to.find, names(new.df))
      if (keep.or.drop == "keep") {
        new.df <- subset(new.df, select = indexes)
      } else if (keep.or.drop == "drop") {
        new.df <- subset(new.df, select = -indexes)
      }
    }
    return(new.df)
  }
  
  subs.data <- remove.keep.col.names.containing.str(all.data,
                                                    c("country", "league", "fixture", "team"))
  
  home.data <- subs.data[seq(1, length(subs.data[1,]), by=2)]
  away.data <- subs.data[seq(2, length(subs.data[1,]), by=2)]
  
  home.data$is.home <- 1
  away.data$is.home <- 0
  
  names(home.data) <- str_remove(names(home.data), "home_")
  names(away.data) <- str_remove(names(away.data), "away_")
  
  if (data.to.return == "both") {
    subs.data <- rbind(home.data, away.data)
    subs.data$is.home <- factor(subs.data$is.home)
    return(subs.data)
  } else if (data.to.return == "home") {
    home.data <- within(home.data, rm(is.home))
    return(home.data)
  } else if (data.to.return == "away") {
    away.data <- within(away.data, rm(is.home))
    return(away.data)
  } else if (data.to.return == "split") {
    return(list(home_data=within(home.data, rm(is.home)), away_data=within(away.data, rm(is.home))))
  }
}


create.train.test.index <- function(all.data, seed=NULL, train.size=0.8, as.bool=FALSE) {
  if (is.numeric(seed)) {set.seed(seed=seed)}
  train.filter <- sample.int(length(all.data[,1]), round(length(all.data[,1]) * train.size))
  if (as.bool) {train.filter <- (1:length(all.data[,1])) %in% train.filter}
  
  return(train.filter)
}


get.all.evaluations <- function(match.results) {
  all.evaluations <- list(train=list(pred=list(), pred.int=list()), test=list(pred=list(), pred.int=list()))
  
  for (trte in c("train", "test")){
    for (pred.type in c("pred", "pred.int")) {
      for (val.type in c("score.diff", "team.winner")) {
        all.evaluations[[trte]][[pred.type]][[val.type]] = get.evaluations(
          match.results[[trte]][[pred.type]][[val.type]], match.results[[trte]]$true[[val.type]])
        }}}
  
  return(all.evaluations)
}


get.evaluations <- function(predictions, observed, all.evaluations=FALSE) {
  evaluations <- list()
  if (all.evaluations) {
    evaluations$corr       <- round(cor(predictions, observed), 2)
    evaluations$RMSE       <- RMSE(predictions, observed)
    evaluations$MAE        <- MAE(predictions, observed)
    evaluations$diff       <- predictions - observed
    evaluations$abs.diff   <- abs(predictions - observed)
  }
  
  evaluations$conf.matrix   <- table(predictions, observed)
  evaluations$correct.pred  <- mean(predictions == observed)
  return(evaluations)
}


RMSE <- function(m, o){
  m <- unlist(m, use.names = FALSE)
  o <- unlist(o, use.names = FALSE)
  round(sqrt(mean((m - o)^2)), 2)
}


MAE <- function(m, o){
  m <- unlist(m, use.names = FALSE)
  o <- unlist(o, use.names = FALSE)
  round(mean(abs(m - o)), 2)
}


get.result.from.score <- function(home.predictions, away.predictions, threshold) {
  match.results <- list()
  
  match.results$score_diff <- home.predictions - away.predictions
  team.winner <- home.predictions - away.predictions
  draws <- abs(team.winner) <= threshold
  home.win <- team.winner > threshold
  away.win <- team.winner < -threshold
  team.winner[draws] <- "draw"
  team.winner[home.win] <- "home"
  team.winner[away.win] <- "away"
  match.results$team.winner <- team.winner
  
  return(match.results)
}
