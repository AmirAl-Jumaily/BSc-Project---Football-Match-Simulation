# setwd("")
source("R/01_prediction_model/02_looking_at_different_models/models.R")
source("R/modelling_utils.R")

feature.set = read.csv("data/csv_of_json_data/feature/match_stats.csv")
home.feature <- split.home.away.data(feature.set, data.to.return="home")
away.feature <- split.home.away.data(feature.set, data.to.return="away")

library(gbm)

train = sample(1:nrow(home.feature), 4500)

home.model = gbm(goals~.-goals, data=home.feature[train,], distribution="gaussian", n.trees=5000, shrinkage=0.01,
            interaction.depth=10)

n.trees = seq(from=100, to=10000, by=100)
home.predmat = predict(home.model, newdata=home.feature[-train,], n.trees=n.trees)
home.berr = with(home.feature[-train,], apply((home.predmat-goals)^2, 2, mean)) # Minimised MSE at 1300 trees. Overfit afterwards.
plot(n.trees, home.berr, pch=19, ylab="MSE", xlab="# Trees", main="Boosting Test Error")
# abline(h=min(test.err), col="red")

away.model = gbm(goals~.-goals, data=away.feature[train,], distribution="gaussian", n.trees=5000, shrinkage=0.01,
                 interaction.depth=10)

n.trees = seq(from=100, to=10000, by=100)
away.predmat = predict(away.model, newdata=away.feature[-train,], n.trees=n.trees)
away.berr = with(away.feature[-train,], apply((away.predmat-goals)^2, 2, mean)) # Minimised MSE at 1500 trees. Overfit afterwards.
plot(n.trees, away.berr, pch=19, ylab="MSE", xlab="# Trees", main="Boosting Test Error")

match.results.pred = get.result.from.score(home.predmat[,4], away.predmat[,2])
match.results.true = get.result.from.score(home.feature[-train, "goals"], away.feature[-train, "goals"])

evaluations.score.diff = get.evaluations(match.results.pred$score.diff, match.results.true$score.diff)
evaluations.team.winner = get.evaluations(match.results.pred$team.winner, match.results.true$team.winner)


