############################################################################################################################
###################################################### Main functions ######################################################
run.simulations <- function(nSims, league.name.or.teams, season, draw.threshold=0.35) {
  if (!("random.forest.models" %in% ls())) {
    random.forest.models <- readRDS(
      "R/all_data/random_forest_models.RDS")
  }
  simulations <- get.all.fixtures.sim.match.stats(nSims, league.name.or.teams, season)
  predictions <- evaluate.model(nSims, simulations, random.forest.models, season, draw.threshold = draw.threshold)
  league.standings <- get.league.standings(predictions)
  
  return(list(league.standings=league.standings, match.predictions=predictions, simulation.inputs=simulations))
}

get.all.fixtures.sim.match.stats <- function(nSims, league.name.or.teams, season) {
  fixtures <- get.all.fixtures.from.league.season(league.name.or.teams, season)
  
  home.ids <- fixtures$fixtures.team.ids$home
  away.ids <- fixtures$fixtures.team.ids$away
    
  simulations <- sim.match.events(nSims, home.ids, away.ids, season)
    
  names(simulations$home.team.stats) <- c(paste0("home_", names(simulations$home.team.stats)))
  names(simulations$away.team.stats) <- c(paste0("away_", names(simulations$away.team.stats)))
  
  teams <- as.data.frame(
    list(home_team=fixtures$fixtures.team.names$home, away_team=fixtures$fixtures.team.names$away))
  
  fixtures$match.stats <- cbind(teams, simulations$home.team.stats, simulations$away.team.stats)
  
  
  return(fixtures)
}

evaluate.model <- function(nSims, simulations, random.forest.models, season, draw.threshold=0.35) {
  ##########################################################################################################################
  #################################           Helper functions of evaluate.model           #################################
  attach.ratings.columns.to.match.stats.data <- function(match.stats, season) {
    
    name.rating.mapper <- function(team.name, mapper, rating.name, season) {
      season.mapper <- mapper[[season]]
      return.value <- season.mapper[season.mapper$team.name == team.name, rating.name]
      if (length(return.value)>1) {print(return.value); return(-1)}
      else {return(return.value)}
    }
    
    match.stats$home_team <- str_replace_all(match.stats$home_team, " ", "\\.")
    match.stats$away_team <- str_replace_all(match.stats$away_team, " ", "\\.")
    
    all.seasons.ratings.data <- readRDS(
      "R/all_data/ratings/key_teams/by_season.RDS")
    all.vars <- colnames(match.stats)[c(3:17, 19:33)]
    all.home.vars <- all.vars[seq(from=1, to=length(all.vars), by=2)]
    all.away.vars <- all.vars[seq(from=2, to=length(all.vars), by=2)]
    
    # Fill in home ratings columns
    for (home.var.name in all.home.vars) {
      home.ratings.var.name <- paste0(home.var.name, "_ratings")
      match.stats[[home.ratings.var.name]] <- -10
      match.stats[[home.ratings.var.name]] <- unlist(map(
        match.stats$home_team, name.rating.mapper,
        mapper=all.seasons.ratings.data, rating.name=home.ratings.var.name, season=season),
        use.names = F)
    }
    # Fill in away ratings columns
    for (away.var.name in all.away.vars) {
      away.ratings.var.name <- paste0(away.var.name, "_ratings")
      match.stats[[away.ratings.var.name]] <- -10
      match.stats[[away.ratings.var.name]] <- unlist(map(
        match.stats$away_team, name.rating.mapper,
        mapper=all.seasons.ratings.data, rating.name=away.ratings.var.name, season=season),
        use.names = F)
    }
    
    return(match.stats)
  }
  #################################           Helper functions of evaluate.model           #################################
  ##########################################################################################################################
  
  # Attach the ratings data to the match.stats.data
  # simulations$match.stats <- attach.ratings.columns.to.match.stats.data(simulations$match.stats, season)

  # Create the match.results dataset for all games with nSims repetitions
  match.results.single <- as.data.frame(cbind(simulations$fixtures.team.names$home,
                                              simulations$fixtures.team.names$away))
  match.results <- match.results.single

  if (nSims > 1) {
    for (i in 1:(nSims-1)) {
      match.results <- rbind(match.results, match.results.single)
    }
  }
  colnames(match.results) <- c("home_team", "away_team")
  
  # TESTS TO SEE WHAT RESULTS IN THE BEST SIMULATION RESULTS
  # Get predictions of goals scored
  match.results$home_goals <- predict(random.forest.models$home,
                                      newdata=simulations$match.stats[, c(3:32)], type="response")
  match.results$away_goals <- predict(random.forest.models$away,
                                      newdata=simulations$match.stats[, c(3:32)], type="response")
  # TESTS TO SEE WHAT RESULTS IN THE BEST SIMULATION RESULTS

  # Evaluate results to get match outcome and score difference
  results <- get.result.from.score(match.results$home_goals, match.results$away_goals, threshold=draw.threshold)
  match.results$winner <- results$team.winner
  match.results$score_diff <- results$score_diff
  
  # Add back in the simulated match statistics along with the team ratings
  match.results <- cbind(match.results, simulations$match.stats[, 3:32])
  
  return(match.results)
  
  # evaluations <- get.evaluations(as.factor(match.results$team.winner), feature.set[,"result"])
  # score.diff.correlation <- cor(match.results$score.diff, (feature.set[,1] - feature.set[,2]))
  
  # return(list(predictions=predictions, match.results=match.results, score.diff.correlation=score.diff.correlation))
  # Got 74.86% correct predictions of match result this way.
  # 91.02% home wins correctly predicted. 36.51% draws predicted correctly. 80.23% away wins predicted.
  # Also got very high correlation of score.diff in the matches at around 86%.
  # Some incorrect predictions may be on the boundary.
  # Improvements over the last attempt.
}

get.league.standings <- function(predictions) {
  ##########################################################################################################################
  ####################################### Helper functions for: get.league.standings #######################################
  aggregate.simulations <- function(predictions) {
    # agg.winner1 <- predictions[, c("home_team", "away_team", "winner", "home_goals", "away_goals")]
    # mean.home.goals <- as.data.frame(aggregate(
    #   predictions[c("home_goals")], by=list(home_team=predictions$home_team, away_team=predictions$away_team), FUN=mean))
    # colnames(mean.home.goals) <- c("home_team", "away_team", "home_goals_avg")
    # mean.home.goals <- mean.home.goals[with(mean.home.goals, order(home_team, away_team)), ]
    # row.names(mean.home.goals) <- NULL
    # mean.away.goals <- as.data.frame(aggregate(
    #   predictions[c("away_goals")], by=list(home_team=predictions$home_team, away_team=predictions$away_team), FUN=mean))
    # colnames(mean.away.goals) <- c("home_team", "away_team", "away_goals_avg")
    # mean.away.goals <- mean.away.goals[with(mean.away.goals, order(home_team, away_team)), ]
    # row.names(mean.away.goals) <- NULL
    # agg.winner1 <- cbind(agg.winner1, mean.home.goals[, "home_goals_avg"], mean.away.goals[, "away_goals_avg"],
    #                      (mean.home.goals[, "home_goals_avg"]-mean.away.goals[, "away_goals_avg"]))
    # colnames(agg.winner1) <- c("home_team", "away_team", "winner", "home_goals", "away_goals",
    #                            "home_goals_avg", "away_goals_avg", "score_diff_avg")
    # agg.winner1 <- agg.winner1[1:380,]
    # agg.winner1$winner <- get.result.from.score(agg.winner1$home_goals_avg, agg.winner1$away_goals_avg, 0.45)$team.winner
    # agg.winner1$home <- ifelse(agg.winner1$winner == "home", 1, 0)
    # agg.winner1$draw <- ifelse(agg.winner1$winner == "draw", 1, 0)
    # agg.winner1$away <- ifelse(agg.winner1$winner == "away", 1, 0)
    # agg.winner1$played <- agg.winner1$home + agg.winner1$draw + agg.winner1$away
    # agg.winner1 <- get.points.for.teams(agg.winner1)
    # agg.winner1 <- agg.winner1[, c("home_team", "away_team", "home", "draw", "away", "winner", "played",
    #                                "home_goals_avg", "away_goals_avg", "score_diff_avg")]
    # agg.winner1 <- get.points.for.teams(agg.winner1)
    
    agg.winner <- predictions[, c("home_team", "away_team", "winner")]
    predictions$home_team <- as.character(predictions$home_team)
    predictions$away_team <- as.character(predictions$away_team)
    agg.winner$home <- ifelse(agg.winner$winner == "home", 1, 0)
    agg.winner$draw <- ifelse(agg.winner$winner == "draw", 1, 0)
    agg.winner$away <- ifelse(agg.winner$winner == "away", 1, 0)
    agg.winner <- reshape2::melt(agg.winner, id=c("home_team", "away_team"), measure.vars = c("home", "draw", "away"),
                                 value.name = "agg_result")
    agg.winner <- reshape2::dcast(agg.winner, home_team + away_team ~ variable,
                                  fun.aggregate = sum, value.var = "agg_result")
    agg.winner$played <- agg.winner$home + agg.winner$draw + agg.winner$away

    mean.home.goals <- as.data.frame(aggregate(
      predictions[c("home_goals")], by=list(home_team=predictions$home_team, away_team=predictions$away_team), FUN=mean))
    colnames(mean.home.goals) <- c("home_team", "away_team", "home_goals_avg")
    mean.home.goals <- mean.home.goals[with(mean.home.goals, order(home_team, away_team)), ]
    row.names(mean.home.goals) <- NULL
    mean.away.goals <- as.data.frame(aggregate(
      predictions[c("away_goals")], by=list(home_team=predictions$home_team, away_team=predictions$away_team), FUN=mean))
    colnames(mean.away.goals) <- c("home_team", "away_team", "away_goals_avg")
    mean.away.goals <- mean.away.goals[with(mean.away.goals, order(home_team, away_team)), ]
    row.names(mean.away.goals) <- NULL
    mean.score.diff <- as.data.frame(aggregate(
      predictions[c("score_diff")], by=list(home_team=predictions$home_team, away_team=predictions$away_team), FUN=mean))
    colnames(mean.score.diff) <- c("home_team", "away_team", "score_diff_avg")
    mean.score.diff <- mean.score.diff[with(mean.score.diff, order(home_team, away_team)), ]
    row.names(mean.score.diff) <- NULL

    agg.winner <- cbind(agg.winner, mean.home.goals[, "home_goals_avg"], mean.away.goals[, "away_goals_avg"],
                        mean.score.diff[, "score_diff_avg"])
    colnames(agg.winner) <- c("home_team", "away_team", "home", "draw", "away", "played",
                              "home_goals_avg", "away_goals_avg", "score_diff_avg")

    agg.winner$home.pct <- agg.winner$home/agg.winner$played
    agg.winner$draw.pct <- agg.winner$draw/agg.winner$played
    agg.winner$away.pct <- agg.winner$away/agg.winner$played
    agg.winner$winner <- colnames(agg.winner[, c("home", "draw", "away")])[
      max.col(agg.winner[, c("home", "draw", "away")],ties.method="first")]
    agg.winner <- get.points.for.teams(agg.winner)
    
    return(agg.winner)
  }
  
  get.aggregated.league.table <- function(predictions, home.columns, away.columns) {
    
    home.table <- aggregate(predictions[, home.columns],
                            by=list(home_team=predictions$home_team), FUN=sum)
    colnames(home.table) <- c("team", "points", "goals_scored", "goals_conceded")
    home.table$goal_difference <- home.table$goals_scored - home.table$goals_conceded
    
    away.table <- aggregate(predictions[, away.columns],
                            by=list(away_team=predictions$away_team), FUN=sum)
    colnames(away.table) <- c("team", "points", "goals_scored", "goals_conceded")
    away.table$goal_difference <- away.table$goals_scored - away.table$goals_conceded
    
    league.table <- as.data.frame(home.table$team)
    colnames(league.table) <- c("team")
    league.table$points <- home.table$points + away.table$points
    league.table$goals_scored <- home.table$goals_scored + away.table$goals_scored
    league.table$goals_conceded <- home.table$goals_conceded + away.table$goals_conceded
    league.table$goal_difference <- home.table$goal_difference + away.table$goal_difference
    
    home.table <- home.table[order(home.table$points, decreasing = T),]
    rownames(home.table) <- NULL
    home.table$position <- rownames(home.table)
    home.table$team <- as.character(home.table$team)
    
    away.table <- away.table[order(away.table$points, decreasing = T),]
    rownames(away.table) <- NULL
    away.table$position <- rownames(away.table)
    away.table$team <- as.character(away.table$team)
    
    league.table <- league.table[order(league.table$points, decreasing = T),]
    rownames(league.table) <- NULL
    league.table$position <- rownames(league.table)
    league.table$team <- as.character(league.table$team)
    
    return(list(league.table=league.table, home.table=home.table, away.table=away.table))
  }
  
  get.points.for.teams <- function(predictions) {
    predictions$home_points <- ifelse(predictions$winner == "home", 3, ifelse(predictions$winner == "draw", 1, 0))
    predictions$away_points <- ifelse(predictions$winner == "away", 3, ifelse(predictions$winner == "draw", 1, 0))
    
    return(predictions)
  }
  ####################################### Helper functions for: get.league.standings #######################################
  ##########################################################################################################################
  
  results <- list(agg.by.mean=list(), agg.by.sum=list())
  a.predictions <- aggregate.simulations(predictions)
  results$agg.by.mean <- get.aggregated.league.table(a.predictions,
                                                     c("home_points", "home_goals_avg", "away_goals_avg"),
                                                     c("away_points", "away_goals_avg", "home_goals_avg"))
  results$agg.by.mean$result.predictions <- a.predictions
  
  
  a.predictions <- get.points.for.teams(predictions)
  a.predictions <- aggregate(a.predictions[c("home_points", "away_points", "home_goals", "away_goals", "score_diff")],
                               by=list(home_team=a.predictions$home_team, away_team=a.predictions$away_team), FUN=sum)
  results$agg.by.sum <- get.aggregated.league.table(a.predictions,
                                                    c("home_points", "home_goals", "away_goals"),
                                                    c("away_points", "away_goals", "home_goals"))
  results$agg.by.sum$result.predictions <- a.predictions
  
  return(results)
}

###################################################### Main functions ######################################################
############################################################################################################################

############################################################################################################################
################################## Helper functions for: get.all.fixtures.sim.match.stats ##################################

get.all.fixtures.from.league.season <- function(league.name.or.teams, season) {
  ##########################################################################################################################
  ###########################           Helpers of get.all.fixtures.from.league.season           ###########################
  get.team.info <- function(league.name.or.teams, season) {
    
    get.id <- function(name, name.id.mapper) {
      return(name.id.mapper[[str_replace_all(name, "\\.", " ")]])
    }
    
    bugs.data <- rjson::fromJSON(
      file="bugs/data/clean/bugs_model_data_dict_seasons_split.json")
    name.id.mapper <- bugs.data[[as.character(season)]]$team_id_mapper
    
    team.info <- list()
    if (length(league.name.or.teams) == 1) {
      if (league.name.or.teams == "Super League") {
        league.name.or.teams <- c("Premier League", "Ligue 1", "Bundesliga 1", "Serie A", "Primera Division")
      }
      match.data <- read.csv(
        "data/csv_of_json_data/clean/match_stats.csv")
      match.data <- match.data[match.data$league_name %in% league.name.or.teams,]
      match.data <- match.data[match.data$league_season == season,]
      match.data$home_team_name <- as.character(match.data$home_team_name)
      match.data$away_team_name <- as.character(match.data$away_team_name)
      
      team.info$teams <- unlist(list(sort(unique(match.data$home_team_name), decreasing=F)), use.names = F)
      
      if (!(length(team.info$teams) == length(unlist(unique(match.data$away_team_name))))) {
        print(team.info$teams)
        print(length(unique(match.data$away_team_name)))
        stop("Home and away teams lists do not match. Investigate!")
      }
    } else {
      # Note that league.name.or.teams here is actually a vector of teams
      team.info$teams <- league.name.or.teams
    }
    team.info$team.ids <- unlist(map(team.info$teams, get.id, name.id.mapper=name.id.mapper), use.names = F)
    
    return(team.info)
  }
  
  get.all.fixtures.from.teams.selected <- function(team.info) {
    fixtures.team.ids <- as.data.frame(gtools::permutations(n=length(team.info$team.ids), r=2, v=team.info$team.ids))
    colnames(fixtures.team.ids) <- c("home", "away")
    
    fixtures.team.names <- as.data.frame(gtools::permutations(n=length(team.info$teams), r=2, v=team.info$teams))
    
    colnames(fixtures.team.names) <- c("home", "away")
    fixtures.team.names$home <- as.character(fixtures.team.names$home)
    fixtures.team.names$away <- as.character(fixtures.team.names$away)
    
    return(list(fixtures.team.ids=fixtures.team.ids, fixtures.team.names=fixtures.team.names, team.info=team.info))
  }
  ###########################           Helpers of get.all.fixtures.from.league.season           ###########################
  ##########################################################################################################################
  
  team.info <- get.team.info(league.name.or.teams, season)
  
  fixtures <- get.all.fixtures.from.teams.selected(team.info)
  
  return(fixtures)
}

sim.match.events <- function(nSims, home.ids.vector, away.ids.vector, season) {
  
  get.teams.param <- function(team.id, params.vector) {
    return(params.vector[team.id])
  }
  
  all.vars <- list(beta=list(c("passes_pct", "possession")),
                   gamma=list(c("passes_tot")),
                   nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl",
                                 "shots_outb", "offsides", "corners", "fouls", "gksaves")),
                   poisson=list(c("yc", "rc")))

  team.params <- readRDS(
    "R/all_data/main/params_means.RDS")

  home.team.stats <- data.frame(matrix(ncol=0, nrow=nSims*length(home.ids.vector)))
  away.team.stats <- data.frame(matrix(ncol=0, nrow=nSims*length(home.ids.vector)))
  season <- as.character(season)
  
  for (distribution in names(all.vars)) {
    for (distrib.list in all.vars[[distribution]]) {
      for (var.name in distrib.list){
        
        if (distribution == "beta") {
          
          home.alpha <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.alpha), use.names = F)
          home.beta <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.beta), use.names = F)
          home.team.stats[[var.name]] <- rbeta(nSims*length(home.ids.vector), home.alpha, home.beta)
          
          away.alpha <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.alpha), use.names = F)
          away.beta <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.beta), use.names = F)
          away.team.stats[[var.name]] <- rbeta(nSims*length(home.ids.vector), away.alpha, away.beta)
          
        } else if (distribution == "gamma") {
          
          home.shape <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.shape), use.names = F)
          home.rate <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.rate), use.names = F)
          home.team.stats[[var.name]] <- rgamma(nSims*length(home.ids.vector), shape=home.shape, rate=home.rate)
          
          away.shape <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.shape), use.names = F)
          away.rate <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.rate), use.names = F)
          away.team.stats[[var.name]] <- rgamma(nSims*length(home.ids.vector), shape=away.shape, rate=away.rate)
          
        } else if (distribution == "nbinom") {
          
          home.r <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.r), use.names = F)
          home.p <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.p), use.names = F)
          home.team.stats[[var.name]] <- rnbinom(nSims*length(home.ids.vector), size=home.r, prob=home.p)
          
          away.r <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.r), use.names = F)
          away.p <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.p), use.names = F)
          away.team.stats[[var.name]] <- rnbinom(nSims*length(home.ids.vector), size=away.r, prob=away.p)
          
        } else if (distribution == "poisson") {
          
          home.lambda <- unlist(map(
            home.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$home.lambda), use.names = F)
          home.team.stats[[var.name]] <- rpois(nSims*length(home.ids.vector), home.lambda)
          
          away.lambda <- unlist(map(
            away.ids.vector, get.teams.param, params.vector=team.params[[var.name]][[season]]$away.lambda), use.names = F)
          away.team.stats[[var.name]] <- rpois(nSims*length(home.ids.vector), away.lambda)
        }
      }
    }
  }
  # Possession
  total.possession <- home.team.stats$possession + away.team.stats$possession
  home.team.stats$possession <- home.team.stats$possession/total.possession
  away.team.stats$possession <- away.team.stats$possession/total.possession
  
  # Shots inside and outside of box
  home.total.box.shots <- home.team.stats$shots_inb + home.team.stats$shots_outb
  home.team.stats$shots_inb <- home.team.stats$shots_tot*(home.team.stats$shots_inb/home.total.box.shots)
  home.team.stats$shots_outb <- home.team.stats$shots_tot*(home.team.stats$shots_outb/home.total.box.shots)
  away.total.box.shots <- away.team.stats$shots_inb + away.team.stats$shots_outb
  away.team.stats$shots_inb <- away.team.stats$shots_tot*(away.team.stats$shots_inb/home.total.box.shots)
  away.team.stats$shots_outb <- away.team.stats$shots_tot*(away.team.stats$shots_outb/home.total.box.shots)
  
  home.team.stats$shots_inb[is.na(home.team.stats$shots_inb)] <- 0
  home.team.stats$shots_outb[is.na(home.team.stats$shots_outb)] <- 0
  away.team.stats$shots_inb[is.na(away.team.stats$shots_inb)] <- 0
  away.team.stats$shots_outb[is.na(away.team.stats$shots_outb)] <- 0
  
  home.team.stats$shots_inb[is.infinite(home.team.stats$shots_inb)] <- 0
  home.team.stats$shots_outb[is.infinite(home.team.stats$shots_outb)] <- 0
  away.team.stats$shots_inb[is.infinite(away.team.stats$shots_inb)] <- 0
  away.team.stats$shots_outb[is.infinite(away.team.stats$shots_outb)] <- 0
  
  # Shots accuracy totalling
  home.total.acc.shots <- home.team.stats$shots_ont + home.team.stats$shots_offt + home.team.stats$shots_bl
  home.team.stats$shots_ont <- home.team.stats$shots_tot*(home.team.stats$shots_ont/home.total.acc.shots)
  home.team.stats$shots_offt <- home.team.stats$shots_tot*(home.team.stats$shots_offt/home.total.acc.shots)
  home.team.stats$shots_bl <- home.team.stats$shots_tot*(home.team.stats$shots_bl/home.total.acc.shots)
  away.total.acc.shots <- away.team.stats$shots_ont + away.team.stats$shots_offt + away.team.stats$shots_bl
  away.team.stats$shots_ont <- away.team.stats$shots_tot*(away.team.stats$shots_ont/away.total.acc.shots)
  away.team.stats$shots_offt <- away.team.stats$shots_tot*(away.team.stats$shots_offt/away.total.acc.shots)
  away.team.stats$shots_bl <- away.team.stats$shots_tot*(away.team.stats$shots_bl/away.total.acc.shots)
  
  home.team.stats$shots_ont[is.na(home.team.stats$shots_ont)] <- 0
  home.team.stats$shots_offt[is.na(home.team.stats$shots_offt)] <- 0
  home.team.stats$shots_bl[is.na(home.team.stats$shots_bl)] <- 0
  away.team.stats$shots_ont[is.na(away.team.stats$shots_ont)] <- 0
  away.team.stats$shots_offt[is.na(away.team.stats$shots_offt)] <- 0
  away.team.stats$shots_bl[is.na(away.team.stats$shots_bl)] <- 0
  
  home.team.stats$shots_ont[is.infinite(home.team.stats$shots_ont)] <- 0
  home.team.stats$shots_offt[is.infinite(home.team.stats$shots_offt)] <- 0
  home.team.stats$shots_bl[is.infinite(home.team.stats$shots_bl)] <- 0
  away.team.stats$shots_ont[is.infinite(away.team.stats$shots_ont)] <- 0
  away.team.stats$shots_offt[is.infinite(away.team.stats$shots_offt)] <- 0
  away.team.stats$shots_bl[is.infinite(away.team.stats$shots_bl)] <- 0
  
  return(match.stats = list(home.team.stats=home.team.stats, away.team.stats=away.team.stats))
}
################################## Helper functions for: get.all.fixtures.sim.match.stats ##################################
############################################################################################################################

############################################################################################################################
########################################### Helper functions for: evaluate.model ###########################################
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
########################################### Helper functions for: evaluate.model ###########################################
############################################################################################################################

############################################################################################################################
################################################### Validation functions ###################################################
check.simulation.averaging.is.done.correctly <- function(match.index, nSims, predictions, mean.home.goals, mean.away.goals) {
  sim.predictions <- predictions[seq(from=match.index, to=380*nSims, by=380),
                                 c("home_team", "away_team", "home_goals", "away_goals", "winner")]
  if (mean(sim.predictions$home_goals) == mean.home.goals[match.index, "home_goals_avg"]) {
    print(paste("Average Home Goals:", mean.home.goals[match.index,]))
  } else {
    print("Difference:")
    print(mean.home.goals[match.index,])
    print(sim.predictions)
  }
  
  if (mean(sim.predictions$away_goals) == mean.away.goals[match.index, "away_goals_avg"]) {
    print(paste("Average Away Goals:", mean.away.goals[match.index,]))
  } else {
    print("Difference:")
    print(mean.away.goals[match.index,])
    print(sim.predictions)
  }
}
################################################### Validation functions ###################################################
############################################################################################################################
