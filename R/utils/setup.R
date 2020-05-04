library(purrr); library(plyr); library(dplyr)
library(R2OpenBUGS); library(coda)
library(stringr); library(rjson)
library(randomForest)

setwd("")

source("R/utils/bugs.R")
source("R/utils/modelling.R")
source("R/utils/simulation.R")

options(stringsAsFactors = FALSE)

run.setup <- function(verbose=TRUE) {
  #########################################################################################################################
  ############################################# Functions to perform on setup #############################################
  
  create.master.team.id.mappers <- function(all.teams) {
    # Get feature set to know teams for each season.
    feature.set <- read.csv(
      "data/csv_of_json_data/clean/match_stats.csv", stringsAsFactors = F)
    feature.set <- feature.set[(((feature.set$league_type == "League") & (feature.set$fixture_round != "Finals"))
                                | (feature.set$country == "World")),]
    feature.set <- feature.set[, c(2, 6, 17, 19)]
    
    # Get all teams across all seasons
    bugs.data <- fromJSON(
      file="bugs/data/clean/bugs_model_data_dict_seasons_combined.json")
    master.team.id.mapper <- as.data.frame(t(as.data.frame(bugs.data$team_id_mapper)))
    master.team.id.mapper$teams <- rownames(master.team.id.mapper)
    rownames(master.team.id.mapper) <- NULL
    master.team.id.mapper <- master.team.id.mapper[, 2:1]
    colnames(master.team.id.mapper) <- c("teams", "overall")
    master.team.id.mapper$teams[master.team.id.mapper$teams == "X1899.Hoffenheim"] <- "1899.Hoffenheim"
    
    # Get BUGS data split by season and create the team.id.mapper
    bugs.data.seasons <- fromJSON(
      file="bugs/data/clean/bugs_model_data_dict_seasons_split.json")
    for (season in sort(names(bugs.data.seasons))) {
      cols <- colnames(master.team.id.mapper)
      season.id.mapper <- as.data.frame(t(as.data.frame(bugs.data.seasons[[season]]$team_id_mapper)))
      season.id.mapper$teams <- rownames(season.id.mapper)
      season.id.mapper$teams[season.id.mapper$teams == "X1899.Hoffenheim"] <- "1899.Hoffenheim"
      
      if (all.teams) {
        seasons.teams <- unique(c(
          feature.set[feature.set$league_season == as.numeric(season), "home_team_name",],
          feature.set[feature.set$league_season == as.numeric(season), "away_team_name",]))
      } else {
        seasons.teams <- unique(c(
          feature.set[((feature.set$league_season == as.numeric(season)) & (feature.set$country != "World")), "home_team_name",],
          feature.set[((feature.set$league_season == as.numeric(season)) & (feature.set$country != "World")), "away_team_name",]))
      }
      
      seasons.teams <- str_replace_all(seasons.teams, " ", "\\.")
      season.id.mapper <- season.id.mapper[season.id.mapper$teams %in% seasons.teams,]
      
      master.team.id.mapper <- merge(master.team.id.mapper, season.id.mapper, by="teams", all.x=T)
      cols <- c(cols, as.character(season))
      colnames(master.team.id.mapper) <- cols
    }
    
    # Save the data
    dir.create("R/all_data", showWarnings=F)
    if (all.teams) {
      # Save a mapper for every team including those not in top 5 leagues
      saveRDS(master.team.id.mapper,
              "R/all_data/all_teams_master_team_id_mapper.RDS")
    } else {
      # Drop any teams that aren't from the top 5 leagues
      master.team.id.mapper <- master.team.id.mapper[rowSums(is.na(master.team.id.mapper)) < 4,]
      master.team.id.mapper <- master.team.id.mapper[order(master.team.id.mapper$overall),]
      rownames(master.team.id.mapper) <- NULL
      
      saveRDS(master.team.id.mapper, "R/all_data/master_team_id_mapper.RDS")
    }
  }
  
  create.team.params.files <- function(main.or.ratings, coda=FALSE) {
    seasons <- c("2016", "2017", "2018", "2019")
    dir.to.save.at <- "R/all_data"
    params.means <- list()
    summary.stats <- list()
    coda.mcmc.list <- list()
    
    if (main.or.ratings == "ratings") {
      bugs.results.root.dir <- "bugs/match_statistics/07/ratings"
      bugs.results.filename <- "bugs_ratings_results.RDS"
      all.vars <- list(
        nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl", "shots_outb",
                      "offsides", "corners", "fouls", "gksaves")),
        beta=list(c("passes_pct", "possession")),
        gamma=list(c("passes_tot", "passes_acc")),
        poisson=list(c("yc"))
      )
    } else {
      bugs.results.root.dir <- "bugs/match_statistics/07"
      bugs.results.filename <- "bugs_results.RDS"
      all.vars <- list(
        nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl", "shots_outb",
                      "offsides", "corners", "fouls", "gksaves")),
        beta=list(c("passes_pct", "possession")),
        gamma=list(c("passes_tot", "passes_acc")),
        poisson=list(c("yc", "rc"))
      )
    }
    
    for (distribution in names(all.vars)) {
      for (distrib.list in all.vars[[distribution]]) {
        for (var.name in distrib.list){
          for (season in seasons) {
            file.name <- paste(bugs.results.root.dir, distribution, var.name, season, bugs.results.filename, sep="/")
            bugs.output <- readRDS(file.name)
            
            params.means[[var.name]][[season]] <- data.frame(
              bugs.output$mean[(str_count(names(bugs.output$mean), "\\.")==1)])
            summary.stats[[var.name]][[season]] <- data.frame(bugs.output$summary)
            if (coda) {coda.mcmc.list[[var.name]][[season]] <- as.mcmc.list(bugs.output)}
          }
        }
      }
    }
    
    # Save the param values for use in simulation
    dir.create(paste(dir.to.save.at, main.or.ratings, sep="/"), showWarnings = F)
    saveRDS(params.means, paste(dir.to.save.at, main.or.ratings, "params_means.RDS", sep="/"))
    saveRDS(summary.stats, paste(dir.to.save.at, main.or.ratings, "summary_stats.RDS", sep="/"))
    if (coda) {saveRDS(coda.mcmc.list, paste(dir.to.save.at, main.or.ratings, "coda_mcmc_list.RDS", sep="/"))}
  }
  
  create.convergence.assessment.files <- function(main.or.ratings) {
    seasons <- c("2016", "2017", "2018", "2019")
    dir.to.save.at <- "R/all_data"
    summary.stats <- readRDS(paste(dir.to.save.at, main.or.ratings, "summary_stats.RDS", sep="/"))
    Rhat.list <- list()
    n.eff.list <- list()
    
    if (main.or.ratings == "ratings") {
      bugs.results.root.dir <- "bugs/match_statistics/07/ratings"
      bugs.results.filename <- "bugs_ratings_results.RDS"
      all.vars <- list(
        nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl", "shots_outb",
                      "offsides", "corners", "fouls", "gksaves")),
        beta=list(c("passes_pct", "possession")),
        gamma=list(c("passes_tot", "passes_acc")),
        poisson=list(c("yc"))
      )
    } else {
      bugs.results.root.dir <- "bugs/match_statistics/07"
      bugs.results.filename <- "bugs_results.RDS"
      all.vars <- list(
        nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl", "shots_outb",
                      "offsides", "corners", "fouls", "gksaves")),
        beta=list(c("passes_pct", "possession")),
        gamma=list(c("passes_tot", "passes_acc")),
        poisson=list(c("yc", "rc"))
      )
    }
    
    # Assess convergence and effective sample size
    for (distribution in names(all.vars)) {
      for (distrib.list in all.vars[[distribution]]) {
        for (var.name in distrib.list){
          for (season in seasons) {
            Rhat.list[[paste0(var.name, ".", season)]] <- (length(
              summary.stats[[var.name]][[season]]$Rhat[summary.stats[[var.name]][[season]]$Rhat<1.01])/
                length(summary.stats[[var.name]][[season]]$Rhat))
            n.eff.list[[paste0(var.name, ".", season)]] <- (length(
              summary.stats[[var.name]][[season]]$n.eff[summary.stats[[var.name]][[season]]$n.eff>1000])/
                length(summary.stats[[var.name]][[season]]$n.eff))
          }
        }
      }
    }
    
    Rhat.list <- round(t(as.data.frame(Rhat.list)), 3)
    n.eff.list <- round(t(as.data.frame(n.eff.list)), 3)
    colnames(Rhat.list) <- "Rhat.LT.1.01.pct"
    colnames(n.eff.list) <- "n.eff.GT.1000.pct"
    convergence.assessment <- cbind(Rhat.list, n.eff.list)
    
    # Save the param values for use in simulation
    dir.create(paste(dir.to.save.at, main.or.ratings, sep="/"), showWarnings = F)
    saveRDS(convergence.assessment, paste(dir.to.save.at, main.or.ratings, "convergence_assessment.RDS", sep="/"))
  }
  
  create.team.ratings.dataframes.for.all.seasons <- function(all.teams) {
    
    id.name.mapper <- function(id, mapper, season) {
      name <- mapper[mapper[,season]==id, "teams"]
      name <- ifelse(length(name) == 1, name, NA)
      return(name)
    }
    
    if (all.teams) {
      master.team.id.mapper <- readRDS(
        "R/all_data/all_teams_master_team_id_mapper.RDS")
    } else {
      master.team.id.mapper <- readRDS(
        "R/all_data/master_team_id_mapper.RDS")
    }
    
    team.ratings <- readRDS("R/all_data/ratings/params_means.RDS")
    seasons <- c("2016", "2017", "2018", "2019")
    all.seasons.ratings.data <- list()
    
    for (season in seasons) {
      season.team.id.mapper <- master.team.id.mapper[, c("teams", season)]
      season.team.id.mapper <- season.team.id.mapper[!is.na(season.team.id.mapper[, season]), ]
      
      season.ratings.data <- as.data.frame(season.team.id.mapper[, "teams"])
      colnames(season.ratings.data) <- c("team.name")
      
      for (var.name in names(team.ratings)) {
        var.ratings <- as.data.frame(team.ratings[[var.name]][[season]])
        var.ratings$team.id <- rownames(var.ratings)
        var.ratings$team.name <- unlist(map(
          var.ratings$team.id, id.name.mapper, mapper=season.team.id.mapper, season=season), use.names = F)
        var.ratings <- var.ratings[!is.na(var.ratings$team.name),]
        home.var.ratings <- var.ratings[, c("team.name", "homeRating.mu")]
        away.var.ratings <- var.ratings[, c("team.name", "awayRating.mu")]
        colnames(home.var.ratings) <- c("team.name", paste0("home_", var.name, "_ratings"))
        colnames(away.var.ratings) <- c("team.name", paste0("away_", var.name, "_ratings"))
        
        season.ratings.data <- merge(season.ratings.data, home.var.ratings, by="team.name", all.x=T)
        season.ratings.data <- merge(season.ratings.data, away.var.ratings, by="team.name", all.x=T)
      }
      
      all.seasons.ratings.data[[season]] <- season.ratings.data
    }
    
    if (all.teams) {
      dir.to.save.at <- paste0("R/all_data/ratings/all_teams")
    } else {
      dir.to.save.at <- paste0("R/all_data/ratings/key_teams")
    }
    
    dir.create(dir.to.save.at, showWarnings=F)
    saveRDS(all.seasons.ratings.data, paste(dir.to.save.at, "by_season.RDS", sep="/"))
  }
  
  create.final.feature.set.with.ratings.columns <- function() {
    
    name.rating.mapper <- function(team.name, mapper, rating.name, season) {
      season.mapper <- mapper[[season]]
      return.value <- season.mapper[season.mapper$team.name == team.name, rating.name]
      if (length(return.value)>1) {
        print(return.value)
        return(-1)
      } else {
        return(return.value)
      }
    }
    
    # Get feature set
    feature.set <- read.csv(
      "data/csv_of_json_data/clean/match_stats.csv", stringsAsFactors = F)
    feature.set <- feature.set[(((feature.set$league_type == "League") & (feature.set$fixture_round != "Finals"))
                                | (feature.set$country == "World")),]
    feature.set <- feature.set[, c(2, 6, 17, 19, 21:54)]
    
    feature.set$home_team_name <- str_replace_all(feature.set$home_team_name, " ", "\\.")
    feature.set$away_team_name <- str_replace_all(feature.set$away_team_name, " ", "\\.")
    
    all.seasons.ratings.data <- readRDS("R/all_data/ratings/all_teams/by_season.RDS")
    # name.rating.mapper("Arsenal", all.seasons.ratings.data, "home_shots_tot", "2018")
    all.vars <- colnames(feature.set)[c(7:34, 37:38)]
    all.home.vars <- all.vars[seq(from=1, to=length(all.vars), by=2)]
    all.away.vars <- all.vars[seq(from=2, to=length(all.vars), by=2)]
    seasons <- c("2016", "2017", "2018", "2019")
    # Fill in home ratings columns
    for (home.var.name in all.home.vars) {
      home.ratings.var.name <- paste0(home.var.name, "_ratings")
      feature.set[[home.ratings.var.name]] <- -10
      for (season in seasons) {
        season.feature.set <- feature.set[feature.set$league_season == as.numeric(season),]
        feature.set[feature.set$league_season == as.numeric(season),][[home.ratings.var.name]] <- unlist(map(
          season.feature.set$home_team_name, name.rating.mapper,
          mapper=all.seasons.ratings.data, rating.name=home.ratings.var.name, season=season),
          use.names = F)
      }
    }
    # Fill in away ratings columns
    for (away.var.name in all.away.vars) {
      away.ratings.var.name <- paste0(away.var.name, "_ratings")
      feature.set[[away.ratings.var.name]] <- -10
      for (season in seasons) {
        season.feature.set <- feature.set[feature.set$league_season == as.numeric(season),]
        feature.set[feature.set$league_season == as.numeric(season),][[away.ratings.var.name]] <- unlist(map(
          season.feature.set$away_team_name, name.rating.mapper,
          mapper=all.seasons.ratings.data, rating.name=away.ratings.var.name, season=season),
          use.names = F)
      }
    }
    
    saveRDS(feature.set, "R/all_data/feature_set.RDS")
  }
  
  create.random.forest.models <- function() {
    dir.to.save.at <- "R/all_data"
    
    # Get feature set
    feature.set <- readRDS("R/all_data/feature_set.RDS")
    feature.set <- feature.set[, c(-1:-4)]
    
    # TESTS TO SEE WHAT RESULTS IN THE BEST SIMULATION RESULTS
    # Just the stats without ratings
    feature.set <- feature.set[,c(1:14, 17:34)]
    # # Just their stats but both teams ratings
    # home.feature.set <- feature.set[, c(seq(from=1, to=34, by=2), 35:64)]
    # away.feature.set <- feature.set[, c(seq(from=2, to=34, by=2), 35:64)]
    # home.model <- randomForest(home_goals~., data=home.feature.set, ntree=1000)
    # away.model <- randomForest(away_goals~., data=away.feature.set, ntree=1000)
    # TESTS TO SEE WHAT RESULTS IN THE BEST SIMULATION RESULTS
    
    # Create models from feature set
    home.model <- randomForest(home_goals~., data=feature.set[, -2], ntree=1500, mtry=24, nodesize=15)
    away.model <- randomForest(away_goals~., data=feature.set[, -1], ntree=1500, mtry=24, nodesize=15)
    random.forest.models <- list(home=home.model, away=away.model)
    
    saveRDS(random.forest.models, paste(dir.to.save.at, "random_forest_models.RDS", sep="/"))
  }
  ############################################# Functions to perform on setup #############################################
  #########################################################################################################################
  
  # Note below that anything to do with ratings was commented out. This work was not included in the project due to time
  # and word count restrictions.
  #########################################################################################################################
  ####################################################### Run setup #######################################################
  # Create a dataframe that shows for every team their id in each season and overall, only returning teams from top 5 leagues
  create.master.team.id.mappers(all.teams = T)
  create.master.team.id.mappers(all.teams = F)
  if (verbose) {print(paste0("Created the master team id mappers: ", Sys.time()))}
  
  # Create files with team distirbutional parameter values as well as summary stats off the bugs run and coda files
  create.team.params.files("main")
  # create.team.params.files("ratings")
  if (verbose) {print(paste0("Created the team param files: ", Sys.time()))}
  
  # Create files that can be used to assess convergence of all of the parameters from the BUGS runs
  create.convergence.assessment.files("main")
  # create.convergence.assessment.files("ratings")
  if (verbose) {print(paste0("Created the convergence assessment files: ", Sys.time()))}
  
  # # Create files of the team ratings for each season
  # create.team.ratings.dataframes.for.all.seasons(all.teams = T)
  # create.team.ratings.dataframes.for.all.seasons(all.teams = F)
  # if (verbose) {print(paste0("Created the dataframes of team ratings for all seasons: ", Sys.time()))}
  
  # # Create final feature set that has all columns required to create random forest model
  # create.final.feature.set.with.ratings.columns()
  # if (verbose) {print(paste0("Created the final feature set: ", Sys.time()))}

  # Create the object that contains the random forest models for home and away teams
  create.random.forest.models()
  if (verbose) {print(paste0("Created the random forest models: ", Sys.time()))}
  if (verbose) {print("SETUP COMPLETE!")}
  ####################################################### Run setup #######################################################
  #########################################################################################################################
}
