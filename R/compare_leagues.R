# setwd("")
source("R/utils/simulation.R")
source("R/utils/league_comparison.R")

#######################################################################################################
season <- "2017"
bugs.data <- fromJSON(file="bugs/data/clean/bugs_model_data_dict_seasons_split.json")
all.results <- run.simulations(1, "Super League", season)
league.table <- all.results$league.standings$agg.by.mean$league.table
name.country.mapper <- bugs.data[[season]]$team_country_mapper
league.table$team.country <- unlist(map(
  league.table$team, get.country, name.country.mapper=name.country.mapper), use.names = F)
#######################################################################################################

# Gives average position of teams from each country
position.summary <- get.league.position.summary(league.table)

# Split league table into 5 and a bit seperate pieces
split.leagues <- split.into.N.leagues(league.table, 5)

# Compare teams of two chosen leagues
league.comparison <- get.country.result.comparison("Italy", "Spain", all.results$match.predictions, name.country.mapper)

# Compare teams depending on expected league position
comparison.of.equal.levels <- compare.teams.of.equal.level(1, c("Premier League", "Ligue 1", "Serie A"), season, 5)
