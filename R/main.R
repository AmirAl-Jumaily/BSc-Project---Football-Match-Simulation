source("R/utils/setup.R")

#########################################################################################################
############################################ USER INPUT HERE ############################################
has.run.setup <- FALSE
nSims <- 200
league.name.or.teams <- ""
season <- "2018"
############################################ USER INPUT HERE ############################################
#########################################################################################################

# Perform necessary setup
if (!has.run.setup) {
  run.setup()
}

# Run simulations
pl.simulation.results <- run.simulations(nSims, "Premier League", season, draw.threshold = 0.01)
fl.simulation.results <- run.simulations(nSims, "Ligue 1", season, draw.threshold = 0.45)
gl.simulation.results <- run.simulations(nSims, "Bundesliga 1", season, draw.threshold = 0.45)
il.simulation.results <- run.simulations(nSims, "Serie A", season, draw.threshold = 0.45)
sl.simulation.results <- run.simulations(nSims, "Primera Division", season, draw.threshold = 0.45)

# Get league comparison statistics
super.league.teams <- c("Atalanta", "Arsenal", "Barcelona", "Manchester.City", "Paris.Saint.Germain", "Bayern.Munich",
                        "Napoli", "Liverpool", "Lyon", "Bayer.Leverkusen", "Real.Madrid", "Montpellier", "VfL.Wolfsburg",
                        "AS.Roma", "Sevilla", "Leicester", "Rennes", "Eintracht.Frankfurt", "Torino", "Espanyol",
                        "Watford", "Strasbourg", "Fortuna.Dusseldorf", "Sassuolo", "Eibar", "Burnley", "Monaco",
                        "VfB.Stuttgart", "Valladolid", "Genoa")

super.league.simulation.results <- run.simulations(nSims, super.league.teams, season, draw.threshold = 0.45)
complete.super.league.simulation.results <- run.simulations(nSims, "Super League", season, draw.threshold = 0.45)
