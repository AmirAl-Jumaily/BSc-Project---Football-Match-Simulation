# NOTE: AS MENTIONED ELSEWHERE, THE BELOW WORK WAS NOT INCLUDED IN THE FINAL PROJECT DUE TO TIME AND WORD COUNT RESTRICTIONS.
# HOWEVER, THE BELOW FUNCTIONS AND SO THE CODE RELATING TO RATINGS CAN BE USED.

setwd("bugs/match_statistics/07")
library(R2OpenBUGS)
library(rjson)

source("R/utils/bugs.R")

filepath <- "bugs/data/clean/bugs_model_data_dict_seasons_split.json"
bugs.data.json <- fromJSON(file = filepath)
do.debug <- FALSE
nChains <- 2

cat("\n#################################################################################\n", file = "match_statistics/ratings/logs.txt", append=TRUE)
cat("Beginning MCMC runs to get ratings distributional parameter estimates\n\n", file = "match_statistics/ratings/logs.txt", append=TRUE)

for (distribution in names(all.vars)) {
  for (distrib.list in all.vars[[distribution]]) {
    for (var.name in distrib.list){
      params <- unlist(all.rating.params, use.names = FALSE)
      
      for (season in sort(names(bugs.data.json))) {
        nTeams <- get.nTeams(bugs.data.json, season)
        
        var.priors.dir <- paste0("match_statistics/ratings/", distribution, "/", var.name, "/")
        directory.to.save <- paste0(var.priors.dir, season, "/")
        dir.create(directory.to.save, recursive = TRUE, showWarnings = FALSE)
        model.file <- paste0(getwd(), "/match_statistics/ratings/ratings_model.txt")
        
        rating.data.file <- paste0(directory.to.save, "bugs_ratings_data.txt")
        bugs.results.file <- paste0(directory.to.save, "bugs_ratings_results.RDS")
        
        if (file.exists(bugs.results.file)) {
          print(paste0("Model results file already exists in: ", directory.to.save))
          next
        }

        ratings.data <- calculate.ratings.data(bugs.data.json, distribution, var.name, season, nTeams)
        
        prepare.ratings.data(rating.data.file, distribution, var.name, season, bugs.data.json, ratings.data, data.digits=4)
        prepare.ratings.inits(distribution, var.name, season, bugs.data.json, nChains, nTeams, inits.digits=5)
        
        init.files <- list.files(path=directory.to.save, pattern="bugs_ratings_inits")
        for (i in 1:length(init.files)) {
          init.files[i] <- paste0(getwd(), "/", directory.to.save, init.files[i])
        }
        
        print(paste0("Running ratings model for: ", distribution, " - ", var.name, " - ", season))
        print(Sys.time())
        cat(paste0("Running ratings model for: ", distribution, " - ", var.name, " - ", season), file="match_statistics/ratings/logs.txt", sep="\n", append=TRUE)
        cat(as.character(Sys.time()), file="match_statistics/ratings/logs.txt", sep="\n", append=TRUE)
        
        model <- bugs(data=rating.data.file, inits=init.files, parameters.to.save=params, model.file=model.file,
                      n.chains=length(init.files), n.iter=4000, n.burnin=2000, n.thin=1,
                      DIC=TRUE, debug=do.debug, working.directory=getwd())
        
        saveRDS(model, file = bugs.results.file)
        
        dir.create(paste0("match_statistics/ratings/", distribution, "/", var.name, "/", toString(as.integer(season)+1), "/"), recursive = TRUE, showWarnings = FALSE)
        
        for (i in 1:nChains) {
          extract.and.save.team.posteriors(model$sims.array, distribution, i, bugs.data.json, season, nTeams, directory.to.save)
          extract.and.save.hyperprior.posteriors(model$sims.array, distribution, i, season, directory.to.save)
        }
        
        cat(as.character(Sys.time()), file="match_statistics/ratings/logs.txt", sep="\n", append=TRUE)
        cat(paste0("Ratings model output saved for: ", distribution, " - ", var.name, " - ", season, "\n\n"), file="match_statistics/ratings/logs.txt", append=TRUE)
        print(Sys.time())
        print(paste0("Ratings model output saved for: ", distribution, " - ", var.name, " - ", season))
        
      }
    }
  }
}
