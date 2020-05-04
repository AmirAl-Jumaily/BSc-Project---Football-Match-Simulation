#######################################################################################################################################
################################# Generic functions used for both the main and ratings BUGS modelling #################################
extract.and.save.team.posteriors <- function(bugs.results, distribution, chain, bugs.data.json, season, nTeams, directory.to.save, ratings=F) {
  if ("sims.array" %in% names(bugs.results)) {bugs.results <- bugs.results$sims.array}
  file.to.save.at <- paste0(directory.to.save, "team_posteriors", i, ".RDS")
  
  if (ratings) {dist.team.params <- unlist(all.team.rating.params)}
  else {dist.team.params <- unlist(all.team.params[[distribution]])}
  
  this.seasons.teams <- sort(rownames(t(as.data.frame(bugs.data.json[[season]]$team_id_mapper))))
  this.seasons.teams[this.seasons.teams == 'X1899.Hoffenheim'] <- '1899.Hoffenheim'
  this.seasons.teams <- matrix(data=this.seasons.teams, nrow=length(this.seasons.teams))
  colnames(this.seasons.teams) <- "teams"
  
  team.posterior.means <- as.data.frame(matrix(data=this.seasons.teams, nrow = nTeams))
  
  for (team.prior in dist.team.params) {
    posterior.mean <- matrix(unname(colMeans(as.data.frame(bugs.results[, chain, ]) %>% select(starts_with(paste0(team.prior, "["))))), nrow=nTeams)
    colnames(posterior.mean) <- team.prior
    team.posterior.means <- as.data.frame(cbind(team.posterior.means, posterior.mean))
  }
  
  team.posterior.means <- team.posterior.means[, 2:(length(dist.team.params)+1)]
  
  saveRDS(team.posterior.means, file = file.to.save.at)
}


extract.and.save.hyperprior.posteriors <- function(bugs.results, distribution, chain, season, directory.to.save, ratings=F) {
  if ("sims.array" %in% names(bugs.results)) {bugs.results <- bugs.results$sims.array}
  file.to.save.at <- paste0(directory.to.save, "hyperprior_posteriors", i, ".RDS")
  
  if (ratings) {dist.hyper.params <- unlist(all.hyperprior.rating.params)}
  else {dist.hyper.params <- unlist(all.hyperprior.params[[distribution]])}
  
  hyperprior.posterior.means <- list()
  
  for (hyperprior in dist.hyper.params) {
    hyperprior.posterior.means[[hyperprior]] <- mean(bugs.results[, chain, hyperprior])
  }
  
  saveRDS(as.data.frame(hyperprior.posterior.means), file = file.to.save.at)
}


get.nTeams <- function(bugs.data.json, season) {
  return(bugs.data.json[[season]]$num_teams)
}
################################# Generic functions used for both the main and ratings BUGS modelling #################################
#######################################################################################################################################

#######################################################################################################################################
#################################################### Main BUGS modelling functions ####################################################
prepare_bugs_data <- function(data.file, distribution, var.name, season, bugs.data.json, data.digits=4) {
  if (!(distribution %in% c("beta", "gamma", "nbinom", "poisson"))) {
    stop("'Distribution' must be one of: 'beta', 'gamma', 'nbinom', 'poisson'.")
  }
  directory.to.save.at <- paste0(distribution, "/", var.name, "/", season, "/")
  season.data <- bugs.data.json[[season]]
  
  nGames <- season.data$num_matches
  nTeams <- season.data$num_teams
  HomeT <- season.data$home_team_game_index
  AwayT <- season.data$away_team_game_index
  
  home.data <- unlist(season.data$home_match_stats[[var.name]], use.names = F)
  away.data <- unlist(season.data$away_match_stats[[var.name]], use.names = F)
  
  data = list(
    nGames=nGames,
    nTeams=nTeams,
    HomeT=HomeT,
    AwayT=AwayT,
    home.data=home.data,
    away.data=away.data,
    homeOpp.data=away.data,
    awayOpp.data=home.data
  )
  
  bugs.data(data, dir=getwd(), digits=data.digits, data.file = data.file)
}


prepare_bugs_inits <- function(distribution, var.name, season, bugs.data.json, nChains, nTeams, inits.digits=5) {
  if (distribution == "beta") {
    inits <- gen.beta.inits.N.chains(nChains, nTeams)
  } else if (distribution == "gamma") {
    inits <- gen.gamma.inits.N.chains(nChains, nTeams)
  } else if (distribution == "nbinom") {
    inits <- gen.nbinom.inits.N.chains(nChains, nTeams)
  } else if (distribution == "poisson") {
    inits <- gen.poisson.inits.N.chains(nChains, nTeams)
  } else {
    stop("'Distribution' must be one of: 'beta', 'gamma', 'nbinom', 'poisson'.")
  }
  
  directory.to.save <- paste0("match_statistics/", distribution, "/", var.name, "/", season, "/")
  bugs.inits(inits=inits, n.chains=nChains, digits=inits.digits,
             inits.files=paste0(directory.to.save, "bugs_inits", 1:nChains, ".txt"))
}


gen.beta.inits.N.chains <- function(nChains, nTeams, minu=0.01, maxu=30) {
  init.values <- vector("list", nChains)
  for (i in 1:nChains) {
    chain.inits <- list(
      home.alpha = runif(nTeams, min=minu, max=maxu),
      home.alpha.shape = runif(1, min=minu, max=maxu),
      home.alpha.rate = runif(1, min=minu, max=maxu),
      
      home.beta = runif(nTeams, min=minu, max=maxu),
      home.beta.shape = runif(1, min=minu, max=maxu),
      home.beta.rate = runif(1, min=minu, max=maxu),
      
      away.alpha = runif(nTeams, min=minu, max=maxu),
      away.alpha.shape = runif(1, min=minu, max=maxu),
      away.alpha.rate = runif(1, min=minu, max=maxu),
      
      away.beta = runif(nTeams, min=minu, max=maxu),
      away.beta.shape = runif(1, min=minu, max=maxu),
      away.beta.rate = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.alpha = runif(nTeams, min=minu, max=maxu),
      againstHomeTeam.alpha.shape = runif(1, min=minu, max=maxu),
      againstHomeTeam.alpha.rate = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.beta = runif(nTeams, min=minu, max=maxu),
      againstHomeTeam.beta.shape = runif(1, min=minu, max=maxu),
      againstHomeTeam.beta.rate = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.alpha = runif(nTeams, min=minu, max=maxu),
      againstAwayTeam.alpha.shape = runif(1, min=minu, max=maxu),
      againstAwayTeam.alpha.rate = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.beta = runif(nTeams, min=minu, max=maxu),
      againstAwayTeam.beta.shape = runif(1, min=minu, max=maxu),
      againstAwayTeam.beta.rate = runif(1, min=minu, max=maxu)
    )
    init.values[[i]] = chain.inits
  }
  return(init.values)
}


gen.gamma.inits.N.chains <- function(nChains, nTeams, minu=0.01, maxu=30) {
  init.values <- vector("list", nChains)
  for (i in 1:nChains) {
    chain.inits <- list(
      home.shape = runif(nTeams, min=minu, max=maxu),
      home.shape.shape = runif(1, min=minu, max=maxu),
      home.shape.rate = runif(1, min=minu, max=maxu),
      
      home.rate = runif(nTeams, min=minu, max=maxu),
      home.rate.shape = runif(1, min=minu, max=maxu),
      home.rate.rate = runif(1, min=minu, max=maxu),
      
      away.shape = runif(nTeams, min=minu, max=maxu),
      away.shape.shape = runif(1, min=minu, max=maxu),
      away.shape.rate = runif(1, min=minu, max=maxu),
      
      away.rate = runif(nTeams, min=minu, max=maxu),
      away.rate.shape = runif(1, min=minu, max=maxu),
      away.rate.rate = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.shape = runif(nTeams, min=minu, max=maxu),
      againstHomeTeam.shape.shape = runif(1, min=minu, max=maxu),
      againstHomeTeam.shape.rate = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.rate = runif(nTeams, min=minu, max=maxu),
      againstHomeTeam.rate.shape = runif(1, min=minu, max=maxu),
      againstHomeTeam.rate.rate = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.shape = runif(nTeams, min=minu, max=maxu),
      againstAwayTeam.shape.shape = runif(1, min=minu, max=maxu),
      againstAwayTeam.shape.rate = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.rate = runif(nTeams, min=minu, max=maxu),
      againstAwayTeam.rate.shape = runif(1, min=minu, max=maxu),
      againstAwayTeam.rate.rate = runif(1, min=minu, max=maxu)
    )
    init.values[[i]] = chain.inits
  }
  return(init.values)
}


gen.nbinom.inits.N.chains <- function(nChains, nTeams, minu=0.01, maxu=30, minp=0.3, maxp=1, minr=5, maxr=30) {
  init.values <- vector("list", nChains)
  for (i in 1:nChains) {
    chain.inits <- list(
      home.p = runif(nTeams, min=minp, max=maxp),
      home.p.alpha = runif(1, min=minu, max=maxu),
      home.p.beta = runif(1, min=minu, max=maxu),
      
      home.r = as.integer(runif(nTeams, min=minr, max=maxr)),
      home.r.lambda = runif(1, min=minu, max=maxu),
      
      away.p = runif(nTeams, min=minp, max=maxp),
      away.p.alpha = runif(1, min=minu, max=maxu),
      away.p.beta = runif(1, min=minu, max=maxu),
      
      away.r = as.integer(runif(nTeams, min=minr, max=maxr)),
      away.r.lambda = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.p = runif(nTeams, min=minp, max=maxp),
      againstHomeTeam.p.alpha = runif(1, min=minu, max=maxu),
      againstHomeTeam.p.beta = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.r = as.integer(runif(nTeams, min=minr, max=maxr)),
      againstHomeTeam.r.lambda = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.p = runif(nTeams, min=minp, max=maxp),
      againstAwayTeam.p.alpha = runif(1, min=minu, max=maxu),
      againstAwayTeam.p.beta = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.r = as.integer(runif(nTeams, min=minr, max=maxr)),
      againstAwayTeam.r.lambda = runif(1, min=minu, max=maxu)
    )
    init.values[[i]] = chain.inits
  }
  return(init.values)
}


gen.poisson.inits.N.chains <- function(nChains, nTeams, minu=0.01, maxu=10) {
  init.values <- vector("list", nChains)
  for (i in 1:nChains) {
    chain.inits <- list(
      home.lambda = runif(nTeams, min=minu, max=maxu),
      home.lambda.shape = runif(1, min=minu, max=maxu),
      home.lambda.rate = runif(1, min=minu, max=maxu),
      
      away.lambda = runif(nTeams, min=minu, max=maxu),
      away.lambda.shape = runif(1, min=minu, max=maxu),
      away.lambda.rate = runif(1, min=minu, max=maxu),
      
      againstHomeTeam.lambda = runif(nTeams, min=minu, max=maxu),
      againstHomeTeam.lambda.shape = runif(1, min=minu, max=maxu),
      againstHomeTeam.lambda.rate = runif(1, min=minu, max=maxu),
      
      againstAwayTeam.lambda = runif(nTeams, min=minu, max=maxu),
      againstAwayTeam.lambda.shape = runif(1, min=minu, max=maxu),
      againstAwayTeam.lambda.rate = runif(1, min=minu, max=maxu)
    )
    init.values[[i]] = chain.inits
  }
  return(init.values)
}


all.vars <- list(
  nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl", "shots_outb", "offsides", "corners", "fouls", "gksaves")),
  beta=list(c("passes_pct", "possession")),
  gamma=list(c("passes_tot", "passes_acc")),
  poisson=list(c("yc", "rc"))
)

all.team.params <- list(
  beta=list(c(
    "home.alpha", "home.beta",
    "away.alpha", "away.beta",
    "againstHomeTeam.alpha", "againstHomeTeam.beta",
    "againstAwayTeam.alpha", "againstAwayTeam.beta"
  )),
  gamma=list(c(
    "home.shape", "home.rate",
    "away.shape", "away.rate",
    "againstHomeTeam.shape", "againstHomeTeam.rate",
    "againstAwayTeam.shape", "againstAwayTeam.rate"
  )),
  nbinom=list(c(
    "home.p", "home.r",
    "away.p", "away.r",
    "againstHomeTeam.p", "againstHomeTeam.r",
    "againstAwayTeam.p", "againstAwayTeam.r"
  )),
  poisson=list(c(
    "home.lambda", "away.lambda",
    "againstHomeTeam.lambda", "againstAwayTeam.lambda"
  ))
)

all.hyperprior.params <- list(
  beta=list(c(
    "home.alpha.shape", "home.alpha.rate",
    "home.beta.shape", "home.beta.rate",
    "away.alpha.shape", "away.alpha.rate",
    "away.beta.shape", "away.beta.rate",
    
    "againstHomeTeam.alpha.shape", "againstHomeTeam.alpha.rate",
    "againstHomeTeam.beta.shape", "againstHomeTeam.beta.rate",
    "againstAwayTeam.alpha.shape", "againstAwayTeam.alpha.rate",
    "againstAwayTeam.beta.shape", "againstAwayTeam.beta.rate"
  )),
  gamma=list(c(
    "home.shape.shape", "home.shape.rate",
    "home.rate.shape", "home.rate.rate",
    "away.shape.shape", "away.shape.rate",
    "away.rate.shape", "away.rate.rate",
    
    "againstHomeTeam.shape.shape", "againstHomeTeam.shape.rate",
    "againstHomeTeam.rate.shape", "againstHomeTeam.rate.rate",
    "againstAwayTeam.shape.shape", "againstAwayTeam.shape.rate",
    "againstAwayTeam.rate.shape", "againstAwayTeam.rate.rate"
  )),
  nbinom=list(c(
    "home.p.alpha", "home.p.beta",
    "home.r.lambda",
    "away.p.alpha", "away.p.beta",
    "away.r.lambda",
    
    "againstHomeTeam.p.alpha", "againstHomeTeam.p.beta",
    "againstHomeTeam.r.lambda",
    "againstAwayTeam.p.alpha", "againstAwayTeam.p.beta",
    "againstAwayTeam.r.lambda"
  )),
  poisson=list(c(
    "home.lambda.shape", "home.lambda.rate",
    "away.lambda.shape", "away.lambda.rate",
    
    "againstHomeTeam.lambda.shape", "againstHomeTeam.lambda.rate",
    "againstAwayTeam.lambda.shape", "againstAwayTeam.lambda.rate"
  ))
)

all.params <- list(
  beta=list(c(
    "home.alpha", "home.alpha.shape", "home.alpha.rate",
    "home.beta", "home.beta.shape", "home.beta.rate",
    "away.alpha", "away.alpha.shape", "away.alpha.rate",
    "away.beta", "away.beta.shape", "away.beta.rate",
    
    "againstHomeTeam.alpha", "againstHomeTeam.alpha.shape", "againstHomeTeam.alpha.rate",
    "againstHomeTeam.beta", "againstHomeTeam.beta.shape", "againstHomeTeam.beta.rate",
    "againstAwayTeam.alpha", "againstAwayTeam.alpha.shape", "againstAwayTeam.alpha.rate",
    "againstAwayTeam.beta", "againstAwayTeam.beta.shape", "againstAwayTeam.beta.rate"
  )),
  gamma=list(c(
    "home.shape", "home.shape.shape", "home.shape.rate",
    "home.rate", "home.rate.shape", "home.rate.rate",
    "away.shape", "away.shape.shape", "away.shape.rate",
    "away.rate", "away.rate.shape", "away.rate.rate",
    
    "againstHomeTeam.shape", "againstHomeTeam.shape.shape", "againstHomeTeam.shape.rate",
    "againstHomeTeam.rate", "againstHomeTeam.rate.shape", "againstHomeTeam.rate.rate",
    "againstAwayTeam.shape", "againstAwayTeam.shape.shape", "againstAwayTeam.shape.rate",
    "againstAwayTeam.rate", "againstAwayTeam.rate.shape", "againstAwayTeam.rate.rate"
  )),
  nbinom=list(c(
    "home.p", "home.p.alpha", "home.p.beta",
    "home.r", "home.r.lambda",
    "away.p", "away.p.alpha", "away.p.beta",
    "away.r", "away.r.lambda",
    
    "againstHomeTeam.p", "againstHomeTeam.p.alpha", "againstHomeTeam.p.beta",
    "againstHomeTeam.r", "againstHomeTeam.r.lambda",
    "againstAwayTeam.p", "againstAwayTeam.p.alpha", "againstAwayTeam.p.beta",
    "againstAwayTeam.r", "againstAwayTeam.r.lambda"
  )),
  poisson=list(c(
    "home.lambda", "home.lambda.shape", "home.lambda.rate",
    "away.lambda", "away.lambda.shape", "away.lambda.rate",
    
    "againstHomeTeam.lambda", "againstHomeTeam.lambda.shape", "againstHomeTeam.lambda.rate",
    "againstAwayTeam.lambda", "againstAwayTeam.lambda.shape", "againstAwayTeam.lambda.rate"
  ))
)
#################################################### Main BUGS modelling functions ####################################################
#######################################################################################################################################

# The below section was not used in the report due to time and word limit contraints.
#######################################################################################################################################
################################################### Rating BUGS modelling functions ###################################################
# prepare.ratings.data <- function(rating.data.file, distribution, var.name, season, bugs.data.json, ratings.data, data.digits=4) {
#   if (!(distribution %in% c("beta", "gamma", "nbinom", "poisson"))) {
#     stop("'Distribution' must be one of: 'beta', 'gamma', 'nbinom', 'poisson'.")
#   }
#   directory.to.save.at <- paste0(distribution, "/", var.name, "/", season, "/")
#   season.data <- bugs.data.json[[season]]
  
#   nGames <- season.data$num_matches
#   nTeams <- season.data$num_teams
#   HomeT <- season.data$home_team_game_index
#   AwayT <- season.data$away_team_game_index
  
#   homeRating.data <- ratings.data[["homeRating.data"]]
#   awayRating.data <- ratings.data[["awayRating.data"]]
  
#   data = list(
#     nGames=nGames,
#     nTeams=nTeams,
#     HomeT=HomeT,
#     AwayT=AwayT,
#     homeRating.data=homeRating.data,
#     awayRating.data=awayRating.data
#   )
  
#   bugs.data(data, dir=getwd(), digits=data.digits, data.file = rating.data.file)
# }


# calculate.ratings.data <- function(bugs.data.json, distribution, var.name, season, nTeams) {
#   bugs.results.file <- paste0("match_statistics/", distribution, "/", var.name, "/", season, "/", "bugs_results.RDS")
#   bugs.results <- readRDS(bugs.results.file)
  
#   home.data <- unlist(bugs.data.json[[season]][["home_match_stats"]][[var.name]], use.names = F)
#   away.data <- unlist(bugs.data.json[[season]][["away_match_stats"]][[var.name]], use.names = F)
  
#   HomeT <- bugs.data.json[[season]][["home_team_game_index"]]
#   AwayT <- bugs.data.json[[season]][["away_team_game_index"]]
  
#   Tparams <- data.frame(bugs.results$mean[(str_count(names(bugs.results$mean), "\\.")==1)])
  
#   homeRating.data <- c()
#   awayRating.data <- c()
  
#   if (distribution == "beta") {
#     for (g in 1:(length(HomeT))) {
#       homeRating.data[g] <- (home.data[g]/(Tparams[["againstAwayTeam.alpha"]][AwayT[g]]/(Tparams[["againstAwayTeam.alpha"]][AwayT[g]] + Tparams[["againstAwayTeam.beta"]][AwayT[g]])))
#       awayRating.data[g] <- (away.data[g]/(Tparams[["againstHomeTeam.alpha"]][HomeT[g]]/(Tparams[["againstHomeTeam.alpha"]][HomeT[g]] + Tparams[["againstHomeTeam.beta"]][HomeT[g]])))
#     }
    
#   } else if (distribution == "gamma") {
#     for (g in 1:(length(HomeT))) {
#       homeRating.data[g] <- (home.data[g]/(Tparams[["againstAwayTeam.shape"]][AwayT[g]]/Tparams[["againstAwayTeam.rate"]][AwayT[g]]))
#       awayRating.data[g] <- (away.data[g]/(Tparams[["againstHomeTeam.shape"]][HomeT[g]]/Tparams[["againstHomeTeam.rate"]][HomeT[g]]))
#     }
    
#   } else if (distribution == "nbinom") {
#     for (g in 1:(length(HomeT))) {
#       homeRating.data[g] <- (home.data[g]/((Tparams[["againstAwayTeam.r"]][AwayT[g]]*(1 - Tparams[["againstAwayTeam.p"]][AwayT[g]]))/Tparams[["againstAwayTeam.p"]][AwayT[g]]))
#       awayRating.data[g] <- (away.data[g]/((Tparams[["againstHomeTeam.r"]][HomeT[g]]*(1 - Tparams[["againstHomeTeam.p"]][HomeT[g]]))/Tparams[["againstHomeTeam.p"]][HomeT[g]]))
#     }
    
#   } else if (distribution == "poisson") {
#     for (g in 1:(length(HomeT))) {
#       homeRating.data[g] <- (home.data[g]/(Tparams[["againstAwayTeam.lambda"]][AwayT[g]]))
#       awayRating.data[g] <- (away.data[g]/(Tparams[["againstHomeTeam.lambda"]][HomeT[g]]))
#     }
#   }
  
#   return(list(homeRating.data=homeRating.data, awayRating.data=awayRating.data))
# }


# prepare.ratings.inits <- function(distribution, var.name, season, bugs.data.json, nChains, nTeams, inits.digits=5) {
#   inits <- gen.norm.inits.N.chains(nChains, nTeams)
  
#   directory.to.save <- paste0("match_statistics/ratings/", distribution, "/", var.name, "/", season, "/")
#   bugs.inits(inits=inits, n.chains=nChains, digits=inits.digits,
#              inits.files=paste0(directory.to.save, "bugs_ratings_inits", 1:nChains, ".txt"))
# }


# gen.norm.inits.N.chains <- function(nChains, nTeams, minu=0.01, maxu=30) {
#   init.values <- vector("list", nChains)
#   for (i in 1:nChains) {
#     chain.inits <- list(
#       homeRating.mu = runif(nTeams, min=minu, max=maxu),
#       homeRating.mu.shape = runif(1, min=minu, max=maxu),
#       homeRating.mu.rate = runif(1, min=minu, max=maxu),
      
#       awayRating.mu = runif(nTeams, min=minu, max=maxu),
#       awayRating.mu.shape = runif(1, min=minu, max=maxu),
#       awayRating.mu.rate = runif(1, min=minu, max=maxu),
      
#       homeRating.tau = runif(nTeams, min=minu, max=maxu),
#       homeRating.tau.shape = runif(1, min=minu, max=maxu),
#       homeRating.tau.rate = runif(1, min=minu, max=maxu),
      
#       awayRating.tau = runif(nTeams, min=minu, max=maxu),
#       awayRating.tau.shape = runif(1, min=minu, max=maxu),
#       awayRating.tau.rate = runif(1, min=minu, max=maxu)
#     )
#     init.values[[i]] = chain.inits
#   }
#   return(init.values)
# }


# all.team.rating.params <- list(c(
#     "homeRating.mu", "homeRating.tau",
#     "awayRating.mu", "awayRating.tau"
#   )
# )


# all.hyperprior.rating.params <- list(c(
#     "homeRating.mu.shape", "homeRating.mu.rate",
#     "homeRating.tau.shape", "homeRating.tau.rate",
#     "awayRating.mu.shape", "awayRating.mu.rate",
#     "awayRating.tau.shape", "awayRating.tau.rate"
#   )
# )


# all.rating.params <- list(c(
#     "homeRating.mu", "homeRating.mu.shape", "homeRating.mu.rate",
#     "homeRating.tau", "homeRating.tau.shape", "homeRating.tau.rate",
#     "awayRating.mu", "awayRating.mu.shape", "awayRating.mu.rate",
#     "awayRating.tau", "awayRating.tau.shape", "awayRating.tau.rate"
#   )
# )
################################################### Rating BUGS modelling functions ###################################################
#######################################################################################################################################

#######################################################################################################################################
################################################## Validating BUGS results functions ##################################################
# check.bugs.results.convergence <- function(all.vars, seasons, bugs.results.filename, main.or.ratings, coda=FALSE) {
#   if (main.or.ratings == "ratings") {
#     bugs.results.root.dir <- "bugs/match_statistics/06/ratings"
#   } else {
#     bugs.results.root.dir <- "bugs/match_statistics/06"
#   }
#   dir.to.save.at <- "R/results"
#   params.means <- list()
#   summary.stats <- list()
#   coda.mcmc.list <- list()
#   
#   for (distribution in names(all.vars)) {
#     for (distrib.list in all.vars[[distribution]]) {
#       for (var.name in distrib.list){
#         for (season in seasons) {
#           file.name <- paste(bugs.results.root.dir, distribution, var.name, season, bugs.results.filename, sep="/")
#           bugs.output <- readRDS(file.name)
#           
#           params.means[[var.name]][[season]] <- data.frame(
#             bugs.output$mean[(str_count(names(bugs.output$mean), "\\.")==1)])
#           summary.stats[[var.name]][[season]] <- data.frame(bugs.output$summary)
#           if (coda) {coda.mcmc.list[[var.name]][[season]] <- as.mcmc.list(bugs.output)}
#         }
#       }
#     }
#   }
#   
#   # Assess convergence and effective sample size
#   Rhat.list <- list()
#   n.eff.list <- list()
#   for (distribution in names(all.vars)) {
#     for (distrib.list in all.vars[[distribution]]) {
#       for (var.name in distrib.list){
#         for (season in seasons) {
#           Rhat.list[[paste0(var.name, ".", season)]] <- (length(
#             summary.stats[[var.name]][[season]]$Rhat[summary.stats[[var.name]][[season]]$Rhat<1.01])/
#               length(summary.stats[[var.name]][[season]]$Rhat))
#           n.eff.list[[paste0(var.name, ".", season)]] <- (length(
#             summary.stats[[var.name]][[season]]$n.eff[summary.stats[[var.name]][[season]]$n.eff>1000])/
#               length(summary.stats[[var.name]][[season]]$n.eff))
#         }
#       }
#     }
#   }
#   
#   Rhat.list <- round(t(as.data.frame(Rhat.list)), 3)
#   n.eff.list <- round(t(as.data.frame(n.eff.list)), 3)
#   colnames(Rhat.list) <- "Rhat.LT.1.01.pct"
#   colnames(n.eff.list) <- "n.eff.GT.1000.pct"
#   convergence.assessment <- cbind(Rhat.list, n.eff.list)
#   
#   # Save the param values for use in simulation
#   dir.create(paste(dir.to.save.at, main.or.ratings, sep="/"), showWarnings = F)
#   saveRDS(params.means, paste(dir.to.save.at, main.or.ratings, "params_means.RDS", sep="/"))
#   saveRDS(summary.stats, paste(dir.to.save.at, main.or.ratings, "summary_stats.RDS", sep="/"))
#   saveRDS(convergence.assessment, paste(dir.to.save.at, main.or.ratings, "convergence_assessment.RDS", sep="/"))
#   if (coda) {saveRDS(coda.mcmc.list, paste(dir.to.save.at, main.or.ratings, "coda_mcmc_list.RDS", sep="/"))}
# }


# INCOMPLETE FUNCTIONS
if (FALSE) {
  look.at.bugs.sim.values.INCOMPLETE.FUNCTION <- function() {
    par(mfrow=c(2,2))
    count = 0
    number.to.skip <- 0
    number.to.continue <- 0
    for (name in names(passes_pct$sims.array[1,1,])) {
      
      if (number.to.continue > 0) {
        number.to.continue <- number.to.continue - 1
      } else {
        skip.or.continue <- readline("Skip (s) or Continue (c)? ")
        if (skip.or.continue == "s") {
          number.to.skip <- as.integer(readline("How much to skip by? "))
          number.to.continue <- 0
        } else if (skip.or.continue == "c") {
          number.to.continue <- as.integer(readline("How much to contniue by? "))
          number.to.skip <- 0
        } else if (skip.or.continue == "q") {
          break()
        }
      }
      
      if (number.to.skip > 0) {
        number.to.skip <- number.to.skip - 1
        next()
      }
      
      plot(density(passes_pct$sims.array[,1,name]),
           main=paste(name, round(mean(passes_pct$sims.array[,1,name]), 3), sep="\n"))
      plot(density(passes_pct$sims.array[,2,name]),
           main=paste(name, round(mean(passes_pct$sims.array[,2,name]), 3), sep="\n"))
      count = count+1
      if (count==2) {
        Sys.sleep(1.5)
        count=0
      }
    }
    
    # Note you can add in the following to evaluate expressions whilst in interactive mode:
    eval(parse(text="mean(1:5)"))
    # So at the prompt the program can ask to skip continue or evaluate expression
    # Depending on what is passed the relevant action is taken
    # Need to see how to handle with incorrect expressions such that an error does not cause the program to break.
    
  }
  
  check.to.see.results.arent.too.low <- function() {
    team.params <- readRDS("bugs/match_statistics/04/param_means_all_teams.RDS")
    all.vars <- list(beta=list(c("passes_pct", "possession")),
                     gamma=list(c("passes_tot", "passes_acc")),
                     nbinom=list(c("shots_tot", "shots_ont", "shots_offt", "shots_inb", "shots_bl",
                                   "shots_outb", "offsides", "corners", "fouls", "gksaves")),
                     poisson=list(c("yc", "rc")))
    
    for (distribution in names(all.vars)) {
      for (distrib.list in all.vars[[distribution]]) {
        for (var.name in distrib.list){
          
          if (distribution == "beta") {
            print(var.name)
            print(min(team.params[[var.name]]$home.alpha))
            print(min(team.params[[var.name]]$away.alpha))
            
            print(min(team.params[[var.name]]$home.beta))
            print(min(team.params[[var.name]]$away.beta))
            
          } else if (distribution == "gamma") {
            print(var.name)
            print(min(team.params[[var.name]]$home.shape))
            print(min(team.params[[var.name]]$away.shape))
            
            print(min(team.params[[var.name]]$home.rate))
            print(min(team.params[[var.name]]$away.rate))
            
          } else if (distribution == "nbinom") {
            print(var.name)
            print(min(team.params[[var.name]]$home.r))
            print(min(team.params[[var.name]]$away.r))
            
            print(min(team.params[[var.name]]$home.prob))
            print(min(team.params[[var.name]]$away.prob))
            
          } else if (distribution == "poisson") {
            print(var.name)
            print(min(team.params[[var.name]]$home.lambda))
            print(min(team.params[[var.name]]$away.lambda))
            
          }
        }
      }
    }
  }
}
################################################## Validating BUGS results functions ##################################################
#######################################################################################################################################
