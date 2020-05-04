get.country <- function(name, name.country.mapper) {
  return(name.country.mapper[[name]])
}

get.league.position.summary <- function(league.table) {
  position.summary <- aggregate(as.numeric(league.table$position),
                                by=list(league.table$team.country), FUN=mean)
  position.summary <- cbind(position.summary, aggregate(as.numeric(league.table$position),
                                                        by=list(league.table$team.country), FUN=sd)$x)
  colnames(position.summary) <- c("country", "mean.pos", "sd.pos")

  return(position.summary)
}

split.into.N.leagues <- function(league.table, n) {
  split.leagues <- list()
  teams.per.league <- as.integer(nrow(league.table)/(n-1))
  remaining.teams <- nrow(league.table)%%teams.per.league
  for (i in 1:n) {
    league.level <- paste0("Level ", i)
    split.leagues[[league.level]] <- list()
    if (i != n) {
      league.table.segment <- league.table[(1+(i-1)*teams.per.league):(teams.per.league*i),]
    } else {
      league.table.segment <- league.table[(1+(i-1)*teams.per.league):nrow(league.table),]
    }
    split.leagues[[league.level]]$country.split <- table(sort(league.table.segment$team.country))
    split.leagues[[league.level]]$league.table <- league.table.segment
  }
  return(split.leagues)
}

get.country.result.comparison <- function(countryA, countryB, match.predictions, name.country.mapper) {
  match.predictions <- match.predictions[, 1:6]
  match.predictions$home_country <- unlist(map(
    match.predictions$home_team, get.country, name.country.mapper=name.country.mapper), use.names = F)
  match.predictions$away_country <- unlist(map(
    match.predictions$away_team, get.country, name.country.mapper=name.country.mapper), use.names = F)

  important.matches <- match.predictions[
    ((match.predictions$home_country == countryA) & (match.predictions$away_country == countryB)) |
      ((match.predictions$home_country == countryB) & (match.predictions$away_country == countryA)),]

  h2hrecord <- table(important.matches$home_country, important.matches$winner)
  colnames(h2hrecord) <- c("home_loss_pct", "home_draw_pct", "home_win_pct")
  h2hrecord[countryA,] <- h2hrecord[countryA,]/sum(h2hrecord[countryA,])
  h2hrecord[countryB,] <- h2hrecord[countryB,]/sum(h2hrecord[countryB,])
  h2hrecord <- round(h2hrecord[,c(3:1)], 2)

  return(list(league.standings=get.league.standings(important.matches), h2hrecord=h2hrecord))
}

compare.teams.of.equal.level <- function(nSims, leagues.to.compare, league.season, teams.per.league) {
  simulations.by.country <- list()
  num.teams.remaining <- matrix(nrow=length(leagues.to.compare), ncol=1)
  rownames(num.teams.remaining) <- sort(leagues.to.compare)
  colnames(num.teams.remaining) <- c("teams.remaining")
  
  for (league in sort(leagues.to.compare)) {
    simulations.by.country[[league]] <- run.simulations(nSims, league, league.season)
    num.teams.remaining[league, "teams.remaining"] <- nrow(
      simulations.by.country[[league]]$league.standings$agg.by.mean$league.table)
  }
  
  # This is to group the teams into their own seperate leagues
  team.groups.by.quality <- list()
  for (league in sort(leagues.to.compare)) {
    league.teams.remaining <- num.teams.remaining[league, ]
    league.table <- simulations.by.country[[league]]$league.standings$agg.by.sum$league.table
    league.level <- 1
    while (league.teams.remaining>0) {
      league.level.name <- paste0("Level ", league.level)
      if (league.teams.remaining > teams.per.league) {
        if ((league.level.name %in% names(team.groups.by.quality))) {
          team.groups.by.quality[[league.level.name]] <- c(
            team.groups.by.quality[[league.level.name]], league.table$team[1:teams.per.league])
        } else {
          team.groups.by.quality[[league.level.name]] <- league.table$team[1:teams.per.league]}
        league.teams.remaining <- league.teams.remaining - teams.per.league
      } else {
        if ((league.level.name %in% names(team.groups.by.quality))) {
          team.groups.by.quality[[league.level.name]] <- c(
            team.groups.by.quality[[league.level.name]], league.table$team)
        } else {
          team.groups.by.quality[[league.level.name]] <- league.table$team}
        league.teams.remaining <- league.teams.remaining - length(league.table$team)
        if (league.teams.remaining < 0) {print("ERROR - Something wrong in calculations when splitting teams")}
      }
      league.level <- league.level + 1
      league.table <- league.table[(teams.per.league+1):nrow(league.table),]
    }
  }
  
  simulations.by.quality <- list()
  for (league.level in names(team.groups.by.quality)) {
    simulations.by.quality[[league.level]] <- run.simulations(
      nSims, team.groups.by.quality[[league.level]], league.season)
  }
  
  return(list(simulations.by.quality=simulations.by.quality, simulations.by.country=simulations.by.country))
}

get.league.table.aggregated.by.team.country <- function(match.predictions, name.country.mapper) {
  match.predictions <- match.predictions[, 1:6]
  match.predictions$home_team <- unlist(map(
    match.predictions$home_team, get.country, name.country.mapper=name.country.mapper), use.names = F)
  match.predictions$away_team <- unlist(map(
    match.predictions$away_team, get.country, name.country.mapper=name.country.mapper), use.names = F)
  
  match.predictions$winning.country <- with(
    match.predictions, ifelse(winner == "home", home_team, ifelse(winner == "away", away_team, paste0(home_team, "=", away_team))))
  match.predictions$losing.country <- with(
    match.predictions, ifelse(winner == "home", away_team, ifelse(winner == "away", home_team, paste0(home_team, "=", away_team))))

  # h2hrecord <- table(important.matches$home_country, important.matches$winner)
  # colnames(h2hrecord) <- c("home_loss_pct", "home_draw_pct", "home_win_pct")
  # h2hrecord[countryA,] <- h2hrecord[countryA,]/sum(h2hrecord[countryA,])
  # h2hrecord[countryB,] <- h2hrecord[countryB,]/sum(h2hrecord[countryB,])
  # h2hrecord <- round(h2hrecord[,c(3:1)], 2)

  return(list(league.standings=get.league.standings(important.matches)
  # , h2hrecord=h2hrecord
  ))
}