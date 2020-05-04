get.summary.stats.table <- function(Data, file.to.save.at=NULL, scale.cols=FALSE, digits=3) {
  # Create the object to store summary statistics
  if (scale.cols) {
    ovrl.summary <- matrix(nrow = 5, ncol = ncol(Data))
    row.names(ovrl.summary) <- c("Min", "Median", "Max", "IQRange", "Range")
    colnames(ovrl.summary) <- colnames(Data)
    Data <- scale(Data)
  } else {
    ovrl.summary <- matrix(nrow = 8, ncol = ncol(Data))
    row.names(ovrl.summary) <- c("Min", "Median", "Mean", "Max", "Std. dev.", "Var.", "IQRange", "Range")
    colnames(ovrl.summary) <- colnames(Data)
  }
  
  # Calculate summary statistics and return object
  ovrl.summary["Min",] <- round(apply(Data, 2, min), digits)
  ovrl.summary["Median",] <- round(apply(Data, 2, median), digits)
  ovrl.summary["Max",] <- round(apply(Data, 2, max), digits)
  ovrl.summary["IQRange",] <- round(apply(Data, 2, IQR), digits)
  ovrl.summary["Range",] <- round(apply(Data, 2, function(x){return(range(x)[2]-range(x)[1])}), digits)
  
  if (!scale.cols) {
    ovrl.summary["Mean",] <- round(colMeans(Data), digits)
    ovrl.summary["Std. dev.",] <- round(apply(Data, 2, sd), digits)
    ovrl.summary["Var.",] <- round(apply(Data, 2, var), digits)
  }
  
  if (is.character(file.to.save.at)) {
    write.csv(ovrl.summary, paste0(dir.to.save.at, "tables/", file.to.save.at))
  }
  
  return(ovrl.summary)
}


# Credit for function on stackoverflow (will link)
get.corr.all.vars <- function(df){
  corr.vars <- df %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1) %>%
    filter(var2 > var1)
  
  corr.vars.split = list(
    low = filter(corr.vars, abs(value) <= .2),
    medium = filter(corr.vars, (abs(value) > .2) & (abs(value) <= .5)),
    high = filter(corr.vars, abs(value) > .5)
    # NOTE: These thresholds were set arbitrarily.
  )
  
  return(corr.vars.split)
}


get.corr.all.vars.diff.sets <- function(df1, df2){
  corr.vars <- cor(df1, df2) %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  corr.vars.split = list(
    low = filter(corr.vars, abs(value) <= .2),
    medium = filter(corr.vars, (abs(value) > .2) & (abs(value) <= .5)),
    high = filter(corr.vars, abs(value) > .5)
  # NOTE: These thresholds were set arbitrarily.
  )

  return(corr.vars.split)
}


find.strong.cor.for.var <- function(df, var.name, opposite.teams) {
  home.var.name <- paste0("home_", var.name)
  away.var.name <- paste0("away_", var.name)
  if (opposite.teams){
    home.away.cor <- cor(match.data[seq(2, 34, 2)], match.data[seq(1, 34, 2)])
    away.home.cor <- cor(match.data[seq(1, 34, 2)], match.data[seq(2, 34, 2)])
  } else {
    home.away.cor <- cor(match.data[seq(1, 34, 2)], match.data[seq(1, 34, 2)])
    away.home.cor <- cor(match.data[seq(2, 34, 2)], match.data[seq(2, 34, 2)])
  }
  
  
  rownames(home.away.cor) <- str_remove(rownames(home.away.cor), "away_")
  rownames(away.home.cor) <- str_remove(rownames(away.home.cor), "home_")
  
  all.cor <- cbind(home.away.cor, away.home.cor)
  all.cor <- subset(all.cor, select=c(home.var.name, away.var.name))
  cor.sum <- abs(all.cor[,home.var.name]) + abs(all.cor[,away.var.name])
  all.cor <- cbind(all.cor, cor.sum)
  
  return(all.cor[order(abs(all.cor[,"cor.sum"]), decreasing = TRUE),])
}