root.dir <- ""


fit.all.dists <- function(data.vector, list.of.dists) {
  list.of.fits = list()
  for (distrib in list.of.dists) {
    list.of.fits[[distrib]] <- fitdist(data.vector, distrib)
  }
  
  return(list.of.fits)
}


save.dist.plots <- function(df, cols.to.plot, distributions, dir.save.at,
                            cullen.frey=TRUE, save.plots=TRUE, verbose=FALSE) {
  for (m.stat in cols.to.plot) {
    dir.name.save <- paste(root.dir, "plots/distributions/match_stats/", dir.save.at, "/", sep="")
    dir.create(dir.name.save, showWarnings = FALSE)
    
    if (save.plots) {
      pdf(paste(dir.name.save, m.stat, ".pdf", sep=""), width=20, height=12)
    }
    
    for (distrib in distributions) {
      tryCatch({
        method <- ifelse(distrib %in% c("lnorm", "nbinom", "binom", "pois"), "mle", "mge")
        fitted.dist <- fitdist(df[[m.stat]], distr = distrib, method=method)
        plot(fitted.dist)
        mtext(paste(distrib, m.stat), outer = TRUE, line=-2.5, cex=2)
        if (verbose) {print(paste(distrib, "SUCCEEDED"))}
      }, error=function(e){if (verbose) {print(paste(distrib, "FAILED"))}})
    }
    if (cullen.frey) {
      descdist(df[[m.stat]], discrete = TRUE)
      text(0.5, 0.5, paste(toupper(m.stat), "DISCRETE", sep=" "), cex=2.5)
      descdist(df[[m.stat]], discrete = FALSE)
      text(0.5, 0.5, paste(toupper(m.stat), "CONTINUOUS", sep=" "), cex=2.5)
    }
    
    if (save.plots) {
      dev.off()
    }
  }
}


create.box.plot.across.values <- function(the.data, col.to.compare, split.by) {
  the.data = the.data[c(split.by, col.to.compare)]
  new.data = list()
  for (the.name in unique(as.factor(the.data[[split.by]]))) {
    new.data[[the.name]] = the.data[the.data[[split.by]] == the.name,][[col.to.compare]]
  }
  return(new.data)
}