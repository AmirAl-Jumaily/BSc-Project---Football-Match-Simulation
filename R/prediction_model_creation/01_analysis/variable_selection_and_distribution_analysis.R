library(fitdistrplus)
library(finalfit)
root.dir <- ""
setwd(root.dir)

source("R/utils/modelling.R")

# Note on the below: I will need to make sure this all still works when we pass in the transformed match stats.
match.data.transformed <- read.csv(paste0(root.dir, "data/csv_of_json_data/transformed/match_stats.csv"))

match.data <- read.csv(paste0(root.dir, "data/csv_of_json_data/clean/match_stats.csv"))
split.match.data <- split.home.away.data(match.data, "split")
for (home.or.away in c("home_data", "away_data")) {
  match.data <- split.match.data[[home.or.away]]
  
  match.data.gamma <- match.data[, 8:9]
  match.data.beta <- match.data[, 10:11]
  match.data.nbinom <- match.data[, c(2:7, 12:14, 17)]
  match.data.pois <- match.data[, 15:16]
  
  match.stat.dists <- list()
  
  dir.name.save <- paste0(root.dir, "plots/distributions/", home.or.away, "/")
  dir.create(dir.name.save, showWarnings = FALSE, recursive = TRUE)
  pdf(paste(dir.name.save, "continuous", ".pdf", sep=""), width=24, height=12)
  par(mfrow=c(2,5), mar=c(2.5,2.5,3,0.5), mgp=c(1.5,0.5,0))
  # Gamma
  match.stat.dists$gamma <- list()
  match.stat.dists$gamma$desc <- list()
  match.stat.dists$gamma$fits <- list()
  match.stat.dists$gamma$gofstats <- list()
  for (m.stat in names(match.data.gamma)) {
    match.stat.dists$gamma$desc[[m.stat]] <- descdist(match.data.gamma[[m.stat]], discrete=F, boot=1000)
    
    distributions <- c("gamma", "norm")#, "lnorm"
    for (distrib in distributions) {
      match.stat.dists$gamma$fits[[m.stat]][[distrib]] <- fitdist(match.data.gamma[[m.stat]], distrib)
    }
    match.stat.dists$gamma$gofstats[[m.stat]] <- gofstat(as.list(match.stat.dists$gamma$fits[[m.stat]]), fitnames=distributions)
    
    match.stat.dists$gamma$gof.summary <- round(rbind(match.stat.dists$gamma$gofstats[[m.stat]]$chisq,
                                                      match.stat.dists$gamma$gofstats[[m.stat]]$cvm,
                                                      match.stat.dists$gamma$gofstats[[m.stat]]$ad,
                                                      match.stat.dists$gamma$gofstats[[m.stat]]$ks,
                                                      match.stat.dists$gamma$gofstats[[m.stat]]$aic,
                                                      match.stat.dists$gamma$gofstats[[m.stat]]$bic), 3)
    rownames(match.stat.dists$gamma$gof.summary) <- c("chisq", "cvm", "ad", "ks", "aic", "bic")
    
    
    denscomp(match.stat.dists$gamma$fits[[m.stat]], legendtext = distributions, demp=T)
    qqcomp(match.stat.dists$gamma$fits[[m.stat]], legendtext = distributions)
    mtext(m.stat)
    cdfcomp(match.stat.dists$gamma$fits[[m.stat]], legendtext = distributions)
    ppcomp(match.stat.dists$gamma$fits[[m.stat]], legendtext = distributions)
  }
  match.stat.dists$gamma$summary <- as.data.frame(cbind(round(colMeans(match.data.gamma), 4), round(apply(match.data.gamma, 2, var), 4)))
  colnames(match.stat.dists$gamma$summary) <- c("ms.mean", "ms.variance")
  match.stat.dists$gamma$summary$var.mean.ratio <- round(match.stat.dists$gamma$summary$ms.variance/match.stat.dists$gamma$summary$ms.mean, 4)
  
  
  # names(t(as.data.frame(unlist(match.stat.dists$gamma$desc$passes_acc)))[,1:7])
  
  # Beta
  match.stat.dists$beta <- list()
  match.stat.dists$beta$desc <- list()
  match.stat.dists$beta$fits <- list()
  match.stat.dists$beta$gofstats <- list()
  for (m.stat in names(match.data.beta)) {
    match.stat.dists$beta$desc[[m.stat]] <- descdist(match.data.beta[[m.stat]], discrete=F, boot=1000)
    
    distributions <- c("beta", "gamma")#"norm"
    for (distrib in distributions) {
      match.stat.dists$beta$fits[[m.stat]][[distrib]] <- fitdist(match.data.beta[[m.stat]], distrib)
    }
    match.stat.dists$beta$gofstats[[m.stat]] <- gofstat(as.list(match.stat.dists$beta$fits[[m.stat]]), fitnames=distributions)
    
    match.stat.dists$beta$gof.summary <- round(rbind(match.stat.dists$beta$gofstats[[m.stat]]$chisq,
                                                     match.stat.dists$beta$gofstats[[m.stat]]$cvm,
                                                     match.stat.dists$beta$gofstats[[m.stat]]$ad,
                                                     match.stat.dists$beta$gofstats[[m.stat]]$ks,
                                                     match.stat.dists$beta$gofstats[[m.stat]]$aic,
                                                     match.stat.dists$beta$gofstats[[m.stat]]$bic), 3)
    rownames(match.stat.dists$beta$gof.summary) <- c("chisq", "cvm", "ad", "ks", "aic", "bic")
    
    denscomp(match.stat.dists$beta$fits[[m.stat]], legendtext = distributions, demp=T)
    qqcomp(match.stat.dists$beta$fits[[m.stat]], legendtext = distributions)
    mtext(m.stat)
    cdfcomp(match.stat.dists$beta$fits[[m.stat]], legendtext = distributions)
    ppcomp(match.stat.dists$beta$fits[[m.stat]], legendtext = distributions)
  }
  match.stat.dists$beta$summary <- as.data.frame(cbind(round(colMeans(match.data.beta), 4), round(apply(match.data.beta, 2, var), 4)))
  colnames(match.stat.dists$beta$summary) <- c("ms.mean", "ms.variance")
  match.stat.dists$beta$summary$var.mean.ratio <- round(match.stat.dists$beta$summary$ms.variance/match.stat.dists$beta$summary$ms.mean, 4)
  
  dev.off()
  
  
  # dir.name.save <- paste0(root.dir, "plots/distributions/")
  # dir.create(dir.name.save, showWarnings = FALSE, recursive = TRUE)
  pdf(paste(dir.name.save, "discrete", ".pdf", sep=""), width=20, height=12)
  par(mfrow=c(2,3), mar=c(2.5,2.5,3,0.5), mgp=c(1.5,0.5,0))
  # Negative Binomial
  match.stat.dists$nbinom <- list()
  match.stat.dists$nbinom$desc <- list()
  match.stat.dists$nbinom$fits <- list()
  match.stat.dists$nbinom$gofstats <- list()
  for (m.stat in names(match.data.nbinom)) {
    match.stat.dists$nbinom$desc[[m.stat]] <- descdist(match.data.nbinom[[m.stat]], discrete=T, boot=1000)
    
    distributions <- c("nbinom", "pois")#, "norm"
    for (distrib in distributions) {
      match.stat.dists$nbinom$fits[[m.stat]][[distrib]] <- fitdist(match.data.nbinom[[m.stat]], distrib)
    }
    match.stat.dists$nbinom$gofstats[[m.stat]] <- gofstat(as.list(match.stat.dists$nbinom$fits[[m.stat]]), fitnames=distributions)
    
    match.stat.dists$nbinom$gof.summary <- round(rbind(match.stat.dists$nbinom$gofstats[[m.stat]]$chisq,
                                                       match.stat.dists$nbinom$gofstats[[m.stat]]$aic,
                                                       match.stat.dists$nbinom$gofstats[[m.stat]]$bic), 3)
    rownames(match.stat.dists$nbinom$gof.summary) <- c("chisq", "aic", "bic")
    
    denscomp(match.stat.dists$nbinom$fits[[m.stat]], legendtext = distributions, demp=T)
    mtext(m.stat)
    cdfcomp(match.stat.dists$nbinom$fits[[m.stat]], legendtext = distributions)
    # plot(density(match.data.nbinom[[m.stat]]), main="Empirical density plot")
  }
  match.stat.dists$nbinom$summary <- as.data.frame(cbind(round(colMeans(match.data.nbinom), 4), round(apply(match.data.nbinom, 2, var), 4)))
  colnames(match.stat.dists$nbinom$summary) <- c("ms.mean", "ms.variance")
  match.stat.dists$nbinom$summary$var.mean.ratio <- round(match.stat.dists$nbinom$summary$ms.variance/match.stat.dists$nbinom$summary$ms.mean, 4)
  
  
  # Poisson
  match.stat.dists$pois <- list()
  match.stat.dists$pois$desc <- list()
  match.stat.dists$pois$fits <- list()
  match.stat.dists$pois$gofstats <- list()
  for (m.stat in names(match.data.pois)) {
    match.stat.dists$pois$desc[[m.stat]] <- descdist(match.data.pois[[m.stat]], discrete=T, boot=1000)
    
    distributions <- c("pois", "nbinom")#, "norm"
    for (distrib in distributions) {
      match.stat.dists$pois$fits[[m.stat]][[distrib]] <- fitdist(match.data.pois[[m.stat]], distrib)
    }
    if (m.stat == "yc") {
      match.stat.dists$pois$gofstats[[m.stat]] <- gofstat(as.list(match.stat.dists$pois$fits[[m.stat]]), fitnames=distributions)
      
      match.stat.dists$pois$gof.summary <- round(rbind(match.stat.dists$pois$gofstats[[m.stat]]$chisq,
                                                       match.stat.dists$pois$gofstats[[m.stat]]$aic,
                                                       match.stat.dists$pois$gofstats[[m.stat]]$bic), 3)
      rownames(match.stat.dists$pois$gof.summary) <- c("chisq", "aic", "bic")
    }
    
    denscomp(match.stat.dists$pois$fits[[m.stat]], legendtext = distributions, demp=T)
    mtext(m.stat)
    cdfcomp(match.stat.dists$pois$fits[[m.stat]], legendtext = distributions)
    # plot(density(match.data.pois[[m.stat]]), main="Empirical density plot")
  }
  match.stat.dists$pois$summary <- as.data.frame(cbind(round(colMeans(match.data.pois), 4), round(apply(match.data.pois, 2, var), 4)))
  colnames(match.stat.dists$pois$summary) <- c("ms.mean", "ms.variance")
  match.stat.dists$pois$summary$var.mean.ratio <- round(match.stat.dists$pois$summary$ms.variance/match.stat.dists$pois$summary$ms.mean, 4)
  
  dev.off()
}

# Numerical measures

# To show us a comparison of variance to mean to justify nbinom. Only shots tot is clear but most have
# a var larger than mean although rc and yc are gret for poisson. For consistency, we'll use nbinom for all.

##################################################################################################################################
# Going through the fistdistrplus documentation. Noting down below functions and things I may want to use when illustarting which
# distribution fits the data the best

# # First thing to start with is list the distributions where the characteristics of the random variable data fit the assumptions of
# # those distributions. For example considering the range of the variable, if it is discrete, it's skewness and anything else
# 
# plotdist(histo=TRUE, demp=TRUE, ) # Look at other arguments

# Note: non-zero skewness reveals a lack of symmetry. Kurtosis value quantifies the weight of tails in comparison to the normal
# distribution for which kurtosis equals 3. To get these values and other classical descriptive statistics:
# descdist()
# Unbiased estimates of the stdv, skewness and kurtosis are given by default but can get them without this correction too.
# Cullen and frey plot is provided with this function as i've seen. I should have a look at the paper and maybe cite it when
# justifying my use of the plot when determining the distributions. However, note that this plot only indicates if the data have
# skewness and kurtosis that match the distributions. This plot shouldnt be used alone to decide and should be used as an
# indicator. Since these two measures are known not to be robust, we can pass the argument boot to perform a nonparamteric
# bootstrap procedure and the graph will show where all of the samples fell in the plot. These higher moments have high variance.
# Expected value and range should be considered along with the plotdist and descdist functions.

# fitdist()
# This function, can be used to estimate the parameters by MLE after specifying which distribution to check for. You can do print
# plot and summary on the object returned frm this function. summary provides a range of metrics and plot provides 4
# goodness-of-fit plots. You can get each of the 4 goodness-of-fit plots seperately using the functions: denscomp, cdfcomp, qqcomp
# and ppcomp. Make sure you understand what each plot tells you. For each of these you can pas a list of different fitdist objects
# which could be different distributions fit on the same dataset so you can easily compare them all. It works by overlaying the
# lines and points on the same 4 plots. You should also define a legend and pass that into the function as is shown in the paper.
# Note that the QQ plots emphasizes the lack of fit in the distribution tails while the PP plot emphasises the lack of the fit in
# the distribution centre. (Double check if this is generally the case or just for the example they are showing).

# Can look at goodness-of-fit statistics as mentioned in the paper are three. (Look online to understand more about these
# goodness-of-fit statistics and if there is one specifically that I should use which is more suitable formy needs).
# gofstat()
# You pass the object returned from fitdist for each of the different distributions and it gives the stats for each of the measures
# as well as AIC and BIC to measure possible overfitting. The Anderson statistics is apparently difficult to compare various
# distributions. The Cramer and Kolmogorov ones don;t take into account the complexity of the distribution (i.e. number of
# parameters) but given that basically all of the distributions I am comparing have 2 statistics, this should be fine for me and so
# I will most likely use these two over the Anderson statistic.

# Can estimate uncertainty of parameter estimates by parameteric or nonparametric bootstraps
# shots.bl.boot <- bootdist(discrete.fits$shots_bl$nbinom)
# Can use summary to get quantiles and can also plot the parameter values against each other to see how the values of each of the
# parameters vary with each other. Bootstrap samples can be used to calculate confidence intervals for the parameter estimates.


##################################################################################################################################

# We find RF does better when we give it all of the data so we will instead have the following distirbutions:
# nbinom:
#   Goals, shots_tot, shots_ont, shots_offt, shots_bl, shots_inb, shots_outb, offsides, corners, fouls, gk saves
# Beta:
#   Passes_pct, Possession
# Gamma:
#   Passes_tot, Passes_acc
# Poisson:
#   yc, rc



# Pairs panels plots + random code
if (FALSE) {
  library(psych)
  
  pairs.panels(match.stats.home[1:6])
  pairs.panels(match.stats.home[7:12])
  pairs.panels(match.stats.home[13:16])
  
  # fitdistr(...)$aic
  # home.data <- split.home.away.data(match.data, "home")
  
  # goals.all.dist.fits <- fit.all.dists(home.data$goals)
  # ggplot(home.data, aes(goals)) + geom_density()
}
