setwd("bugs/data/filtered")
source("R/utils/analysis.R")

library(tibble)
library(dplyr)
library(tidyr)
library(stringr)

match.stats.home = read.csv("home_match_stats.csv")
match.stats.away = read.csv("away_match_stats.csv")

home.corr.split <- get.corr.all.vars(match.stats.home)
away.corr.split <- get.corr.all.vars(match.stats.away)

# Now we apply the above and see the results

new.match.stats.home <- data.frame(match.stats.home)
new.match.stats.away <- data.frame(match.stats.away)

new.match.stats.home$home_shots_inb_pct <- (
  new.match.stats.home$home_shots_inb/new.match.stats.home$home_shots_tot
)
new.match.stats.home$home_shots_ont_pct <- (
  new.match.stats.home$home_shots_ont/new.match.stats.home$home_shots_tot
)
new.match.stats.home$home_shots_bl_pct <- (
  new.match.stats.home$home_shots_bl/new.match.stats.home$home_shots_tot
)

new.match.stats.away$away_shots_inb_pct <- (
  new.match.stats.away$away_shots_inb/new.match.stats.away$away_shots_tot
)
new.match.stats.away$away_shots_ont_pct <- (
  new.match.stats.away$away_shots_ont/new.match.stats.away$away_shots_tot
)
new.match.stats.away$away_shots_bl_pct <- (
  new.match.stats.away$away_shots_bl/new.match.stats.away$away_shots_tot
)


new.match.stats.home <- within(new.match.stats.home, rm(
  home_shots_inb, home_shots_outb, home_shots_ont, home_shots_offt, home_shots_bl,
  home_passes_acc, home_passes_tot))
new.match.stats.away <- within(new.match.stats.away, rm(
  away_shots_inb, away_shots_outb, away_shots_ont, away_shots_offt, away_shots_bl,
  away_passes_acc, away_passes_tot))


# Now we rerun the correlation checker to see the results for these 12 varaibles
new.home.corr.split <- get.corr.all.vars(new.match.stats.home)
new.away.corr.split <- get.corr.all.vars(new.match.stats.away)

# We see significantly less correlation.
# In the home data we find correlation between:
#   possession, corners <-> shots_tot
#   possession <-> passes_pct
# In the away data we find correlation between:
#   possession <-> passes_pct

# Based on this, considering we were debating whether to drop possession or passes_pct,
# it looks sensible to drop possession since this will mean we have non inter-covariate
# correlation values greater than 0.5 except for home_corners <-> home_shots_tot and even
# that value is almost exactly 0.5, and so on the threshold set. away correlation is 0.494.
# If corners don't explain much in the model then we will rmeove that too. However it may be
# a good indicator of a team's play style.
# NOTE: The 0.5 threshold was set arbitrarily.

# CONCLUSION:
#   ADD (3): shots_inb_pct, shots_ont_pct, shots_bl_pct
#   REMOVE (7): shots_ont, shots_offt, shots_bl, shots_inb, shots_outb, passes_acc, passes_tot
#   MAYBE REMOVE (2): (passes_pct or possession (more likely)), corners


################################################################################################################
# Now we apply the above but we compare stats of opposing teams to find relationships
match.data <- read.csv("data/csv_of_json_data/filtered/match_stats.csv")
match.data <- match.data[,21:54]
home_pct_shots_bl <- match.data$away_shots_bl / match.data$away_shots_tot
away_pct_shots_bl <- match.data$home_shots_bl / match.data$home_shots_tot
# home_pct_shots_bl[is.na(home_pct_shots_bl)] <- 0
# away_pct_shots_bl[is.na(away_pct_shots_bl)] <- 0
match.data <- cbind(match.data, home_pct_shots_bl, away_pct_shots_bl)
match.data[is.na(match.data)] <- 0
remove(home_pct_shots_bl, away_pct_shots_bl)
match.data <- subset(match.data, select=-c(home_shots_bl, away_shots_bl))

home.away.stat.cor <- get.corr.all.vars.diff.sets(match.data[seq(1, 34, 2)], match.data[seq(2, 34, 2)])

# "goals"      "shots_ont"  "shots_offt" "shots_tot"  "shots_inb" 
# "shots_outb" "passes_acc" "passes_tot" "passes_pct" "possession" "corners"   
# "offsides"   "fouls"      "yc"         "rc"         "gksaves" 
# "pct_shots_bl"


