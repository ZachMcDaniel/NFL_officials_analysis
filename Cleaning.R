rm(list=ls())

library(dplyr)
library(reshape2)

load("officials_overview.rda")
load("officials_stats.rda")
load("officials_games.rda")


###############################################################################
#officials_df
###############################################################################

#changing column types
officials_df$officials <- as.character(officials_df$officials)
officials_df$games <- as.integer(officials_df$games)
officials_df$positions <- factor(officials_df$positions)
officials_df$years <- as.Date(officials_df$years, format = "%y")

#Rename columns

###############################################################################
# stats_df
###############################################################################

#changing column types
stats_df$years <- as.Date(stats_df$years, format = "%y")
stats_df$games <- as.integer(stats_df$games)
stats_df$playoff_games <- as.integer(stats_df$playoff_games)
stats_df$position <- factor(stats_df$position)
stats_df$home <- as.integer(stats_df$home)
stats_df$visitor <- as.integer(stats_df$visitor)
#stats_df$home_penalties <-
#stats_df$home_wp <-
stats_df$total_penalties <- as.integer(stats_df$total_penalties)
stats_df$penalty_yards <- as.integer(stats_df$penalty_yards)
stats_df$avg_penalties<- as.numeric(stats_df$avg_penalties)
stats_df$avg_yards <- as.numeric(stats_df$avg_yards)
#stats_df$league_home_penalties <-
#stats_df$league_home_wp <-
stats_df$league_avg_penalties <- as.numeric(stats_df$league_avg_penalties)
stats_df$league_avg_yards <- as.numeric(stats_df$league_avg_yards)

#use gsub to get rid of %

#Rename Columns

################################################################################
# games_df
################################################################################

#changing column types
games_df$Date <- #as.Date()
games_df$PtsO <- as.integer(games_df$PtsO)
games_df$Pen <- as.integer(games_df$Pen)
games_df$Yds <- as.integer(games_df$Yds)
games_df$Pts <- as.integer(games_df$Pts)
games_df$Pen1 <- as.integer(games_df$Pen1)
games_df$Yds1 <- as.integer(games_df$Yds1)

#rename columns

###############################################################################
# game_info_df
###############################################################################
