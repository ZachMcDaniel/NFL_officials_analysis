rm(list = ls())

library(readr)
library(dplyr)


load('Official_Stats_Merged.rda')
load('Official_Games_Merged.rda')

################################################################################
#Making numerical and categoric dataframes
################################################################################
num_officials_stats <- officials_stats
num_officials_stats <- select(num_officials_stats, home_penalties
                              ,home_wp, total_penalties, penalty_yards, avg_penalties,
                              avg_yards,avg_yard_per_penalty, home_bias, harsh,
                              high_penalty, tenure)

################################################################################
#Correlation Matrix 
################################################################################

num_officials_stats <- na.omit(num_officials_stats)

## To filter on correlations, we first get the correlation matrix for the 
## predictor set
dfnumCorr <- cor(num_officials_stats) 

## caret's findCorrelation function is used to identify columns to remove.
## The absolute values of pair-wise correlations are considered. If two variables 
## have a high correlation, the function looks at the mean absolute correlation
## of each variable and removes the variable with the largest mean absolute correlation.
library(caret)

highCorr <- findCorrelation(dfnumCorr, .5)
highCorr

#Check columns before removing
#####names(dffull[highCorr]) this was incorrect dataframe form Script 4v5
names(num_officials_stats[highCorr])

#####Added addition functions to write the correlation out in a file for visual analysis
#change to dataframe
outCorMatrx = as.data.frame(dfnumCorr)

#write correlation matrix
write_csv(outCorMatrx, "OfficialStatsCorMatrx.csv")


################################################################################
#Home Bias & Harshness
################################################################################
num_officials_stats$avg_yard_per_penalty <- num_officials_stats$penalty_yards / num_officials_stats$total_penalties

num_officials_stats$home_bias <- ifelse(num_officials_stats$home_wp > 
                                          num_officials_stats$league_home_wp, 1, 0)

num_officials_stats$harsh <- ifelse( num_officials_stats$avg_yard_per_penalty >= 12, 1, 0)

num_officials_stats$high_penalty <- ifelse(num_officials_stats$avg_penalties >= num_officials_stats$league_avg_penalties, 1, 0)

num_officials_stats$tenure <- officials_stats$years - as.integer(officials_stats$start_year)
num_officials_stats$tenure_0to5 <- ifelse(num_officials_stats$tenure <= 5, 1, 0)
num_officials_stats$tenure_5to10 <- ifelse(num_officials_stats$tenure >5 & num_officials_stats$tenure <= 10, 1, 0)
num_officials_stats$tenure_10plus <- ifelse(num_officials_stats$tenure >10 , 1, 0)

num_officials_stats2 <- select(num_officials_stats,playoff_games, tenure_0to5, tenure_5to10, tenure_10plus, harsh, home_bias, high_penalty)
#playoff_games, home, visitor, home_penalties, home_wp, total_penalties, penalty_yards, avg_penalties,avg_yards, league_home_penalties, league_home_wp, league_avg_penalties, league_avg_yards,
################################################################################
#ROC curves
################################################################################

## Compute the areas under the ROC curve for each feature
library(caret)
aucVals <- filterVarImp(x = num_officials_stats2[, -1], y = as.factor(num_officials_stats2$high_penalty))

#add predictor names as a column
aucVals$Predictor <- rownames(aucVals)
#sort on importance
aucVals = aucVals[order(-aucVals$X1),]
#reorder columns
aucVals = aucVals[,c(3,2)]
#rename column
colnames(aucVals)[2] <- "AUC"

readr::write_csv(aucVals, "NumOfficialsStatsAUC.csv")


###############################################################################
officials_stats$start_year <- as.integer(officials_stats$start_year)
officials_stats$experience <- officials_stats$years - officials_stats$start_year

cor(officials_stats$experience, officials_stats$home_wp)


