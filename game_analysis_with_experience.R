

rm(list = ls())


library(readr)
library(dplyr)

load("game_with_total_experience.rda")

################################################################################
#Making Numeric & Categoric only predictors DF
################################################################################

num_game_df <- select(game_df, duration_min, attendance, temp, humidity, wind, `wind chill`,
                      home_points, home_penalties, home_yards, away_points, away_penalties,
                      away_yards, total_experience, total_penalties, highpenalty, home_win, 
                      home_advantage)



cat_game_df <- select(game_df, won_toss, OT_toss, roof, surface, date, home_team, away_team, year,
                      total_penalties)

################################################################################
#Correlation Matrix for game_df with total experience
################################################################################

num_game_df <- na.omit(num_game_df)

## To filter on correlations, we first get the correlation matrix for the 
## predictor set
dfnumCorr <- cor(num_game_df) 

## caret's findCorrelation function is used to identify columns to remove.
## The absolute values of pair-wise correlations are considered. If two variables 
## have a high correlation, the function looks at the mean absolute correlation
## of each variable and removes the variable with the largest mean absolute correlation.
library(caret)

highCorr <- findCorrelation(dfnumCorr, .5)
highCorr

#Check columns before removing
#####names(dffull[highCorr]) this was incorrect dataframe form Script 4v5
names(num_game_df[highCorr])

#####Added addition functions to write the correlation out in a file for visual analysis
#change to dataframe
outCorMatrx = as.data.frame(dfnumCorr)

#write correlation matrix
write.csv(outCorMatrx, "GameDfCorMatrx.csv", row.names = TRUE)

################################################################################
## ROC Curve testing numeric predictors on categoric feature
###############################################################################

## Compute the areas under the ROC curve for each feature
library(caret)
aucVals <- filterVarImp(x = num_game_df[, -1], y = as.factor(num_game_df$home_advantage))

#add predictor names as a column
aucVals$Predictor <- rownames(aucVals)
#sort on importance
aucVals = aucVals[order(-aucVals$X1),]
#reorder columns
aucVals = aucVals[,c(3,2)]
#rename column
colnames(aucVals)[2] <- "AUC"

write_csv(aucVals, "NumGameDfAUC.csv")
