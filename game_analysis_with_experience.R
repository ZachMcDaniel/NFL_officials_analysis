

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

################################################################################
#highest penalty games table
################################################################################
library(dplyr)
game_df <- arrange(game_df, desc(total_penalties))
game_df$ratio <- (game_df$home_yards + game_df$away_yards)/game_df$total_penalties 
high_ratio <- top_n(game_df, 20, ratio)

top_penalty_game <- top_n(game_df, 20, total_penalties)
top_penalty_game$ratio <- (top_penalty_game$home_yards +top_penalty_game$away_yards)/
                                top_penalty_game$total_penalties

df <- group_by(top_penalty_game, year)
toppenalty_years <- summarize(df, freq = sum(highpenalty) )

###############################################################################
# Total Penalties vs Year ANOVA
###############################################################################
game_df <- group_by(game_df, year)
#game_df <- ungroup(game_df)
yearly_penalty_stats <- summarize(game_df, total_penalties = sum(total_penalties), avg_penalties =
                    mean(total_penalties), total_yards = sum(home_yards+ away_yards), avg_yards =
                    mean(home_yards+away_yards))


###run anovas on this table 
game_df <- game_df[game_df$year != 2020,]

fit<-aov(total_penalties~year, data=game_df) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$year)
df <- as.data.frame(df$year)

bartlett.test(total_penalties~year,data=game_df)
#we do not have equal variance
kruskal.test(total_penalties~year,data=game_df)
#there is a significant difference in years
shapiro.test(game_df$total_penalties)
### testing assumptions - data is normal

write.csv(df, file = "total_penalties_vs_year.csv", row.names = TRUE)


################################################################################
#Total Yards vs year
################################################################################
game_df$total_yards <- game_df$home_yards + game_df$away_yards

###run anovas on this table 
game_df <- game_df[game_df$year != 2020,]

fit<-aov(total_yards~year, data=game_df) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$year)
df <- as.data.frame(df$year)

bartlett.test(total_yards~year,data=game_df)
#we do not have equal variance
kruskal.test(total_yards~year,data=game_df)
#there is a significant difference in years
shapiro.test(game_df$total_yards)
### testing assumptions - data is normal

write.csv(df, file = "total_yards_vs_year.csv", row.names = TRUE)

###############################################################################
#
#################################################################################

library(dplyr)
game_df <- group_by(game_df, year)
summ <- summarize(game_df, total_game = n())

