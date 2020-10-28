rm(list=ls())

library(dplyr)
library(tidyr)
library(reshape2)
load("officials_overview.rda")
load("officials_stats.rda")
load("officials_games.rda")
load("game_info.rda")


###############################################################################
#officials_df
###############################################################################

#changing column types
officials_df$officials <- as.character(officials_df$officials)
officials_df$games <- as.integer(officials_df$games)
officials_df$positions <- factor(officials_df$positions)
#start year and end year as date?

#Rename columns

#fixing columns
officials_df$start_year <- sapply(strsplit(officials_df$years, "-"), "[[", 1 )
officials_df$end_year <- sapply(strsplit(officials_df$years, "-"), "[[", 2 )

#remove years col
officials_df <- select(officials_df, -years)

# reorder columns
officials_df <- select(officials_df, officials, games, start_year, end_year, positions, urls)

#rewriting the file
save(officials_df, file = "officials_overview.rda")

###############################################################################
# stats_df
###############################################################################

stats_df$home_penalties <- gsub("%", "", stats_df$home_penalties)
stats_df$home_wp <- gsub("%", "", stats_df$home_wp)
stats_df$playoff_games <- gsub("*", "", stats_df$playoff_games, fixed = TRUE)
stats_df$league_home_penalties <- gsub("%", "", stats_df$league_home_penalties)
stats_df$league_home_wp <- gsub("%", "", stats_df$league_home_wp)


#changing column types
stats_df$years <- as.integer(stats_df$years)
stats_df$games <- as.integer(stats_df$games)
stats_df$playoff_games <- as.integer(stats_df$playoff_games)
stats_df$home <- as.integer(stats_df$home)
stats_df$visitor <- as.integer(stats_df$visitor)
stats_df$home_penalties <- as.numeric(stats_df$home_penalties)
stats_df$home_wp <- as.numeric(stats_df$home_wp)
stats_df$total_penalties <- as.integer(stats_df$total_penalties)
stats_df$penalty_yards <- as.integer(stats_df$penalty_yards)
stats_df$avg_penalties<- as.numeric(stats_df$avg_penalties)
stats_df$avg_yards <- as.numeric(stats_df$avg_yards)
stats_df$league_home_penalties <- as.numeric(stats_df$league_home_penalties)
stats_df$league_home_wp <- as.numeric(stats_df$league_home_wp)
stats_df$league_avg_penalties <- as.numeric(stats_df$league_avg_penalties)
stats_df$league_avg_yards <- as.numeric(stats_df$league_avg_yards)


#Changing scale of win %
#home_wp is the only column to one decimal
stats_df$home_wp <- stats_df$home_wp * 100
stats_df$league_home_wp <- stats_df$league_home_wp * 100


#fixing position col
jobs <- strsplit(stats_df$position, ",")
job1 <- character(0)
job2 <- character(0)

for (i in jobs) {
 job1 <- c(job1, i[1])
 if (length(i) == 2){
   job2 <- c(job2, i[2])
 }
 else {
   job2 <- c(job2,NA)
 }
}

stats_df$position1 <- job1
stats_df$position2 <- job2

#remove the position column
stats_df <- select(stats_df, -position)
#reorder columns
stats_df <- select(stats_df, years, games, position1, position2, playoff_games, 
                   home, visitor, home_penalties, home_wp, total_penalties, 
                   penalty_yards, avg_penalties, avg_yards, league_home_penalties, 
                   league_home_wp, league_avg_penalties, league_avg_yards, urls)

#rewrite the file
save(stats_df, file = "officials_stats.rda")

###############################################################################
#combined officials with their stats per year
##############################################################################3
officials_stats<-merge(officials_df, stats_df, by = "urls")
save(officials_stats, file = "Official_Stats_Merged.rda")






################################################################################
# games_df
################################################################################

#changing column types
games_df$Date <- as.Date(games_df$Date, format = "%B %d, %Y")
games_df$PtsO <- as.integer(games_df$PtsO)
games_df$Pen <- as.integer(games_df$Pen)
games_df$Yds <- as.integer(games_df$Yds)
games_df$Pts <- as.integer(games_df$Pts)
games_df$Pen1 <- as.integer(games_df$Pen1)
games_df$Yds1 <- as.integer(games_df$Yds1)

#rename columns
games_df <- rename(games_df, date = Date, home_team = Tm, away_team = Opp, position = Pos, 
                   away_points = PtsO, away_penalties = Pen, away_yards = Yds,
                   home_points = Pts, home_penalties = Pen1, home_yards = Yds1)

#reorder columns 
games_df <- select(games_df, date, position, home_team, away_team, home_points, home_penalties,
                   home_yards, away_points, away_penalties, away_yards, gameurl, urls)

#need to cut down games to unique one
#rewrite the file
save(games_df, file = "officials_games.rda")





###############################################################################
# game_info_df
###############################################################################
load("game_info.rda")

# removing conflicts with coercion 
game_info_df$won_toss <- gsub("(deferred)", "", game_info_df$won_toss, fixed = T)
game_info_df$OT_toss <- gsub("(deferred)", "", game_info_df$OT_toss, fixed = T)
game_info_df$won_toss <- gsub(" ()", "", game_info_df$won_toss, fixed = T)
game_info_df$OT_toss <- gsub(" ()", "", game_info_df$OT_toss, fixed = T)
game_info_df$attendance <- gsub(",", "", game_info_df$attendance)

#changing column types
game_info_df$attendance <- as.integer(game_info_df$attendance)
game_info_df$roof <- factor(game_info_df$roof)
game_info_df$surface <- factor(game_info_df$surface)

#fixing the factor levels of roof & surface
levels(game_info_df$roof)[levels(game_info_df$roof) == "retractable roof (closed)"] <- "closed roof"
levels(game_info_df$roof)[levels(game_info_df$roof) == "retractable roof (open)"] <- "open roof"

#look up what matrixturf and sportturf are
levels(game_info_df$surface)[levels(game_info_df$surface) == "a_turf"] <- "astroturf"
levels(game_info_df$surface)[levels(game_info_df$surface) == "astroplay"] <- "astroturf"
levels(game_info_df$surface)[levels(game_info_df$surface) == "astroplay "] <- "astroturf"
levels(game_info_df$surface)[levels(game_info_df$surface) == "fieldturf "] <- "fieldturf"
levels(game_info_df$surface)[levels(game_info_df$surface) == "grass "] <- "grass"




# cleaning weather column
weather_temp <- game_info_df$weather
weather_temp <- data.frame(weather_temp)
weather <- weather_temp %>% separate(weather_temp, c("degrees", "humidity", "wind", "wind chill"), sep =  ",")
weather$urls <- game_info_df$urls

## merging weather df to game_info_df
game_info_df <- merge(game_info_df, weather, by = "urls")

#cleaning out new separated weather columns
game_info_df$degrees <- gsub(" degrees", "", game_info_df$degrees)
game_info_df$humidity <- gsub("relative humidity ", "", game_info_df$humidity)
game_info_df$humidity <- gsub("%", "", game_info_df$humidity)
game_info_df$wind <- gsub("wind", "", game_info_df$wind)
game_info_df$wind <- gsub("mph", "", game_info_df$wind)
game_info_df$`wind chill` <- gsub("wind chill ", "", game_info_df$`wind chill`)




#changing duration from h:m to just min
duration_temp<-strsplit(game_info_df$duration, ":" )
hours<-sapply(duration_temp, "[[", 1)
hours <- as.integer(hours) * 60
min <- sapply(duration_temp, "[[", 2)
min <- as.integer(min)
game_info_df$duration_min <- hours + min


game_info_df$humidity <-trimws(game_info_df$humidity, "both")
game_info_df$wind <-trimws(game_info_df$wind, "both")
game_info_df$`wind chill` <-trimws(game_info_df$`wind chill`, "both")

game_info_df$temp <- as.integer(game_info_df$temp)
game_info_df$humidity <- as.integer(game_info_df$humidity)
game_info_df$wind <- as.integer(game_info_df$wind)
game_info_df$`wind chill` <- as.integer(game_info_df$`wind chill`)

#rename degrees to temp
game_info_df <- rename(game_info_df, temp = degrees)
game_info_df <- rename(game_info_df, gameurl = urls)

game_info_df <- select(game_info_df, -weather, -duration)
game_info_df <- select(game_info_df, won_toss, OT_toss, roof, surface, duration_min,
                       attendance, temp, humidity, wind, `wind chill`, urls)


#rewrite the file
save(game_info_df, file = "game_info.rda")

###############################################################################
# Combine game_info_df and games_df

combined_game_info_df <- merge(game_info_df, games_df, by = "gameurl")
save(combined_game_info_df, file = "Official_Games_Merged.rda")

complete_official_data <- merge(combined_game_info_df, officials_stats, by = "urls") 
#has 610105rows
#