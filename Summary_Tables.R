rm(list = ls())

load("Official_Games_Merged.rda")
load("Official_Stats_Merged.rda")

###############################################################################

combined_game_info_df$year <- format(combined_game_info_df$date, '%Y')

game_ref_df<-merge(officials_stats, combined_game_info_df, by.x=c("urls", "years"), by.y=c("urls", "year"))

save(game_ref_df, file = "game_ref_df.rda")


##Summary Table                   
game_ref_df <- group_by(game_ref_df, gameurl)
game_ref_df$start_year <- as.integer(game_ref_df$start_year)
#game_ref_df <- ungroup(game_ref_df)
summ <- summarize(game_ref_df, total_experience = sum(years-start_year)) 



game_df <- dplyr::select(combined_game_info_df, -urls, -position)
game_df <- dplyr::distinct(game_df)
game_df <- merge(game_df, summ, by = "gameurl")

save(game_df, file = "game_with_total_experience.rda")

###############################################################################
## adding variables highpenalty, home_win, and home advantage
################################################################################
load("game_with_total_experience.rda")

#total_penalties
for (i in game_df){
  game_df$total_penalties <- game_df$home_penalties + 
    game_df$away_penalties
}

#high penalty (target variable)
game_df$highpenalty <- ifelse(game_df$total_penalties >= 15, 1, 0)
#Home_win
game_df$home_win <- ifelse(game_df$home_points > game_df$away_points, 1, 0)
#home penalty advantage
game_df$home_advantage <- ifelse(game_df$away_penalties > 
                                                 game_df$home_penalties, 1, 0)

save(game_df, file = "game_with_total_experience.rda")

################################################################################
#Team Level analysis
################################################################################
load("game_with_total_experience.rda")
library(dplyr)

game_df$team_name <- game_df$home_team
game_df <- group_by(game_df, team_name)
Total_Home_Penalties <- summarize(game_df, total_home_penalties = sum(home_penalties))

game_df$team_name <- game_df$away_team
game_df <- group_by(game_df, team_name)
Total_Away_Penalties <- summarize(game_df, total_away_penalties = sum(away_penalties))


team_penalties <- merge(Total_Home_Penalties, Total_Away_Penalties, by = "team_name")

#Rams from st. Louis -> LA
#Raiders from LA -> Oakland
#Washington Redskins -> Washington Football Team
#Chargers from San Diego -> LA



game_df <- ungroup(game_df)
game_df <- group_by(game_df, year)
years_home <- summarize(game_df, total_home_penalites = sum(home_penalties))

game_df <- ungroup(game_df)
game_df <- group_by(game_df, year)
years_away <- summarize(game_df, total_away_penalites = sum(away_penalties))

years_penalties <- merge(years_home, years_away, by = "year")

game_df <- group_by(game_df, year)
years_total <- summarize(game_df, total_penalties = sum(total_penalties))

years_penalties <- merge(years_penalties, years_total, by = 'year')

################################################################################
#Team penalties by year - home & away
################################################################################
game_df$team_name <- game_df$home_team
game_df <- group_by(game_df, team_name, year)
home_penalties_by_year <- summarize(game_df, total_home_penalties = sum(home_penalties),
                home_yards = sum(home_yards))

game_df$team_name <- game_df$away_team
game_df <- group_by(game_df, team_name, year)
away_penalties_by_year <- summarize(game_df, total_away_penalties = sum(away_penalties),
                                    away_yards = sum(away_yards))

team_penalties_by_year <- merge(home_penalties_by_year, away_penalties_by_year, 
                                by = c("team_name", "year"))

team_penalties_by_year$total_penalties <- (team_penalties_by_year$total_home_penalties + 
                                             team_penalties_by_year$total_away_penalties)
team_penalties_by_year$total_yards <- (team_penalties_by_year$home_yards + team_penalties_by_year$away_yards)


save(team_penalties_by_year, file = "team_penalties_by_year.rda")
readr::write_csv(team_penalties_by_year, "team_penalties_by_year.csv")


