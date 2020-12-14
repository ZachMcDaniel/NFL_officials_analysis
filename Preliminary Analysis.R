rm(list=ls())

load("Official_Games_Merged.rda")
load("Official_Stats_Merged.rda")

################################################################################
#Creating Target Variable for Games
################################################################################

#creating a target variable based on total penalties > 15 per game

#total_penalties
for (i in combined_game_info_df){
  combined_game_info_df$total_penalties <- combined_game_info_df$home_penalties + 
                                           combined_game_info_df$away_penalties
}

#high penalty (target variable)
combined_game_info_df$highpenalty <- ifelse(combined_game_info_df$total_penalties >= 15, 1, 0)
#Home_win
combined_game_info_df$home_win <- ifelse(combined_game_info_df$home_points > combined_game_info_df$away_points, 1, 0)
#home penalty advantage
combined_game_info_df$home_advantage <- ifelse(combined_game_info_df$away_penalties > 
                                                 combined_game_info_df$home_penalties, 1, 0)

##Subset complete weather cases
Weather <- dplyr::filter(combined_game_info_df, !is.na(combined_game_info_df$temp) &
                           !is.na(combined_game_info_df$humidity) & !is.na(combined_game_info_df$wind))
readr::write_csv(Weather,"Game_Weather_Rattle.csv")
##Subset complete attendance cases
Attendance <- dplyr::filter(combined_game_info_df, !is.na(combined_game_info_df$attendance))

readr::write_csv(Attendance,"Game_Attendance_Rattle.csv")


#tests
#temp - high penalty  YES p = 2.2e-16
t.test(Weather$temp[Weather$highpenalty == 0], Weather$temp[Weather$highpenalty == 1])
#temp - home advantage NO p = .312
t.test(Weather$temp[Weather$home_advantage == 0], Weather$temp[Weather$home_advantage == 1])
#wind - high penalty YES p= 7.36e-13
t.test(Weather$wind[Weather$highpenalty == 0], Weather$wind[Weather$highpenalty == 1])
#wind - home advantage NO p = .14
t.test(Weather$wind[Weather$home_advantage == 0], Weather$wind[Weather$home_advantage == 1])
#attendance - high penalty YES p = .0007
t.test(Weather$attendance[Weather$highpenalty == 0], Weather$attendance[Weather$highpenalty == 1])
#attendance - home_advantage YES p.0013
t.test(Weather$attendance[Weather$home_advantage == 0], Weather$attendance[Weather$home_advantage == 1])
#home win - home advantage YES p = 2.2e-16
t.test(Weather$home_win[Weather$home_advantage == 0], Weather$home_win[Weather$home_advantage == 1])




###Reorder for simplicity on rattle
combined_game_info_df <- dplyr ::select(combined_game_info_df,
                                urls, gameurl, date,
                                won_toss, OT_toss,
                                roof, surface, duration_min,
                                attendance, temp, humidity, wind, `wind chill`,
                                position, home_team, away_team,
                                home_points, home_penalties, home_yards,
                                away_points, away_penalties, away_yards,
                                total_penalties, highpenalty)

###############################################################################
# Creating target variable for Official stats
###############################################################################
#by average penalties > average league penalties

#High penalty is the target variable labeling high penalty games
officials_stats$highpenalty <- ifelse( officials_stats$avg_penalties >= officials_stats$league_avg_penalties , 1, 0)

#Tenure is how long the official has worked up to that year
officials_stats$tenure <- officials_stats$years - as.integer(officials_stats$start_year)

#average yard per penalty
officials_stats$avg_yard_per_penalty <- officials_stats$penalty_yards / officials_stats$total_penalties

#harsh is if the ref calls higher yard penalties with an average penalty per yard >=10
officials_stats$harsh <- ifelse( officials_stats$avg_yard_per_penalty >= 10, 1, 0)

#Home Bias is penalizing away teams more than 50% of penalties called and home wp >= league home wp
officials_stats$home_bias <- ifelse( officials_stats$home_penalties <= 50.00, 1, 0 & officials_stats$home_wp >= officials_stats$league_home_wp)




#tests on tenure and high penalty
cor(officials_stats$tenure, officials_stats$avg_penalties)
t.test(officials_stats$tenure[officials_stats$highpenalty == 0], officials_stats$tenure[officials_stats$highpenalty == 1])

#Tests on high penalty and avg yard per penalty
cor(officials_stats$highpenalty, officials_stats$avg_yard_per_penalty)
t.test(officials_stats$avg_yard_per_penalty[officials_stats$highpenalty == 0], officials_stats$avg_yard_per_penalty[officials_stats$highpenalty == 1])

#Tests on harshness
cor(officials_stats$home_bias, officials_stats$tenure)
t.test(officials_stats$tenure[officials_stats$home_bias == 0], officials_stats$tenure[officials_stats$home_bias == 1])
t.test(officials_stats$home_bias[officials_stats$highpenalty == 0], officials_stats$home_bias[officials_stats$highpenalty == 1])
cor(officials_stats$home_bias, officials_stats$harsh)

t.test(officials_stats$tenure[officials_stats$harsh == 0], officials_stats$tenure[officials_stats$harsh == 1])
t.test(officials_stats$home_bias[officials_stats$harsh == 0], officials_stats$home_bias[officials_stats$harsh == 1])

officials_stats$start_year <- as.factor(officials_stats$start_year)
t.test(officials_stats$start_year[officials_stats$harsh == 0], officials_stats$start_year[officials_stats$harsh == 1])

#group years as cohorts
table(as.factor(officials_stats$start_year), officials_stats$harsh)

readr::write_csv(officials_stats, "Official_Stats_Rattle.csv")





