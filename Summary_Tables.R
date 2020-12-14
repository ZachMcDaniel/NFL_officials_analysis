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

load("game_with_total_experience.rda")
