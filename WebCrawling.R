rm(list=ls())

# Load packages
library(xml2) 
library(httr) 



# officials_df
load("officials_overview.rda")

###############################################################################
#Official stats
###############################################################################


stats_df<-data.frame(years=character(0),games=character(0),
                      playoff_games=character(0),position=character(0),
                      home=character(0),visitor=character(0),
                      home_penalties=character(0),home_wp=character(0),
                      total_penalties=character(0),penalty_yards=character(0),
                      avg_penalties=character(0),avg_yards=character(0),
                      league_home_penalties=character(0),league_home_wp=character(0),
                      league_avg_penalties=character(0),league_avg_yards=character(0),
                      urls=character(0))


for (u in officials_df$urls) {
  print(u) 
  url<-GET(u) 
  Sys.sleep(10) 
  page<-read_html(url)
  
  
  years<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/th"))
  games<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[1]"))
  playoff_games<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[2]"))
  position<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[3]"))
  home<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[4]"))
  visitor<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[5]"))
  home_penalties<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[6]"))
  home_wp<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[7]"))
  total_penalties<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[8]"))
  penalty_yards<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[9]"))
  avg_penalties<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[10]"))
  avg_yards<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[11]"))
  league_home_penalties<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[12]"))
  league_home_wp<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[13]"))
  league_avg_penalties<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[14]"))
  league_avg_yards<-xml_text(xml_find_all(page, "//table[@id='official_stats']//tbody/tr/td[15]"))
  
  
  stats_temp<-data.frame(years, games, playoff_games, position,
                         home, visitor, home_penalties, home_wp,
                         total_penalties,penalty_yards, avg_penalties,
                         avg_yards, league_home_penalties ,league_home_wp,
                         league_avg_penalties,league_avg_yards)
  
 
  stats_temp$urls<-rep(u,nrow(stats_temp))
  
  stats_df<-rbind(stats_df, stats_temp)
}


save(stats_df, file="officials_stats.rda")
write.csv(stats_df, "officals_stats.csv", row.names=FALSE)

################################################################################
#Games
################################################################################

games_df <- data.frame(Date = character(0), 
                       Tm = character(0), 
                       Opp = character(0), 
                       Pos = character(0),
                       PtsO =  character(0), 
                       Pen =  character(0),
                       Yds =  character(0),
                       Pts =  character(0),
                       Pen1 =  character(0),
                       Yds1 =  character(0),
                       gameurl = character(0))


for (i in officials_df$urls) {
  print(i) 
  url<-GET(i) 
  Sys.sleep(5) 
  page<-read_html(url)
  
  table_comment <- xml_find_all(page, "//div[@id='all_games']//comment()")
  table_comment_clean<- gsub("<!--" , "", table_comment)
  table_comment_clean2 <- gsub("-->", "", table_comment_clean)
  page2 <- read_html(table_comment_clean2)
  
  Date<-xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]/th"))
  Tm <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[1]"))
  Opp <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[2]"))
  Pos <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[3]"))
  PtsO <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[4]"))
  Pen <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[5]"))
  Yds <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[6]"))
  Pts <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[7]"))
  Pen1<- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[8]"))
  Yds1 <- xml_text(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]//td[9]"))
  gameurl <- xml_attr(xml_find_all(page2, "//table[@id='games']//tbody/tr[not(@class='thead')]/th/a"), "href")
  gameurl <- paste("https://www.pro-football-reference.com", gameurl, sep="")


  
  games_temp <- data.frame(Date, 
                           Tm, 
                           Opp, 
                           Pos,
                           PtsO, 
                           Pen,
                           Yds,
                           Pts,
                           Pen1,
                           Yds1,
                           gameurl)
  
  games_temp$urls<-rep(i,nrow(games_temp))
  
  games_df<-rbind(games_df, games_temp)
  
}

save(games_df, file="officials_games.rda")
write.csv(games_df, "officals_games.csv", row.names=FALSE)


###############################################################################
#Game Info
###############################################################################

# games_df
load("officials_games.rda")

game_info_df <- data.frame(won_toss = character(0),
                           OT_toss = character(0),
                           roof = character(0),
                           surface = character(0),
                           duration = character(0), 
                           attendance = character(0),
                           weather = character(0),
                           urls = character(0))

# Removing duplicate games
urls_unique <- unique(games_df$gameurl)
urls_temp <- urls_unique[4183:5646]
for (i in urls_temp) {
  print(i)
  url <- GET(i)
  Sys.sleep(5)
  pg <- read_html(url)
  
  table_comment <- xml_find_all(pg, "//div[@id='all_game_info']//comment()")
  table_comment_clean<- gsub("<!--" , "", table_comment)
  table_comment_clean2 <- gsub("-->", "", table_comment_clean)
  page2 <- read_html(table_comment_clean2)
  
  won_toss_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Won Toss')]/following-sibling::td")
  if (length(won_toss_temp) > 0) {
    won_toss <- xml_text(won_toss_temp)
  }
  else{
    won_toss <- NA
  }
  
  OT_toss_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Won OT Toss')]/following-sibling::td" )
  if (length(OT_toss_temp)>0) {
    OT_toss <- xml_text(OT_toss_temp)
  }
  else {
    OT_toss <- NA
  }
  
  roof_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Roof')]/following-sibling::td" )
  if (length(roof_temp)>0) {
    roof <- xml_text(roof_temp)
  }
  else {
    roof <- NA
  }
  
  surface_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Surface')]/following-sibling::td" )
  if (length(surface_temp)>0) {
    surface <- xml_text(surface_temp)
  }
  else {
    surface <- NA
  }
  
  duration_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Duration')]/following-sibling::td" )
  if (length(duration_temp)>0) {
    duration <- xml_text(duration_temp)
  }
  else {
    duration <- NA
  }
  
  attendance_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Attendance')]/following-sibling::td" )
  if (length(attendance_temp)>0) {
    attendance <- xml_text(attendance_temp)
  }
  else {
    attendance <- NA
  }
  
  weather_temp <- xml_find_all(page2, "//table[@id='game_info']//tr/th[contains(text(), 'Weather')]/following-sibling::td" )
  if (length(weather_temp)>0) {
    weather <- xml_text(weather_temp)
  }
  else {
    weather <- NA
  }
  
  game_info_temp <- data.frame(
    won_toss,
    OT_toss,
    roof,
    surface,
    duration,
    attendance,
    weather)
  
  game_info_temp$urls<-rep(i,nrow(game_info_temp))
  
  game_info_df<-rbind(game_info_df, game_info_temp)
}

#save the df
save(game_info_df, file="game_info.rda")
#write.csv(game_info_df, "game_info.csv", row.names=FALSE)


