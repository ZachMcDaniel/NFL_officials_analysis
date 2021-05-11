rm(list = ls())
load('team_penalties_by_year.rda')

team_penalties_by_year <- team_penalties_by_year[team_penalties_by_year$year != 2020,]

################################################################################
#TOTAL PENALTIES
fit<-aov(total_penalties~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)

write.csv(df, file = "team_vs_total_penalties.csv", row.names = TRUE)

bartlett.test(total_penalties~team_name,data=team_penalties_by_year)
#we do not have equal variance <.05 = we do not have equal variance
kruskal.test(total_penalties~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$total_penalties)
### testing assumptions - > .05 = data is normal

################################################################################
# TOTAL YARDS
fit<-aov(total_yards~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)


df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)
write.csv(df, file = "team_vs_total_yards.csv", row.names = TRUE)

bartlett.test(total_yards~team_name,data=team_penalties_by_year)
#we do not have equal variance
kruskal.test(total_yards~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$total_yards)
### testing assumptions - > .05 = data is normal

################################################################################
# AWAY penalties
fit<-aov(total_away_penalties~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)


df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)
write.csv(df, file = "team_vs_away_penalties.csv", row.names = TRUE)

bartlett.test(total_away_penalties~team_name,data=team_penalties_by_year)
#we do have equal variance
kruskal.test(total_away_penalties~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$total_away_penalties)
### testing assumptions - > .05 = data is normal
#data is not normal?

###############################################################################
#home penalties
fit<-aov(total_home_penalties~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)
write.csv(df, file = "team_vs_home_penalties.csv", row.names = TRUE)

bartlett.test(total_home_penalties~team_name,data=team_penalties_by_year)
#we do not have equal variance
kruskal.test(total_home_penalties~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$total_home_penalties)
#data is not normal?

################################################################################
#Away Yards
fit<-aov(away_yards~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)
write.csv(df, file = "team_vs_away_yards.csv", row.names = TRUE)

bartlett.test(away_yards~team_name,data=team_penalties_by_year)
#we do have equal variance
kruskal.test(away_yards~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$away_yards)
#data is not normal?


################################################################################
#Home yards
fit<-aov(home_yards~team_name,
         data=team_penalties_by_year) # Specify data frame
class(fit) # Object of class "aov"

# Extract specific attributes with $
fit$coefficients

# Easier to use summary()
summary(fit)

df <- TukeyHSD(fit)
class(df$team_name)
df <- as.data.frame(df$team_name)
write.csv(df, file = "team_vs_home_yards.csv", row.names = TRUE)

bartlett.test(home_yards~team_name,data=team_penalties_by_year)
#we do not have equal variance
kruskal.test(home_yards~team_name,data=team_penalties_by_year)
#there is a significant difference in teams <.05 = there is a stat signf diff
shapiro.test(team_penalties_by_year$home_yards)
#data is not normal?

#################################################################################



