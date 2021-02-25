rm(list = ls())
load('team_penalties_by_year.rda')

library(ggplot2)
library(scales)

p <- quickplot(team_name, sum(total_penalties), data =  team_penalties_by_year,
               geom = "line")
p


