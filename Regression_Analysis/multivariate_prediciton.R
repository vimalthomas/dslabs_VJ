library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#fitting the line
fit<-Teams_small%>% mutate(R = R/G,HR=HR/G)%>%lm(avg_attendance ~ R + HR + W+ yearID, data = .)

#pipeline for variables if Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.What would this team's average attendance be in 2002?
df <- data.frame(R = 5, HR =1.2, W = 80, yearID = 2002)

#prediciton
predict(fit,df)
