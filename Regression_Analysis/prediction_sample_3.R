library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#predicting average attendance by runs per game
Teams_small%>% mutate(R = R/G)%>%
  do(tidy(lm(avg_attendance ~ R, data = .), conf.int = TRUE))

#predicting average attandace by homeruns per game
Teams_small%>% mutate(HR = HR/G)%>%
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = TRUE))
#predicting average attendance by wins
Teams_small%>% mutate(W = W)%>%
  do(tidy(lm(avg_attendance ~ W, data = .), conf.int = TRUE))

#if a team does not win any game, then the attandance is the plain intercept because its b0+wb1 here w is 0.

#prediction by year
Teams_small%>% mutate(yearID = yearID)%>%
  do(tidy(lm(avg_attendance ~ yearID, data = .), conf.int = TRUE))


#correlation check
Teams_small%>% mutate(R = R/G)%>%summarize(r = cor(W, R))
Teams_small%>% mutate(HR = HR/G)%>%summarize(r = cor(W, HR))

#stratification by wins

#prediction by runs per game
Teams_small%>% mutate(W1=round(W/10))%>%filter(W1 %in% 5:10)%>%
  group_by(W1)%>%
  mutate(R = R/G)%>%
  do(tidy(lm(avg_attendance ~ R, data = .), conf.int = TRUE))%>%filter(term=='R')

#prediction by HR per game
Teams_small%>% mutate(W1=round(W/10))%>%filter(W1 %in% 5:10)%>%
  group_by(W1)%>%
  mutate(HR = HR/G)%>%
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = TRUE))%>%filter(term=='HR')

#correlation check - Teams_small%>% mutate(W1=round(W/10))%>%filter(W1 %in% 5:10)%>%

Teams_small%>% mutate(W1=round(W/10))%>%filter(W1 %in% 5:10)%>%
  group_by(W1)%>%
  mutate(R = R/G)%>%summarize(r = cor(R, avg_attendance))


Teams_small%>% mutate(W1=round(W/10))%>%filter(W1 %in% 5:10)%>%
  group_by(W1)%>%
  mutate(HR = H/G)%>%summarize(r = cor(HR, avg_attendance))

#Runs per games effect on Wins starte and actual wins correlation


Teams_small%>% mutate(W1=round(W/10))%>%
  group_by(W1)%>%filter(W1 ==6)%>%
  mutate(HR = HR/G)%>%select(avg_attendance,HR)%>% arrange(desc(avg_attendance))
  
  %>%summarize(Hr = cor(HR, avg_attendance))

#checking if teams with similar number of wins, have larger average attendance if they have more HR









