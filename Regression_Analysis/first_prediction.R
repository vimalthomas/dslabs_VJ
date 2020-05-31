#predicting singles and bbs based on previous data.

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#create a similar table like bat_02 to get the historical data.
bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%group_by(yearID)%>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(yearID,playerID, singles, bb)

#refine further the reslut data by sumarizing it by season.

bat_12<-bat_01%>%group_by(playerID)%>%mutate(mean_singles=mean(singles),mean_bb=mean(bb))%>%distinct(playerID,mean_singles,mean_bb)

#join historical data with current data 2002
result1<-bat_02%>%inner_join(bat_12,by="playerID")

#find sample corralations to see if there is preliminary relationship
result1%>%summarize(r=cor(singles,mean_singles))

result1%>%summarize(r=cor(bb,mean_bb))

#fit the model for singles and bb
fit1<-result1%>%lm(singles ~mean_singles,data=.)

fit2<-result1%>%lm(bb ~mean_bb,data=.)

#predict model based on fit line
pred_y1<-predict(fit1)
pred_y2<-predict(fit2)


df1<-data.frame(predicted_singles=pred_y1,actual_singles=result1$singles)

df1%>%ggplot(aes(actual_singles,predicted_singles))+
  geom_line()

df2<-data.frame(predicted_bb=pred_y2,actual_bb=result1$bb)

df2%>%ggplot(aes(actual_bb,predicted_bb))+
  geom_point()






