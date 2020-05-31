
#Load the Lahman library and filter the Teams data frame to the years 1961-2001. Run a linear model in R predicting the number of runs per game based on both the number of bases on balls per game and the number of home runs per game.



dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(RG=R/G, BBG = BB/G, HG=HR/G)

fit<-dat%>%lm(RG~BBG+HG,data=.)

dat
fit

#another sample model predictions with confidence interval ggplot vs manual.

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

p1<-ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

p2<-galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

grid.arrange(p1,p2,ncol=2)



