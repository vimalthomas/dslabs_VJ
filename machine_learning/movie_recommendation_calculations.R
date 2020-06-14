#Compute the number of ratings for each movie and then plot it against the year the movie came out. Use the square root transformation on the counts.

#What year has the highest median number of ratings? 1995

movielens%>%select(movieId,title,year,rating)%>%
  group_by(movieId)%>%summarize(rat_count=n(),
                                as.character(first(year)),
                                first(title))%>%ungroup()%>%
  mutate(year = `as.character(first(year))`,
         title= `first(title)`)%>%select(movieId,rat_count,year,title)%>%
  filter(year>=1993)%>%
  ggplot(aes(year,rat_count,group=year))+
  geom_boxplot()+
  scale_y_continuous("sqrt")

                                
#Among movies that came out in 1993 or later, 
#what are the 25 movies with the most ratings per year,
#and what is the average rating of each of the top 25 movies?
data("movielens")
movielens%>%select(movieId,title,year,rating)%>%
  group_by(movieId)%>%summarize(rat_count=n(),avg_rat=mean(rating),
                                as.character(first(year)),
                                first(title))%>%ungroup()%>%
  mutate(year = `as.character(first(year))`,
         title= `first(title)`,
         avg_rat_year=rat_count/(2018-as.numeric(year)))%>%select(movieId,rat_count,avg_rat,avg_rat_year,year,title)%>%
  filter(year>=1993)%>%arrange(desc(rat_count))

#stratifying by rat_per_year
data("movielens")
movielens%>%select(movieId,title,year,rating)%>%
  group_by(movieId)%>%summarize(rat_count=n(),avg_rat=mean(rating),
                                as.character(first(year)),
                                first(title))%>%ungroup()%>%
  mutate(year = `as.character(first(year))`,
         title= `first(title)`,
         avg_rat_year=rat_count/(2018-as.numeric(year)))%>%select(movieId,rat_count,avg_rat,avg_rat_year,year,title)%>%
  filter(year>=1993)%>%arrange(desc(rat_count))%>%group_by(avg_rat_year)%>%
  summarize(first(avg_rat_year),n=mean(avg_rat))%>%ggplot(aes(`first(avg_rat_year)`,n))+
  geom_point()+
  geom_smooth()


#Compute the average rating for each week and plot this average against day. Hint: use the round_date function before you group_by.

#What type of trend do you observe?
movielens <- mutate(movielens, date = as_datetime(timestamp))
  
library(lubridate)
head(movielens)
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#average and standard error of the category.


movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


