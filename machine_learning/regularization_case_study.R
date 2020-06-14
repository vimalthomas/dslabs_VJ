library(dslabs)
library(tidyverse)
library(dplyr)


options(digits=7)
set.seed(1986, sample.kind="Rounding")

n <- round(2^rnorm(1000, 8, 1))

#creating quality of schools

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))




#assigning normally distributed test scores based on the school quality

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
str(scores)


schools <- schools %>% mutate(score = sapply(scores, mean))
schools

#find top to schools based on average score
top10<-schools%>% top_n(10,score) %>% arrange(desc(score))

#find the worst10 schol
worst10<-schools%>% top_n(10,-score) %>% arrange((score))
worst10

#median school size between overall and top10 and worst
median(top10$size)
median(schools$size)
median(worst10$size)

schools%>%ggplot(aes(score,size))+
  geom_point()+
  geom_point(data = filter(schools, rank<=10), col = 2)
#we see that smaller sized features create more variability.
#lets regularize the dataset
#overall means

overall <- mean(sapply(scores, mean))
overall
alpha<-25
schools_reg <- schools %>% 
  mutate(score = sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha)))
top10_reg<-schools_reg%>% top_n(10,score) %>% arrange(desc(score))

top10_reg

schools_reg%>%ggplot(aes(score,size))+
  geom_point()+
  geom_point(data = filter(schools, rank<=10), col = 2)
#the above analysis ensures that regulization works a bit but want to find an apt alpha value.

#now let us write a function to see which could be a best alpha value.
head(schools_reg)

RMSE<- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

rmse_reg <- RMSE(schools_reg$score, overall)
rmse_reg
schools$quality
alpha_index<-seq(10,250,1)
alpha_index
rmse_reg_index<-sapply(alpha_index,function(y){
  
  
  schools_reg_index <- schools %>% 
    mutate(score = sapply(scores, function(x) sum(x)/(length(x)+y)))
  schools_reg_index
  
  rmse_reg_index <- RMSE(schools_reg_index$score, schools_reg_index$quality)
  rmse_reg_index
  
})


str(rmse_reg_index)
str(alpha_index)

df_result<-data.frame(alpha_values=alpha_index,rmse_values=rmse_reg_index)

df_result%>%arrange(rmse_reg_index)
#this reveals that 135 is the optimized alpha value.

#best value alpha is applied


overall <- mean(sapply(scores, mean))
overall
alpha<-135
schools_reg <- schools %>% 
  mutate(score = sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha)))
top10_reg<-schools_reg%>% top_n(10,score) %>% arrange(desc(score))

top10_reg

schools_reg%>%ggplot(aes(score,size))+
  geom_point()+
  geom_point(data = filter(schools, rank<=10), col = 2)






