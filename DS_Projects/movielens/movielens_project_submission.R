################################
# Movielens Project Submission
#Author: Vimal Thomas Joseph
################################

# This project is to predict movie rating based on user, movies and other features.

# The Model generation code consists of the following modules

#1) Library Management
#2) Function Loading
#3) Master Data Preparation
#4) Data Analysis and Visualization
#5) Creation of training and testing datasets
#6) Machine Learning Models based on feature effects
#7) Creation of Ensemble 

#######################################
## 1. Library Management
####################################### 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyr)
library(dplyr)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(ggthemes)
library(recosystem)

#######################################
# 2.Master Data Creation
#######################################

#Data for the model has been downloaded from grouplens.org. 
#The downloaded dataset consists of the following.

#1. User Id - Id of the user.
#2. Movie Id - Id of the Movie.
#3. Rating - Rating from 0.5 to 5 (in 0.5 interval).
#4. Timestamp - Timestamp of the rating in UNIX format.
#5. Title - Title of the movie with release year in brackets.
#6. Genres - Genres of the movie delimited by pipe.

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#Validaition Set Creation

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#Making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########--------------#################
##3. Function Loading##
##########--------------#################

RMSE<-function(true_rating,predicted_rating){
  sqrt(mean((true_rating-predicted_rating)^2))
  
}

########################################
#4. Data Preparations
########################################

#To understand  the summary of the edx dataset
head(edx)


#To create multiple rows based on genres data for each movie rating
edx_wrangled<-edx%>%separate_rows("genres",sep = "\\|")

#Adding number of genres for each movie rating 
edx_gen_count<-edx_wrangled%>%group_by(movieId,userId)%>%
  summarize(genres_count=n())

#Converting timestamp to proper date format
edx_wrangled1<-edx%>%
  inner_join(edx_gen_count,by = c("movieId","userId"))%>%
  mutate(date = as_datetime(timestamp),
         year=substring(str_extract(title,'\\([0-9]{4}'),2,5))

edx_wrangled<-edx_wrangled1


#data before wrangling
head(edx)

#data after wranglng
edx_wrangled%>%
  select(userId,movieId,rating,date,title,genres,year,genres_count)%>%
  head(10)


##############################################
# Data Visualizations
##############################################

##Looking at the summary and wrangled data, some of the points to note are,

#1) average rating is above 3.5, 
#2) There are no 0 ratings for any movie
#3) There are multiple genres for most of the movies delimited with a pipe
#4) The release year of the movie is embadded in the title
#5) The rating date is given in a timestamp format which should be converted. 

#To further Anlayze the dataset, let us first look at the full picture of these movie ratings.

#approved - full bar diagram of rating count
#full bar diagram of rating count

options(scipen = 999)
edx_wrangled%>%filter(year >=1900 & year < 2020)%>%
  summarize(year,year_count=round(n()))%>%
  ggplot(aes(year))+
  geom_bar(position = position_dodge(width = 1), width=0.5)+
  xlab("Release Year") +
  ylab("Total number of rating") +
  ggtitle("Movie Ratings by Year ") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

#This clearly states that the count of ratings have increased between 1970 and 2000. The downtrend starts from 2000 again.
#The next look at this dataset would be based on the average rating by year.

#approved average rating by release year  - as 
edx_wrangled%>%
  filter(year >=1971 & year <= 2008)%>%
  group_by(year)%>%summarize(avg_rating=mean(rating),.groups = "keep")%>%
  ggplot()+
  geom_point(aes(x=year,y=avg_rating,color=avg_rating))+
  xlab("Release Year") +
  ylab("Average Ratings") +
  ggtitle("Average Rating by Year ") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


#side by side view of graphs.
p4<-edx_wrangled%>%select(year,movieId,rating)%>%
  group_by(movieId)%>%summarize(year=first(year),
                                avg_rating=mean(rating),
                                count_rating=n(),
                                .groups = "keep")%>%
  filter(year>=1971 & year < 2008 & count_rating<1000 )%>%
  ggplot(aes(x=avg_rating,
             y=count_rating,
             color=avg_rating))+
  geom_point(aes(color=avg_rating),
             show.legend = FALSE,
             position = position_dodge(width = 1),size=2)+
  xlab("Average Rating") +
  ylab("Rating Count") +
  scale_size(0.5)+
  theme_economist()

p5<-edx_wrangled%>%select(year,movieId,rating)%>%
  group_by(movieId)%>%summarize(year=first(year),avg_rating=mean(rating),count_rating=n(),.groups = "keep")%>%
  filter(year>=1971 & year < 2008 & count_rating>1000)%>%
  ggplot(aes(x=avg_rating,y=count_rating))+
  geom_point(aes(color=avg_rating),show.legend = FALSE,position = position_dodge(width = 1),size=2)+
  xlab("Average Rating") +
  ylab("Rating Count") +
  scale_size(0.5)+
  xlim(1,5)+
  theme_economist()

grid.arrange(p4,p5,ncol=2)

#################################
# Model Approach
################################## 

#creationg of training and testing datasets.

set.seed(75, sample.kind = "Rounding") 
test_index <- createDataPartition(y = edx_wrangled$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx_wrangled[-test_index,]
test_set <- edx_wrangled[test_index,]

train_set<-train_set%>%mutate(train_set, date = as_datetime(timestamp))
test_set<-test_set%>%mutate(test_set, date = as_datetime(timestamp))

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set,by='date')%>%
  semi_join(train_set,by='genres')
#dim(train_set)
#dim(test_set)


#Model 1 - Average_Rating_System
#----------------------------------------------
mu_hat<-mean(train_set$rating)


#Calculating RMSE for Model 1
model_rmse<-(RMSE(mu_hat,test_set$rating))

RMSE_Table<-data.frame(Model_Name = 'Basic_Average',RMSE_Value=model_rmse)
RMSE_Table

# Model 2 - Average Rating + Movie Effect
#----------------------------------------
mu<-mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu ),.groups = "keep")


prediction_values<-test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i ) %>%
  .$pred

model_rmse<-RMSE(test_set$rating, prediction_values)

RMSE_Table<-rbind(RMSE_Table,data.frame(Model_Name = 'Basic_with_Movie_Effect',RMSE_Value=model_rmse))
RMSE_Table

# Model 3 - Average Rating + Movie Effect + User Effect
#-----------------------------------------------------

  user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i),.groups = "keep")

prediction_values <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_rmse<-RMSE(test_set$rating, prediction_values)

RMSE_Table<-rbind(RMSE_Table,data.frame(Model_Name = 'Basic_with_Movie_and_user_effect',RMSE_Value=model_rmse))
RMSE_Table


# Model 4: Average Rating + Movie Effect + User Effect + Genres Effect
#--------------------------------------------------------------------
  
  #introducing genres effect
  genres_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(g_u = mean(rating - mu - b_i - b_u),.groups = "keep")

prediction_values <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs,by='genres')%>%
  mutate(pred = mu + b_i + b_u + genres_count*g_u) %>%
  .$pred


model_rmse<-RMSE(test_set$rating, prediction_values)

RMSE_Table<-rbind(RMSE_Table,data.frame(Model_Name = 'Basic_with_Movie_User_and_genres_effect',RMSE_Value=model_rmse))
RMSE_Table



# Model 5: Introducing date effect. 
#--------------------------------------

week_avg <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs,by='genres')%>%
  group_by(date) %>%
  summarize(d_ui = mean(rating - mu - b_i - b_u - g_u),.groups = "keep")

week_avg

prediction_values <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs,by='genres')%>%
  left_join(week_avg, by='date')%>%
  mutate(pred = mu + b_i + b_u + genres_count*g_u + d_ui) %>%
  .$pred


model_rmse<-RMSE(test_set$rating, prediction_values)

RMSE_Table<-rbind(RMSE_Table,data.frame(Model_Name = 'Basic_with_Movie_user_genres_and_date_effect',RMSE_Value=model_rmse))
RMSE_Table

# Model 6: Matrix Factorization with recosystem package
#------------------------------------------------------

recommender<-Reco()

#converting user id and movie id as numeric indexes
train_small<-train_set%>%mutate(userId=as.numeric(userId))%>%select(userId,movieId,rating)
test_small<-test_set%>%mutate(userId,as.numeric(userId))%>%select(userId,movieId)


#creating matrix for train and test sets.
train_small_m<-as.matrix(train_small)
test_set_m<-as.matrix(test_small)

#writing data to an operating system file.
write.table(train_small_m,
            file = "train_set.txt",
            sep = " ", 
            row.names = FALSE, 
            col.names = FALSE)
write.table(test_set_m, 
            file = "test_set.txt",
            sep = " ",
            row.names = FALSE, 
            col.names = FALSE)

#reading from the files written
training_dataset <- data_file("train_set.txt")
test_dataset <- data_file("test_set.txt")
predict_dataset<-tempfile()

#training - using default parameters.
recommender$train(training_dataset,opts = c(costp_12=0.1,costq_12=0.1,lrate=0.1,niter=100,nthread=6,verbose=F))

#predicting
recommender$predict(test_dataset, out_file(predict_dataset))

#write predicted values into pred_rating
pred_rating<-scan(predict_dataset)

model_rmse<-RMSE(test_set$rating, round(pred_rating,1))
RMSE_Table<-rbind(RMSE_Table,data.frame(Model_Name = 'Matrix Factorization with recosystem',RMSE_Value=model_rmse))

RMSE_Table

#####################################
# Model Performances and Fine-Tuning.
#####################################
#Based on the RMSEs for each models, it is evident that adding date effect worsen 
#the RMSE expectation. For now, let us remove date effect from our model list and 
#continue fine-tuning part.
# determining lamda
l<-seq(3.5,6.5,0.25)

#function to loop through lambda values
result<-sapply(l,function(lam){
  mu<-mean(train_set$rating)
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu )/(n()+lam),.groups = "keep")
  
  
  #introducing userid effect
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lam),.groups = "keep")
  
  
  #introducing genres effect
  genres_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(g_u = sum(rating - mu - b_i - b_u)/(n()+lam),.groups = "keep")
  
  predicted_values <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u ) %>%
    .$pred
  
  return(RMSE(test_set$rating, predicted_values))
})


#Plotting the result and lambda values.
plot(l,result)



#############################################################
# Final Model based on feature effects with validation set.
############################################################


# determining lamda
lam<-4.75

#function to loop through lambda values

mu<-mean(edx_wrangled$rating)
movie_avgs <- edx_wrangled %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu )/(n()+lam),.groups = "keep")


#introducing userid effect
user_avgs <- edx_wrangled %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lam),.groups = "keep")


#introducing genres effect
genres_avgs <- edx_wrangled %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(g_u = sum(rating - mu - b_i - b_u)/(n()+lam),.groups = "keep")

predicted_values <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs,by='genres')%>%
  mutate(pred = mu + b_i + b_u + g_u ) %>%
  .$pred

RMSE(validation$rating, predicted_values)

##################################################
# Additional Final Model 2 with recommender package and validation dataset.
##################################################


recommender<-Reco()

#creating training and test set based on edx and validation set.
train_small<-edx_wrangled%>%mutate(userId=as.numeric(userId))%>%select(userId,movieId,rating)
test_small<-validation%>%mutate(userId,as.numeric(userId))%>%select(userId,movieId)  

#Converting the datasets into matrix
train_small_m<-as.matrix(train_small)
test_set_m<-as.matrix(test_small)

#writing the matrices as files in the operating system to be used in the training and prediction.
write.table(train_small_m, 
            file = "train_set.txt", 
            sep = " ", 
            row.names = FALSE, 
            col.names = FALSE)
write.table(test_set_m, 
            file = "test_set.txt", 
            sep = " ", 
            row.names = FALSE, 
            col.names = FALSE)

#referring the datasets from file.  
training_dataset <- data_file("train_set.txt")
test_dataset <- data_file("test_set.txt")
predict_dataset<-tempfile()

#training using edx data
recommender$train(training_dataset,opts = c(costp_12=0.1,costq_12=0.1,lrate=0.1,niter=100,nthread=6,verbose=F))

#predicting using validation set
recommender$predict(test_dataset, out_file(predict_dataset))
pred_rating<-scan(predict_dataset)

RMSE(validation$rating, round(pred_rating,1))


