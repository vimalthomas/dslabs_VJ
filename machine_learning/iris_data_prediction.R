library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


#spliting test and train data

set.seed(2, sample.kind="Rounding")    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


cutoff <- seq(1, 10,0.1)
cutoff

#accuracy for sepal.Width

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

max(accuracy)

#accuracy for sepal.length

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor")
  
  mean(y_hat == train$Species)
})

max(accuracy)









#accuracy for Petal.Width

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

max(accuracy)

#accuracy for Petal.Length

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

max(accuracy)




#applying the training cutoff for test data

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") 

mean(y_hat == test$Species)

#now lets examine the same excercise by using test data as predictors... reverse engineering

#accuracy for sepal.Width

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor")
  mean(y_hat == test$Species)
})

max(accuracy)

#accuracy for sepal.length

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor")
  
  mean(y_hat == test$Species)
})

max(accuracy)









#accuracy for Petal.Width

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor")
  mean(y_hat == test$Species)
})

max(accuracy)

#accuracy for Petal.Length

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor")
  mean(y_hat == test$Species)
})

max(accuracy)




#lets do some data exploratory analysis

plot(iris,pch=21,bg=iris$Species)


#this reveals that a combination of features could help increase the overall accuracy.

#creating two best_cutoffs.



#accuracy for Petal.Width

w_accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

max(w_accuracy)

#accuracy for Petal.Length

l_accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

max(l_accuracy)

#applying the cutoff


best_cutoff1 <- cutoff[which.max(w_accuracy)]
best_cutoff2 <- cutoff[which.max(l_accuracy)]
best_cutoff1
best_cutoff2
y_hat <- ifelse((test$Petal.Length > best_cutoff2 | test$Petal.Width > best_cutoff1), "virginica", "versicolor") 

mean(y_hat == test$Species)

