set.seed(1, sample.kind="Rounding")
#highly correlated x1 and x2
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")
#create test index
test_index<-createDataPartition(dat$y,times=1,p=0.5,list=FALSE)

#create test and train dataset by splitting data using test index.

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

#model1 - just x1 feature

  #fit the model
  fit<-lm(y~x_1,data=train_set)
  
  y_hat<-predict(fit,test_set)
  rmse<-sqrt(mean((y_hat-test_set$y)^2))
  
  rmse
  

#model2 - just x1 feature
  
  #fit the model
  fit<-lm(y~x_2,data=train_set)
  
  y_hat<-predict(fit,test_set)
  rmse<-sqrt(mean((y_hat-test_set$y)^2))
  
  rmse
 
  
  
#model3 - just x1,x2 feature
  
  #fit the model
  fit<-lm(y~x_1 + x_2,data=train_set)
  
  y_hat<-predict(fit,test_set)
  rmse<-sqrt(mean((y_hat-test_set$y)^2))
  
  rmse
  
  



