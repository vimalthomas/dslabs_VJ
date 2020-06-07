

dataset_function<-function(val){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(val, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  B<-100
  r<-replicate(B,{
    
    #create test index
    test_index<-createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    
    #create test and train dataset by splitting data using test index.
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    m<-mean(train_set$y)
    rmse_guess<-mean((m-test_set$y)^2)
    
    
    #fit the model
    fit<-lm(y~x,data=train_set)
    
    y_hat<-predict(fit,test_set)
    rmse<-sqrt(mean((y_hat-test_set$y)^2))
    
    rmse
  })
  c(mean(r),sd(r))
  
  
}



n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding")
sapply(n,dataset_function)





  





