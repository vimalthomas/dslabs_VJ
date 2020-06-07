set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later

#a data making function whose mu_1 will be changed to seq a set of data sets
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

mu_1 <- seq(0, 3, len=25)

res<-function(t){
dat <- make_data(mu_1=t)
#fitting logistic regression line
glm_fit<-dat$train%>%glm(y ~ x, data=., family = "binomial")

#predicting conditioanl probabilities
p_hat_logit<-predict(glm_fit, newdata = dat$test, type = "response")
#converting into categorical value
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
str(y_hat_logit)
str(dat$test$y)
#getting accuracy
accuracy<-confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]]
accuracy
}

#applying both functions to create 25 different datasets
result<-sapply(mu_1,res)


#qqplot for comparing the results of accuracy 
qqplot(mu_1,result)

