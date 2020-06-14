

library(caret)
library(dslabs)
set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models


predictions <- sapply(fits, function(fit){ 
  #print(model)
  (predict(fit,mnist_27$test))
}) 
predictions

m<-mapply(predictions, FUN=as.numeric)
mat<-matrix(data=m, ncol=10, nrow=200)
mat_mean<-rowMeans(mat)
pred_ensamble<-as.data.frame(mat_mean)%>%mutate(mat_mean=ifelse(mat_mean>=3.5,7,2))

t<-pred_ensamble%>%mutate(x=as.factor(mat_mean))%>%select(x)
t$x
confusionMatrix(factor(t$x), factor(mnist_27$test$y))$overall["Accuracy"]


#all accuracys 
acc_df<-sapply(index, function(index){ 
 
confusionMatrix(factor(predictions[,index]), factor(mnist_27$test$y))$overall["Accuracy"]

})
acc_df
models
acc_df%>%pivot_wider(data=.)

acc_mx<-as.matrix(acc_df)
mean(acc_mx[,1])

#now let us take all training accuracy
index
training_acc <- sapply(index, function(index){ 
  
  fits[[index]]$results$Accuracy
}) 

df_acc<-as.data.frame(training_acc)
colnames(df_acc)<-models
df_acc
colMins(as.matrix(df_acc))
models
colMeans(as.matrix(colMins(as.matrix(df_acc))))


#predicting with only method accuracy >0.8 since the training accuracy is 0.8
#repeating all steps for a fewer models with training accuracy >=0.8



models <- c("glm", "naive_bayes",  "knn", "gamLoess",  "qda", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models


predictions <- sapply(fits, function(fit){ 
  #print(model)
  (predict(fit,mnist_27$test))
}) 
predictions

m<-mapply(predictions, FUN=as.numeric)
mat<-matrix(data=m, ncol=6, nrow=200)
mat_mean<-rowMeans(mat)
pred_ensamble<-as.data.frame(mat_mean)%>%mutate(mat_mean=ifelse(mat_mean>=3.5,7,2))

t<-pred_ensamble%>%mutate(x=as.factor(mat_mean))%>%select(x)
t$x
confusionMatrix(factor(t$x), factor(mnist_27$test$y))$overall["Accuracy"]

# the accuracy improved by removing only those models whos min average accuracy is lesser than minimum average training accuracy togather.



    
    
