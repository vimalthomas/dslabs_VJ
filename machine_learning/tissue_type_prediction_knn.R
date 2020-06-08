set.seed(1, sample.kind="Rounding")
library(caret)
library(tidyverse)
library(dslabs)

data("tissue_gene_expression")
head(tissue_gene_expression)
ks <- c(1,3,5,7,9,11)

tgedf<-data.frame(x=tissue_gene_expression$x,  y= tissue_gene_expression$y)

#create test index
test_index<-createDataPartition(tgedf$y,times=1,p=0.5,list=FALSE)
length(test_index)
#create test and train dataset by splitting data using test index.
test_set <- tgedf%>%slice(test_index)
train_set <- tgedf%>%slice(-test_index)

acc <- sapply(ks, function(k){
  #fitting the line
  knn_fit<-knn3(y~., data=train_set ,k=k)
  knn_fit
  y_hat_knn<-predict(knn_fit, test_set, type = "class")
  y_hat_knn
  length(y_hat_knn)
  length(test_set$y)
  #accuracy
  confusionMatrix(data = y_hat_knn, reference = test_set$y)$overall["Accuracy"]
  
})

df1<-data.frame(k=ks,accuracy=acc)

df1%>%arrange(k)



