set.seed(1, sample.kind="Rounding")
library(caret)
library(tidyverse)
library(dslabs)

data(heights)
head(heights)
ks <- seq(1, 101, 3)

#create test index
test_index<-createDataPartition(heights$sex,times=1,p=0.5,list=FALSE)

#create test and train dataset by splitting data using test index.
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

F_1 <- sapply(ks, function(k){


#fitting the line
knn_fit<-knn3(sex~height,data=train_set,k=k)

y_hat_knn<-predict(knn_fit, test_set, type = "class")
#F1 score
F_meas(data = y_hat_knn, reference = test_set$sex)

})

plot(ks, F_1)
#index<-which.max(F_1)
#ks[index]

df<-data.frame(k=ks,f_score=F_1)
df%>%arrange(f_score)

ks[which.max(F_1)]

