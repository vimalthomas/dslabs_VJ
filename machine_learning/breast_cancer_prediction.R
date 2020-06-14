#breast cancer prediction assessement

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(matrixStats)
library(caret)
data(brca)

dim(brca$x)
brca$x

which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#scaling 
brca_xminus<-sweep(brca$x,2,colMeans(brca$x),"-")
colMedians(brca_xminus)
brca_xdiv<-sweep(brca_xminus,2,colSds(brca_xminus),"/")
colMedians(brca_xdiv)

class(brca_xdiv)
d<-as.matrix(dist(brca_xdiv))
ind_b<-which(brca$y=="B")
ind_m<-which(brca$y=="M")
ind_m
plot(d)
dim(brca_xdiv)

#find a sample distance for the first sample. 
#average distance between first sample and the benign sammple
d_1_b<-mean(d[1,ind_b])
d_1_b
#average distance between first sample and the maligin sammple
d_1_m<-mean(d[1,ind_m])
d_1_m

#make a heatmap of the distance matrix
d_features <- dist(t(brca_xdiv))

heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

#Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.

d_features_matx<-as.matrix(d_features)
dim(d_features_matx)

memb<-cutree(hc, k = 10)
hc<-hclust(d_features, method = "complete")
plot(hc)
hc
ctree<-cutree(hc,k=5)
data.frame(ctree)%>%arrange(ctree)

#principal component analysis

p_x<-prcomp(brca_xdiv)
summary(p_x)


#plottings PCs


df1<-data.frame(p_x$x[,1],p_x$x[,2])
c<-c('pca1','pca2')
colnames(df1)<-c
df1
df1<-cbind(type=brca$y,df1)
rownames(df1) <- 1:nrow(df1)
df1
df1%>%ggplot(aes(pca1,pca2,color=type))+
  geom_point()


#box plottings PCs

#making boxplot for first 10 pcs

# Plotting PCs, boxplot.  We can see PC1 is significantly different from others
data.frame(type = brca$y ,p_x$x[,1:10]) %>%
  gather(key = "PC",value="value", -type) %>%
  ggplot(aes(PC,value,fill = type)) +
  geom_boxplot()


#creating training and testing datasets

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- brca_xdiv[test_index,]
test_y <- brca$y[test_index]
train_x <- brca_xdiv[-test_index,]
train_y <- brca$y[-test_index]

#test propoertions

mean(test_y=='B')

#k-means clustering model

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}


set.seed(3, sample.kind = "Rounding") 
k<-kmeans(train_x,centers=2)
table(k$cluster,train_y)

pred_k<-predict_kmeans(test_x,k)
act_val_k<-test_y
pred_val_k<-ifelse(pred=='1','B','M')
confusionMatrix(data=factor(pred_val_k),reference = factor(act_val_k))
kmeans_acc<-confusionMatrix(data=factor(pred_val_k),reference = factor(act_val_k))$overall["Accuracy"]

kmeans_acc

#logistic regression model

train_glm<-train(y=train_y,x=train_x, method="glm")

predict_glm<-predict(train_glm,test_x)

confusionMatrix(data=factor(predict_glm),reference = factor(test_y))
glm_acc<-confusionMatrix(data=factor(predict_glm),reference = factor(test_y))$overall["Accuracy"]

#QDA

train_qda<-train(y=train_y,x=train_x, method="qda")

predict_qda<-predict(train_qda,test_x)

confusionMatrix(data=factor(predict_qda),reference = factor(test_y))
qda_acc<-confusionMatrix(data=factor(predict_qda),reference = factor(test_y))$overall["Accuracy"]

#LDA

train_lda<-train(y=train_y,x=train_x, method="lda")

predict_lda<-predict(train_lda,test_x)

confusionMatrix(data=factor(predict_lda),reference = factor(test_y))
lda_acc<-confusionMatrix(data=factor(predict_lda),reference = factor(test_y))$overall["Accuracy"]

#loess
set.seed(5, sample.kind = "Rounding") 
train_loess<-train(y=train_y,x=train_x, method="gamLoess")

predict_loess<-predict(train_loess,test_x)

confusionMatrix(data=factor(predict_loess),reference = factor(test_y))
loess_acc<-confusionMatrix(data=factor(predict_loess),reference = factor(test_y))$overall["Accuracy"]


#KNN
set.seed(7, sample.kind = "Rounding") 
k_seq<-seq(3, 21, 2)
train_knn<-train(y=train_y,x=train_x, method="knn",tuneGrid = data.frame(k=k_seq))
train_knn
predict_knn<-predict(train_knn,test_x)

confusionMatrix(data=factor(predict_knn),reference = factor(test_y))
knn_acc<-confusionMatrix(data=factor(predict_knn),reference = factor(test_y))$overall["Accuracy"]
knn_acc

#Random Forest Model
set.seed(9, sample.kind = "Rounding") 
mtry_seq<-seq(3, 9, 2)
train_rf<-train(y=train_y,x=train_x,
                method="rf",
                importance = TRUE,
                tuneGrid = data.frame(mtry=mtry_seq))
train_rf$
predict_rf<-predict(train_rf,test_x)

confusionMatrix(data=factor(predict_rf),reference = factor(test_y))
rf_acc<-confusionMatrix(data=factor(predict_rf),reference = factor(test_y))$overall["Accuracy"]
rf_acc
train_rf$finalModel$importance
varImp(train_rf)

#k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors, and random forest.

pred_kmeans<-as.factor(pred_val_k)

pred_kmeans
predict_glm
predict_qda
predict_lda
predict_loess
predict_knn
predict_rf



predict_ensamble<-ifelse(pred_kmeans=='B',1,0)+ifelse(predict_glm=='B',1,0)+
  ifelse(predict_qda=='B',1,0)+ifelse(predict_lda=='B',1,0)+
  ifelse(predict_loess=='B',1,0)+ifelse(predict_knn=='B',1,0)+
  ifelse(predict_rf=='B',1,0)

pred_en_final<-as.factor(ifelse(predict_ensamble>3,'B','M'))

en_acc<-confusionMatrix(data=factor(pred_en_final),reference = factor(test_y))$overall["Accuracy"]
  
#LDA model has the highest accuracy in all the models.





