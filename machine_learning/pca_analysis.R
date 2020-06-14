library(dslabs)
library(dplyr)
library(tidyverse)
library(dslabs)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)
#running principal component anaysis
pca<-prcomp(tissue_gene_expression$x)

#coverting into a dataframe
df1<-data.frame(pca$x[,1],pca$x[,2])
c<-c('pca1','pca2')
colnames(df1)<-c

#binding tissue type as acolumn
df1<-cbind(tissuetype=tissue_gene_expression$y,df1)
rownames(df1) <- 1:nrow(df1)
df1

#making a plot to see the cluster types between tissue
df1%>%ggplot(aes(pca1,pca2,color=tissuetype))+
  geom_point()

#to understand primary pcas contain most of the predictor value, we will now compare average of all predictors with primary pca

avg_all_predictors<-rowMeans(tissue_gene_expression$x)

df1<-cbind(df1,avg_all_pred=avg_all_predictors)
df1%>%ggplot(aes(pca1,avg_all_pred,color=tissuetype))+
  geom_point()

cor(df1$pca1,df1$avg_all_pred)

#redoing the same analysis but now after removing the center from the dataset
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pca<-prcomp(x)

df1<-data.frame(pca$x[,1],pca$x[,2])
c<-c('pca1','pca2')
colnames(df1)<-c
df1<-cbind(tissuetype=tissue_gene_expression$y,df1)
rownames(df1) <- 1:nrow(df1)
df1
df1%>%ggplot(aes(pca1,pca2,color=tissuetype))+
  geom_point()

#making boxplot for first 10 pcs
pca<-prcomp(x)
df2<-data.frame(pca$x[,1],pca$x[,2],pca$x[,3],pca$x[,4],pca$x[,5],pca$x[,6],pca$x[,7],pca$x[,8],pca$x[,9],pca$x[,10])
c<-c('pca1','pca2','pca3','pca4','pca5','pca6','pca7','pca8','pca9','pca10')
colnames(df2)<-c
df2<-cbind(tissuetype=tissue_gene_expression$y,df2)
rownames(df2) <- 1:nrow(df2)
df2
#there is a question to see the max median difference in tissue types of pca7
df2%>%ggplot(aes(tissuetype,pca7))+
  geom_boxplot()
#summary shows, we need only first 3 pcas to attain a cumulative proportion of >50 of entire data

summary(pca)  
  