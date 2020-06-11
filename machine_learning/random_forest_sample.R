library(dslabs)
library(caret)
library(tidyverse)
library(rpart)
library(randomForest)

data(tissue_gene_expression)
set.seed(1991, sample.kind="Rounding") 

y<-tissue_gene_expression$y

x<-tissue_gene_expression$x

train_result2<-train(x,y,metho="rf",nodesize=1,tuneGrid = data.frame(mtry = seq(50, 200, 25)))


ggplot(train_result2,highlight=TRUE)
importance<-varImp(train_result2)
importance


