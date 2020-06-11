library(dslabs)
library(caret)
library(tidyverse)
library(rpart)

data(tissue_gene_expression)
set.seed(1991, sample.kind="Rounding") 

y<-tissue_gene_expression$y

x<-tissue_gene_expression$x

train_result<-train(x,y,method='rpart',tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)))
train_result$bestTune

ggplot(train_result,hightlight=TRUE)

#tuning control parameter 

train_result2<-train(x,y,method="rpart",
                    tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)),
                    control = rpart.control(minsplit = 0.1))
train_result$bestTune
tree_terms <- as.character(unique(train_result2$finalModel$frame$var[!(train_result2$finalModel$frame$var == "<leaf>")]))
tree_terms
#varIMp shows the importance of the preictors
varImp(train_result2)

