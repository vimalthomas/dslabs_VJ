library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
#to know the accuracy using lda method
train(x,y, method = "lda",preProcess = "center")

#to udnerstand what predictors drive the algorithm, lets take a look at the 
dfmeans<-t(train(x,y, method = "lda")$finalModel$means)%>%data.frame()


dfmeans%>%mutate(x=rownames(.))%>%ggplot(aes(cerebellum,hippocampus,label=x))+
  geom_point()+
  geom_text()+
  geom_abline()
