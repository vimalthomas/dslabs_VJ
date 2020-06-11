#titanic sample data for prediction

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)


# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

head(titanic_clean)
str(titanic_clean)
set.seed(42, sample.kind="Rounding") 
test_index<-createDataPartition(titanic_clean$Survived,times=1,p=0.2,list=FALSE)
length(test_index)

#create test and train dataset by splitting data using test index.
test_set <- titanic_clean%>%slice(test_index)
train_set <- titanic_clean%>%slice(-test_index)

str(train_set)




#predicting using knn method with 10 fold cross validation

set.seed(10, sample.kind="Rounding") 

train_cls_tree<-train(Survived~.,method="rpart",data = train_set,
                 tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
train_cls_tree

ggplot(train_cls_tree)

plot(train_cls_tree$finalModel)
text(train_cls_tree$finalModel)

y_hat_cls_tree<-predict(train_cls_tree,test_set)
#test prediction based on tree
head(test_set)

c<-colnames(test_set)
c
dat<-c('0','male','1','28','12.00','0','0','1','S')
test<-data.frame()%>%pivot_longer(.)

test<-test_set%>%filter(Age==28 & Sex=='male')
str(test_set)
str(test)
test<-test%>%rbind(dat)
predict(train_cls_tree,test)

plot(y_hat_cls_tree)

confusionMatrix(factor(y_hat_cls_tree),factor(test_set$Survived))

#A 28-year-old male

#would NOT survive
#correct 
#A female in the second passenger class

#would survive
#correct 
#A third-class female who paid a fare of $8

#would survive
#correct 
#A 5-year-old male with 4 siblings

#would NOT survive
#correct 
#A third-class female who paid a fare of $25

#would NOT survive
#correct 
#A first-class 17-year-old female with 2 siblings

#would survive
#correct 
#A first-class 17-year-old male with 2 siblings

#would NOT survive
#correct 

