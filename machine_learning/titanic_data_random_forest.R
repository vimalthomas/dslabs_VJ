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

set.seed(14, sample.kind="Rounding") 

train_cls_tree<-train(Survived~.,method="rf",data = train_set,ntree=100,
                 tuneGrid = data.frame(mtry = seq(1,7,1)))
train_cls_tree
varImp(train_cls_tree)
ggplot(train_cls_tree)

plot(train_cls_tree$finalModel)
text(train_cls_tree$finalModel)

y_hat_cls_tree<-predict(train_cls_tree,test_set)
confusionMatrix(factor(y_hat_cls_tree),factor(test_set$Survived))
