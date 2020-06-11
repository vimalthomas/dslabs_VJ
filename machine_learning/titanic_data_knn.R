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




#predicting using knn method with a few predictors (sex,class,age,fare)
set.seed(6, sample.kind="Rounding") 
train_knn<-train(Survived~.,method="knn",data = train_set,tuneGrid = data.frame(k=seq(3, 51, 2)))
train_knn

ggplot(train_knn)

  y_hat_knn<-predict(train_knn,test_set)

y_hat_knn

confusionMatrix(factor(y_hat_knn),factor(test_set$Survived))

