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


#predicting using glm method with age predictors
set.seed(1, sample.kind="Rounding") 
train_glm_age<-train(Survived~Age,method="glm",data = train_set)
train_glm_age

y_hat_glm_age<-predict(train_qda,test_set)

y_hat_glm_age

confusionMatrix(factor(y_hat_glm_age),factor(test_set$Survived))



#predicting using glm method with a few predictors (sex,class,age,fare)
set.seed(1, sample.kind="Rounding") 
train_glm_many<-train(Survived~Sex + Pclass + Age + Fare,method="glm",data = train_set)
train_glm_many

y_hat_glm_many<-predict(train_glm_many,test_set)

y_hat_glm_many

confusionMatrix(factor(y_hat_glm_many),factor(test_set$Survived))


#predicting using glm method with all predictors
set.seed(1, sample.kind="Rounding") 
train_glm_all<-train(Survived~.,method="glm",data = train_set)
train_glm_all

y_hat_glm_all<-predict(train_glm_all,test_set)

y_hat_glm_all

confusionMatrix(factor(y_hat_glm_age),factor(test_set$Survived))
confusionMatrix(factor(y_hat_glm_many),factor(test_set$Survived))
confusionMatrix(factor(y_hat_glm_all),factor(test_set$Survived))