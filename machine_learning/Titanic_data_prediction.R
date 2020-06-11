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
str(test_set)

nrow(train_set)
nrow(test_set)
mean(train_set$Survived==1)

#setting the seed


#guessing the accuracy of the guessing method
N<-nrow(test_set)
guess_y<-sample(c(1,0),N,replace=TRUE)
guess_accuracy<-mean(guess_y)

#guessing based on estimation from training set.
f_x<-train_set%>%filter(Sex=='female')
mean(f_x$Survived==1)
m_x<-train_set%>%filter(Sex=='male')
mean(m_x$Survived==1)

#predicting based on training set proportions

y_hat <- ifelse(test_set$Sex == 'female', 1, 0)
y_hat
table(predicted = y_hat, actual = test_set$Survived)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  summarize(accuracy = mean(y_hat == Survived))

confusionMatrix(factor(y_hat),factor(test_set$Survived))

#predicting by pclasss
train_set%>%group_by(Pclass)%>%summarize(sum(as.numeric(Survived))/n())

train_set%>%group_by(Pclass)%>%summarize(mean(Survived==1))

y_hat_class <- ifelse(test_set$Pclass == '1', 1, 0)%>%factor
y_hat_class

confusionMatrix(factor(y_hat_class),factor(test_set$Survived))

#predicting by gender and pclasss
train_set%>%group_by(Sex,Pclass)%>%summarize(mean(Survived==1))

y_hat_gen_class <- ifelse((test_set$Pclass == '1' & test_set$Sex=='female') | (test_set$Pclass == '2' & test_set$Sex=='female') , 1, 0)%>%factor
y_hat_gen_class 

confusionMatrix(factor(y_hat_gen_class),factor(test_set$Survived))

#compariing confusion matrix for gender,pclass and gen & pclass

confusionMatrix(factor(y_hat),factor(test_set$Survived))

confusionMatrix(factor(y_hat_class),factor(test_set$Survived))

confusionMatrix(factor(y_hat_gen_class),factor(test_set$Survived))

#calculating F_1 score
F_meas(data = factor(y_hat), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_class), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_gen_class), reference = factor(test_set$Survived))

#predicting using lda method with a fare predictor
set.seed(1, sample.kind="Rounding") 

train_lda<-train(Survived~Fare,method="lda",data = train_set)
train_lda

y_hat_lda<-predict(train_lda,test_set)

y_hat_lda

confusionMatrix(factor(y_hat_lda),factor(test_set$Survived))
set.seed(1, sample.kind="Rounding") 

#predicting using qda method with a fare predictor

train_qda<-train(Survived~Fare,method="qda",data = train_set)
train_qda

y_hat_qda<-predict(train_qda,test_set)

y_hat_qda

confusionMatrix(factor(y_hat_qda),factor(test_set$Survived))

