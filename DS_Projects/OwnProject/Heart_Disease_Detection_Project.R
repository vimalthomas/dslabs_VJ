#######################################
#Project: Heart Disease Detection Model
#Author: Vimal Thomas Joseph
#
#The Model generation code consists of the following modules
#1) Library Management
#2) Function Loading
#3) Data Preparation
#4) Data Analysis and Visualization
#5) Scaling, PCA & Cluster Analysis
#6) Creation of training and testing datasets
#7) Machine Learning Model generation
#8) Creation of Ensemble 
#######################################
## 1. Library Management
#######################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyr)
library(naniar)
library(rpart)
library(matrixStats)
library(graphics)
library(stats)
library(dplyr)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(ggthemes)

#######################################
## 2. Function Loading
#######################################

RMSE<-function(true_rating,predicted_rating){
  sqrt(mean((true_rating-predicted_rating)^2))
  
}


#################################
# 3. Data Preparation
#################################
#This data is loaded from https://archive.ics.uci.edu/ml/machine-learning-databases/
#The following section describes the characteristics of the feature variables.
#
#
##1. #3  (age)   - age in years    
## 2. #4  (sex)  - (1 = male; 0 = female)  
## 3. #9  (cp)   -  chest pain type 1: typical angina, 2: atypical angina, 3: non-anginal pain, 4: asymptomatic     
## 4. #10 (trestbps) - resting blood pressure (in mm Hg on admission to the hospital) 
## 5. #12 (chol)    -serum cholestoral in mg/dl  
## 6. #16 (fbs)       -(fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)
## 7. #19 (restecg)   -resting electrocardiographic results  - 0 - normal
##1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
##2: showing probable or definite left ventricular hypertrophy
## 8. #32 (thalach)   # maximum heart rate achieved
## 9. #38 (exang)  exercise induced angina (1 = yes; 0 = no)
## 10. #40 (oldpeak)   ST depression induced by exercise relative to rest
## 11. #41 (slope)     the slope of the peak exercise ST segment
# Value 1: upsloping
# Value 2: flat
# Value 3: downsloping
## 12. #44 (ca)   number of major vessels (0-3) colored by flourosopy     
## 13. #51 (thal)      3 = normal; 6 = fixed defect; 7 = reversable defect
## 14. #58 (num) num: diagnosis of heart disease (angiographic disease status)
## Value 0: < 50% diameter narrowing
## Value 1: > 50% diameter narrowing
##(in any major vessel: attributes 59 through 68 are vessels)

#Data donwloading
heart_data<-read_csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"),
  col_names=FALSE)

#Adding column names
c<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

colnames(heart_data)<-c

###########################################
# 4. Data Analysis and Visualization
###########################################

#Understanding the data summary
summary(heart_data)

#Attempting to find out missing values across dataset

x<-apply(heart_data, 1, function(x){
  
  as.integer(any(grep("\\?",x)))
  
})

index<-which(x==1)
sapply(index,function(i){
  
  heart_data[i,]
})

#there are totally 6 rows in the entire dataset with the presence of missing values.
#next step is to convert them into NA and then replace them with suitable values.

heart_data$ca[heart_data$ca == "?"] <- NA
heart_data$thal[heart_data$thal == "?"] <- NA


#to ensure that NA is correctly replaced, let us check the count of NAs present in the dataset

sum(is.na(heart_data))


#Filling NAs with suitable values.

summary(as.numeric(heart_data$ca))
summary(as.numeric(heart_data$thal))

hd_wrangled<-heart_data%>%
  filter(num %in% c(1,0))%>%
  mutate(ca=ifelse(is.na(ca),0,as.numeric(ca)),
         thal=ifelse(is.na(thal),3,as.numeric(thal)),
  )

#Data visualization

#Creating a grid graph of disease and sex proportions.

p1<-hd_wrangled%>%select(sex,num,age)%>%
  ggplot(aes(num,group=sex,fill=sex))+
  geom_bar(show.legend = FALSE)+
  xlab("Disease") +
  ylab("Count of People") +
  ggtitle("Disease by Male/Female ") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


p2<-hd_wrangled%>%select(sex,num,age)%>%
  ggplot(aes(sex,group=num,fill=num))+
  geom_bar(show.legend = FALSE)+
  xlab("Sex") +
  ylab("Count of People") +
  ggtitle("Male/Female Count by Disease ") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

grid.arrange(p1,p2,ncol=2)

#Creating graph of disease by Age.

hd_wrangled%>%select(sex,num,age)%>%
  ggplot(aes(age,group=num,fill=num))+
  geom_bar(show.legend = FALSE)+
  xlab("Age") +
  ylab("Count of People") +
  ggtitle("Disease by Age ") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


#Line Graph of serum cholestoral and blood pressure.

hd_wrangled%>%select(chol,age,trestbps)%>%group_by(age)%>%summarize(max_chol=max(chol),
                                                                    max_trestbps=max(trestbps))%>%
  ggplot()+
  geom_line(aes(age,max_chol,colour="red"))+
  geom_line(aes(age,max_trestbps,colour="blue"))+
  xlab("Age") +
  ylab("Max of Chol & Trestbps")+
  scale_colour_discrete(name  ="colour",
                        labels=c("Chol", "Trestbps"))+
  ggtitle("Chol & Trestbps by Age ")+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

summary(hd_wrangled)
# we should note that the median value did not change for both features we handled in the dataset with replacement values.

####################################
#5. Scaling, PCA & Cluster Analysis
####################################

#creation of feature (x) and outcome (y) variables to predict y_hat. As taught in the course, features are created as vector matrix and outcome data is created as a y factor
hd_m<-as.matrix(hd_wrangled)

hd_x<-hd_m[,1:13]
hd_y<-as.factor(hd_m[,14])

#First step is to look at the features if any of the feature has higher variance proportions when compared to other features.
#To do that, let us look at the summary of the features to see what values we deal with.

summary(hd_x)

#It looks like age, trestbps, chol and thalach have values in 100s while the rest of the features have values in 1s.
#This could create broader variance differences when preparing the data for model training.
#As taught in the course, let us see if scaling this dataset makes any difference in their variance proportions.
#One way to find out if scaling is required, is by conducting PCA.
#Let us compute Principal Component Analysis - PCA for scaled vs unscaled dataset of same features.

unscaled <- prcomp(hd_x)
scaled <- prcomp(hd_x, scale = TRUE)

#Let us look at the summary of both scaled and unscaled data.
#Proportion of the variance for PC1 reduced from 70% to 18%. This scenario is observed for other features as well.

summary(unscaled)
summary(scaled)



par(mfrow=c(1,2))
biplot(unscaled)
biplot(scaled)

#Let us scale data set by subtracting the column average from each column values and dividing that by overall column standard deviation.

hd_x_minus<-sweep(hd_x,2,colMeans(hd_x),"-")

hd_xdiv<-sweep(hd_x_minus,2,colSds(hd_x_minus),"/")


#make a heatmap of the distance matrix

d_features <- dist(t(hd_xdiv))

heatmap(as.matrix(d_features))

#Cluster Analysis
#Let us try to perform hierarchical clustering as taught in the course on the 14 features. Cut the tree into 5 groups.
#this analysis clearly shows prominent cluster groups like thalach. we can use this information in the clustering models 

hc<-hclust(d_features, method = "complete")
plot(hc)

ctree<-cutree(hc,k=5)
data.frame(ctree)%>%arrange(ctree)

#Now that we have scaled the data, let us apply PCA technique to actual dataset.
p_x<-prcomp(hd_xdiv)

#Creation of a dataframe with only first two PCAs.
df1<-data.frame(p_x$x[,1],p_x$x[,2])
c<-c('pca1','pca2')
colnames(df1)<-c

#binding y outcome to the dataframe.
df1<-cbind(type=hd_y,df1)
rownames(df1) <- 1:nrow(df1)


# Plotting PCs, boxplot.  We can see PC1 is significantly different from others
data.frame(type = hd_y ,p_x$x[,1:10]) %>%
  gather(key = "PC",value="value", -type) %>%
  ggplot(aes(PC,value,fill = type)) +
  geom_boxplot()

##############################################
#6) Creation of training and testing datasets
##############################################

set.seed(1, sample.kind = "Rounding")    
test_index <- createDataPartition(hd_y, times = 1, p = 0.15, list = FALSE)
test_x <- hd_xdiv[test_index,]
test_y <- hd_y[test_index]
train_x <- hd_xdiv[-test_index,]
train_y <- hd_y[-test_index]

mean(test_y==1)

#############################################
#Generation of Machine Learning Models
#############################################

#since this is a classification problem of DISEASE and NOT DISEASE, let us try k-means, logistic regression, classification trees and random forest 

#Model 1 : k-means clustering model
#----------------------------------

#similar to an exercise taught in the course, let us write a function for means.

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

#training
k<-kmeans(train_x,centers=2)

#predicting
pred_k<-predict_kmeans(test_x,k)

#loading actual test y outcomes
act_val_k<-test_y
#converting predicted values to disease or not disease status
pred_val_k<-ifelse(pred_k=='1','1','0')

#using actual values and the predicted values, generating a confusion matrix. Only the overall accuracy is extracted out.
kmeans_acc<-confusionMatrix(data=factor(pred_val_k),reference = factor(act_val_k))$overall["Accuracy"]



# Modell 2: logistic regression model
#------------------------------------
#training
train_glm<-train(y=factor(train_y),x=train_x, method="glm")

#predicting
predict_glm<-predict(train_glm,test_x)

#using actual values and the predicted values, generating a confusion matrix. Only the overall accuracy is extracted out.
glm_acc<-confusionMatrix(data=factor(predict_glm),reference = factor(test_y))$overall["Accuracy"]


# Model 3: KNN Model
#-------------------
#setting up tunegrid parameter
k_seq<-seq(3, 21, 2)

#training
train_knn<-train(y=factor(train_y),x=train_x, method="knn",tuneGrid = data.frame(k=k_seq))

#predicting
predict_knn<-predict(train_knn,test_x)

#using actual values and the predicted values, generating a confusion matrix. Only the overall accuracy is extracted out.
knn_acc<-confusionMatrix(data=predict_knn,reference = test_y)$overall["Accuracy"]


#Model 4: classification Tree Model 
#----------------------------------

#training
train_cls_tree<-train(y=factor(train_y),x=train_x, method="rpart",
                      tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

#plotting the complex parameter
par(mfrow=c(1,1))
ggplot(train_cls_tree)

#plotting the tree
plot(train_cls_tree$finalModel) 
text(train_cls_tree$finalModel)

#predicting
predict_cls_tree<-predict(train_cls_tree,test_x)

#using actual values and the predicted values, generating a confusion matrix. Only the overall accuracy is extracted out.
cls_tree_acc<-confusionMatrix(data=predict_cls_tree,reference = test_y)$overall["Accuracy"]

#Model 5: Random Forest Model 
#-----------------------------

#training
train_rf<-train(y=factor(train_y),x=train_x, method="rf",
                ntree=150,
                tuneGrid = data.frame(mtry = seq(1,7,1)))

#plotting the predictors against the boootstrap samples
ggplot(train_rf)

#finding out the optimum number of trees beyond with the error remains the same.
plot(train_rf$finalModel,main="Random Forest Model - Optimum Tree Count")

#predicting
predict_rf<-predict(train_rf,test_x)

#using actual values and the predicted values, generating a confusion matrix. Only the overall accuracy is extracted out.
rf_acc<-confusionMatrix(data=factor(predict_rf),reference = factor(test_y))$overall["Accuracy"]


#Listing the Accuracies
rf_acc
cls_tree_acc
knn_acc
glm_acc
kmeans_acc

#######################################
#8. Creation of Ensemble Model
#######################################

#Creating Data Frame of all models along with the actual outcomes.
result<-data.frame(
  predict_cls_tree,
  as.factor(pred_val_k),
  predict_glm,
  predict_rf,
  predict_knn,
  test_y)

#adding column name
cname<-(c("cls_tree","kmeans","glm","rf","knn","test_y"))
colnames(result)<-cname

head(result)

#based on the dataframe, use the models to generate an ensemble model for the disease prediction.
#fine-tuned the ifelse condition to increase specificity. 

result<-result%>%mutate(ens = 
                          as.numeric(as.character(cls_tree))+
                          as.numeric(as.character(kmeans))+
                          as.numeric(as.character(glm))+
                          as.numeric(as.character(rf))
                        +as.numeric(as.character(knn))
)%>%mutate(ensemble_y=ifelse(ens>=1,1,0))


#Accuracy of Ensemble Model.                
Ens_Acc<-confusionMatrix(data=as.factor(result$ensemble_y),reference = test_y)$overall["Accuracy"]                

#Final confusion matrix
confusionMatrix(data=as.factor(result$ensemble_y),reference = test_y)

#Finale RMSE of Ensemble Model

RMSE(as.numeric(result$ensemble_y),as.numeric(as.character(test_y)))


