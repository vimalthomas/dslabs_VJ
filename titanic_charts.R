options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(gridExtra)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

head(titanic)
nrow(titanic)

#sample density plot on titanic data showing male vs femlae age
titanic%>%ggplot(aes(Age,group=Sex,fill="Red"))+
  geom_density(alpha=0.2,na.rm=FALSE)+
  stat_density(position="stack")+
  
  facet_grid(.~Sex)


#sample qqplot

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params

titanic%>%filter(!is.na(Age))%>%ggplot(aes(sample=Age))+
  geom_qq(dparams=params)+
  geom_abline()
  
#sample barplot
titanic%>%ggplot(aes(Survived,fill=Sex))+
  geom_bar(position = position_dodge())
  

#sample density plot again
titanic%>%ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.2,na.rm=FALSE)

#sample boxplot
titanic%>%filter(Fare>0)%>%ggplot(aes(Survived,Fare))+
  geom_boxplot()+
  scale_y_continuous(trans = "log2")+
  xlab("")+
  geom_jitter(alpha = 0.3)+
  geom_point(show.legend = FALSE)

#sample boxplot again with multi grids!!!

p1<-titanic%>%ggplot(aes(Pclass,fill=Survived))+
  geom_bar()
p1
p2<-titanic%>%ggplot(aes(Pclass,fill=Survived))+
  geom_bar(position=position_fill())
p2
p3<-titanic%>%ggplot(aes(Survived,fill=Pclass))+
  geom_bar(position=position_fill())
p3

grid.arrange(p1,p2,p3,ncol=3)

#sample density again
titanic%>%ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.2,na.rm=FALSE)+
  facet_grid(Sex~Pclass)





