library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3) 

head(stars)

mean=mean(stars$magnitude)
sd=sd(stars$magnitude)

mean
sd
#distribution of magnitute - two peaks
stars%>%ggplot(aes(magnitude))+
  geom_density()

#distribution of temp- two peaks
stars%>%ggplot(aes(temp))+
  geom_density()

#scatterplot between temo and mag
stars%>%ggplot(aes(magnitude,temp))+
  geom_point()

stars%>%ggplot(aes(magnitude,log10(temp)))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()


#another example of scatterplot repel text

stars%>%filter(temp>4000)%>%ggplot(aes(magnitude,(temp),label=star))+
  geom_point(size=1)+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_text_repel()


#another example of scatterplot

stars%>%ggplot(aes(magnitude,log2(temp),color=type))+
  geom_point(size=1)+
  scale_y_reverse()+
  scale_x_reverse()

stars%>%order_by(temp)
  


