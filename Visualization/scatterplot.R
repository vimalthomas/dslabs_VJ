library(tidyverse)
library(dslabs)
library(ggrepel)
library(ggthemes)
data("murders")
head(murders)
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)
murders%>%ggplot()


p<-ggplot(data = murders,aes(population/10^6,total,label=abb))+
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel()+
  #scale_x_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10")
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name = "Region")+
  theme_economist()

p