library(tidyverse)
library(dslabs)
data(heights)


p <- heights %>% filter(sex == "Male") %>%ggplot(aes(sample = height))+
  geom_qq()



# histogram with blue fill, black outline, labels and title
#p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  #xlab("Male heights in inches") +
  #ggtitle("Histogram")

p