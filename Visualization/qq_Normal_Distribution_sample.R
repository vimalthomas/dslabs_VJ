library(tidyverse)
library(dslabs)
data(heights)



p<-heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# histogram with blue fill, black outline, labels and title
#p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  #xlab("Male heights in inches") +
  #ggtitle("Histogram")

p