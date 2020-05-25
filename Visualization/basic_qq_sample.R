library(tidyverse)
library(dslabs)
data(heights)

p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p<-p + geom_qq(dparams = params) +
  geom_abline()

# histogram with blue fill, black outline, labels and title
#p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  #xlab("Male heights in inches") +
  #ggtitle("Histogram")

p