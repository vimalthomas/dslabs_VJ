library(tidyverse)
library(dslabs)
data(heights)

p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))+
  geom_density()+
  geom_density(fill = "blue")




# histogram with blue fill, black outline, labels and title
#p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
#xlab("Male heights in inches") +
#ggtitle("Histogram")

p