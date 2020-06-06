#simple guessing algorithm and the accuracy of the datasets.

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))

#female proportion of inclass group 

str(x)
str(y)

dat%>%group_by(type)%>%summarize(mean(sex=='Female'))

#it is observed that inclass has more female prsense. 
# if one should predict sex based on class type, we could simply write online==Female baesd on proportions.

y_hat <- ifelse(dat$type == 'online', "Female", "Male") %>% 
  factor(levels = c("Female", "Male"))

#we can table it based on table function
table(y_hat, y)

#one way to see the summary between acutual and prediction

confusionMatrix(y_hat, y)

#other ways are using caret package.

library(caret)

sensitivity(y_hat, y)

specificity(y_hat, y)


















x <- dat$type