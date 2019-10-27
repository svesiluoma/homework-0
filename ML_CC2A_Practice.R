library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
head(dat)
head(y)
head(x)

# Q1 Naisten osuus eri ryhmissä
inclass_group <- dat %>% filter(type== "inclass")
inclass_female <- inclass_group$sex == "Female"
mean(inclass_female)
head(inclass_group)

online_group <- dat %>% filter(type== "online")
online_female <- online_group$sex == "Female"
mean(online_female)
head(online_group)

# Q2 oletus, että online on miehiä ja inclass naisia - accuracy?
new_prediction <- dat %>% mutate(pred_by_type = ifelse(type == "inclass", "Female", "Male"))
head(new_prediction)
mean(new_prediction$sex == new_prediction$pred_by_type)

# Q3 Confusion matrix
y_hat <- new_prediction$pred_by_type
table(predicted = y_hat, actual = y)
# Vastaus kysymykseen oli yllä oleva ilman rivien ja sarakkeiden nimeämisiä eli = ja ennen

# Q4 Sensitivity of this prediction
class(y_hat)
y_hat <- as.factor(y_hat) # it was class character instead of factor
class(y)
sensitivity(y_hat, y)

# Q5 specificity
specificity(y_hat, y)

# Q6 Prevalence (% of females) in the dat dataset?
females <- dat$sex == "Female"
mean(females)
