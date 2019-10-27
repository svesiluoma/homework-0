set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
summary(disease)
summary(test)

# Q2 What is the probability that a test is positive?
mean(test)

# Q3 What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

# Q4 What is the probability that you have the disease if the test is positive?
mean(disease[test==1]==1)

# Q5 If the test is positive, what is the relative risk of having the disease?
# First calculate the probability of having the disease given a positive test, then normalize it against the disease prevalence.
mean(disease[test==1]==1)/mean(disease==1)

# Q6 To compute conditional prob for being male in the heights dataset
library(dslabs)
library(dplyr)
library(ggplot2)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Q7 Use the quantile 0.1, ...,0.9 and the cut function to assure each group has the same number of points
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# Q8 
library(MASS)
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%  	
  qplot(x, y, data =.)
