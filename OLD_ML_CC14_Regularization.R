library(dslabs)
library(tidyverse)
library(caret)

# First, let's simulate the number of students in each school
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))
# Now let's assign a true quality for each school that is completely independent from size. This is the parameter we want to estimate in our analysis.
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
# We can see the top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))
# Now let's have the students in the school take a test. 
# There is random variability in test taking, so we will 
# simulate the test scores as normally distributed with 
# the average determined by the school quality with a 
# standard deviation of 30 percentage points. 
# This code will simulate the test scores
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
summary(schools)

# Q1 Top schools based on the average score?
schools %>% top_n(10, score) %>% arrange(desc(score))
# Mallivastaus
schools_avgscore <- schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
schools_avgscore

# Q2 Median school size
median(schools$size)
median(schools_avgscore$size)
# Mallivastaus
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

# Q3
schools %>% top_n(-10, score) %>% .$size %>% median()
# Mallivastaus
median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()

# Q4
plot(schools$size,schools$score)
# esimerkki muualta
set.seed(1)
mydata <- data.frame(a=1:50, b=rnorm(50))
ggplot(mydata,aes(x=a,y=b)) + 
  geom_point(colour="blue") +
  geom_point(data=mydata[10:13, ], aes(x=a, y=b), colour="red", size=5)