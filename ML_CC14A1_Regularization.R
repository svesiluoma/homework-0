# Comprehension check: regularization
library(tidyverse)
library(caret)

# First, let's simulate the number of students in each school
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely 
# independent from size. This is the parameter we want to estimate 
# in our analysis. 
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
# There is random variability in test taking, so we will simulate 
# the test scores as normally distributed with the average 
# determined by the school quality with a standard deviation 
# of 30 percentage points. 
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1 Top schools
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# Q2 Median schools
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

# Q3 bottom 10
median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()

# Q4
# Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality.
summary(schools)
top_schools <- schools %>% top_n(10, quality) %>% arrange(desc(quality))
dim(top_schools)
top_schools
schools %>% ggplot(aes(size, score)) + 
  geom_point() +
  geom_point(data=top_schools, aes(size, score), colour="red")
# Mallivastaus
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

# Q5 
overall <- mean(sapply(scores, mean))
overall
alpha <- 25
school_score_avgs <- schools %>% mutate(score_regzd = overall + size * (score - overall) / (size + alpha))
summary(school_score_avgs)
school_score_avgs[which.max(school_score_avgs$score_regzd),]
school_score_avgs %>% top_n(10, score) %>% arrange(desc(score_regzd)) 
# Mallivastaus
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))
summary(score_reg)

# Q6
RMSE <- function(alpha){
  temp <- schools %>% mutate(score_reg = overall + size*(score - overall)/(size + alpha))
  sqrt(mean((temp$quality - temp$score_reg)^2))
}
alphas <- seq(10, 250, 1)
rmses <- sapply(alphas, RMSE)
plot(alphas, rmses)
rmses[which.min(alphas)]
alphas[which.min(rmses)]
# Mallivastaus
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

# Q7
alpha <- 135
score_reg2 <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg2 = score_reg2) %>%
  top_n(10, score_reg2) %>% arrange(desc(score_reg2))
# Mallivastaus
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Q8
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)] 
# Mallivastaus
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  