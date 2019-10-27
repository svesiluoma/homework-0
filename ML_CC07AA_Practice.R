# KNN - Q1
library(tidyverse)
library(caret)
library(dslabs)
summary(heights)

y <- heights$sex
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index) 
test_set <- heights %>% slice(test_index) 

summary(train_set)
summary(test_set)
summary(heights)

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(xk){
  fit <- knn3(sex ~ height, data = train_set, k=xk)
  
})