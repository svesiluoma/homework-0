library(caret)
data(iris)

# Remove detosa species
iris <- iris[-which(iris$Species=='setosa'),]
str(iris)
summary(iris)

y <- iris$Species
head(iris)
head(y)

# Q7 To create train and test partitionings using createDataPartition
set.seed(2, sample.kind="Rounding")
# line of code added
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]
summary(train)

# Q8 Gretest overall accuracy per one feature
library(purrr)
# Study Sepal.Length accuracy
cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(train$Sepal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(train$Species))
y_hat <- factor(y_hat)
levels(y_hat) = levels(train$Species)
mean(y_hat == train$Species)

# Study Sepal.Width accuracy
cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(train$Sepal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(train$Species))
y_hat <- factor(y_hat)
levels(y_hat) = levels(train$Species)
mean(y_hat == train$Species)

# Study Petal.Length accuracy
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(train$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(train$Species))
y_hat <- factor(y_hat)
levels(y_hat) = levels(train$Species)
mean(y_hat == train$Species)

# Study Petal.Width accuracy
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(train$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(train$Species))
y_hat <- factor(y_hat)
levels(y_hat) = levels(train$Species)
mean(y_hat == train$Species)

# Q9
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat_test <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat_test)
levels(y_hat_test) = levels(test$Species)
mean(y_hat_test == test$Species)

# Q10 
hae_accuracy_per_muuttuja <- function(muuttuja, settityyppi) {
  cutoff <- seq(min(muuttuja), max(muuttuja), 0.1)
  if (settityyppi == "train") {
    tasot = train$Species
  } else {
    tasot = test$Species
  }
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(muuttuja > x, "virginica", "versicolor") %>% 
      factor(levels = levels(tasot))
    mean(y_hat == tasot)
  })
  max(accuracy)
}

hae_accuracy_per_muuttuja(test$Sepal.Length,"test")
hae_accuracy_per_muuttuja(test$Sepal.Width,"test")
hae_accuracy_per_muuttuja(test$Petal.Length,"test")
hae_accuracy_per_muuttuja(test$Petal.Width,"test")

# Q11
plot(iris,pch=21,bg=iris$Species)

PL_cutoff = 4.7
PW_cutoff = 1.5
y_hat_test <- ifelse(test$Petal.Length > PL_cutoff | test$Petal.Width > PW_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
levels(y_hat_test) = levels(test$Species)
mean(y_hat_test == test$Species)



