library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
summary(tissue_gene_expression)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
image(as.matrix(d))

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

dim(train_set)
dim(test_set)

train_set[1:3, ]
test_set[1:3, ]

summary(train_set)

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k) 
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = factor(test_set$sex))
})
range(F_1)
plot(ks, F_1)

summary(F_1)
F_1[1:3]
max(F_1)
best_k <- ks[which.max(F_1)]
best_k
F_1

# Q2
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
data("tissue_gene_expression")
summary(tissue_gene_expression)
summary(tissue_gene_expression$y)
summary(tissue_gene_expression$x)
tissue_gene_expression$y[1:3]
tissue_gene_expression$x[1:3]

dim(as.data.frame(tissue_gene_expression))

y <- tissue_gene_expression$y
dim(y)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- as.data.frame(tissue_gene_expression) %>% slice(-test_index)
test_set <- as.data.frame(tissue_gene_expression) %>% slice(test_index)

dim(train_set)
dim(test_set)

train_set[1:3,1:3]
test_set[1:3,1:3]

ks <- c(1, 3, 5, 7, 9, 11)
accuracy <- sapply(ks, function(k){
  fit <- knn3(y ~ ., data = train_set, k = k) 
  y_hat <- predict(fit, test_set, type = "class")
  confusionMatrix(y_hat, factor(test_set$y))$overall[["Accuracy"]] 
})
summary(accuracy)
accuracy
print(accuracy)

fit <- knn3(y ~ ., data = train_set, k = 3) 
y_hat <- predict(fit, test_set, type = "class")
confusionMatrix(y_hat, factor(test_set$y))$overall[["Accuracy"]]
