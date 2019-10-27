# Q1

library(caret)
library(dslabs)
library(tidyverse)
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

names(fits)
summary(fits)

# Q2
# Prediction jokaiselle metodille erikseen y rows, mallien määrä columns
length(mnist_27$test$y)
length(models)
# Mallivastaus
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
summary(pred)
class(pred)

# Q3 
test <- matrix(mnist_27$test$y, nrow = length(mnist_27$test$y), ncol = 10)
mean(pred == test)
# Mallivastaus
# Accuracy for each model in the test set and the mean accuracy across all models can be computed using the following code:
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# Q4
majority <- sapply(1:nrow(pred), function(idx) {
  # get the number of time each entry in df occurs
  t <- table(t(pred[idx, ]))
  # get the maximum count (or frequency)
  t.max <- max(t)
  # get all values that equate to maximum count
  t <- as.numeric(names(t[t == t.max]))
})
mean(majority == mnist_27$test$y)
# Mallivastaus
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
y_hat
mean(y_hat == mnist_27$test$y)

# Q5
sum(acc > 0.815)
models
acc
# Mallivastaus
#The comparison of the individual methods to the ensemble can be done using the following code:
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

# Q6
fits[2]$results$Accuracy
class(fits)
minim <- sapply(1:10, function(idx) {
  mi <- min(t(as.vector(fits[[idx]]$results$Accuracy)))
})
summary(minim)
minim %>% head()
mean(minim)
# Mallivastaus
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# Q7
acc_selected <- acc[acc >= 0.8]
acc_selected
mean(acc_selected)
# Mallivastaus
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
