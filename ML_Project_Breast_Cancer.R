# Breas cancer project

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# Q1: Dimensions and properties
summary(brca)
summary(brca$x)
dim(brca$x)
mean(brca$y == "M")
x_means <- colMeans(brca$x)
which(x_means == max(x_means))
x_sd <- colSds(brca$x)
which(x_sd == min(x_sd))
# Mallivastaus
dim(brca$x)[1] # How many samples
dim(brca$x)[2] # How many predictors
mean(brca$y == "M") # Proportion of malignant samples
which.max(colMeans(brca$x)) # Column with the highest mean
which.min(colSds(brca$x)) # COlumn with the lowest standard deviation

# Q2: Scaling the matrix
x_mean_0 <- sweep(brca$x, 2, colMeans(brca$x))
brcax_standardized <- sweep(x_mean_0, 2, colSds(brca$x), FUN = "/")
sd(brcax_standardized[,1])
median(brcax_standardized[,1])
# Mallivastaus
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
sd(x_scaled[,1])
median(x_scaled[,1])

# Q3: Distance
d <- dist(x_scaled)
summary(d)
dim(as.matrix(d))
mean(as.matrix(d)[2:357])
mean(as.matrix(d)[358:569])
# Mallivastaus
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)]) #he average distance between the first sample, which is benign, and other benign samples?
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM) # he average distance between the first sample and malignant samples?

# Q4: Heatmap of features
heatmap(cor(x_scaled), scale = "row", labRow = NA, labCol = NA)
# Mallivastaus
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Q5: Hierarchical clustering
h <- hclust(d_features)
plot(h, cex = 0.65)
h_cut <- cutree(h, k=5)
plot(h_cut)
# Mallivastaus
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

# Q6: PCA - proportion of variance
pca <- prcomp(x_scaled)
summary(pca)
# Mallivastaus
pca <- prcomp(x_scaled)
summary(pca)    # see PC1 Cumulative Proportion      
                # first value of Cumulative Proportion that exceeds 0.9: PC7

# Q 7: PCA - plotting PCs
data.frame(pca$x, type = brca$y) %>% 
  ggplot(aes(PC1, PC2, fill = type)) +
  geom_point(cex=3, pch=21)
# Mallivastaus
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

# Q8: PCA - PC boxplot
# Make a data frame containing the tumor type and 
# the first 10 PCs, then gather by PC.
dfxy <- data.frame(pca$x[,1:10], type = brca$y)
dataxy <- dfxy %>% group_by(type) 
summary(dataxy)
dataxy %>% head()
data_gathered <- dataxy %>% 
  gather(PC, value, -type) 
summary(data_gathered)
data_gathered %>% head()
data_gathered %>% 
  group_by(type) %>%
  ggplot(aes(PC, value, fill=type)) + 
  geom_boxplot()
# Mallivastaus
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# Prepare data
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]
summary(train_x)
summary(train_y)

# Q9: Training and test sets
mean(train_y == "B")
mean(test_y == "B")

# Q10a:K-means Clustering
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
knn_data_train <- data.frame(type = train_y, train_x)
set.seed(3, sample.kind = "Rounding")
# Perform k-means clustering on the training set with 2 centers and assign the output to k
#k <- train(type ~ ., method = "knn", data = knn_data)
k <- kmeans(train_x, centers = 2)
summary(k)
# use the predict_kmeans function to make predictions on the test set.
knn_predict <- predict_kmeans(test_x, k)
knn_predict %>% head()
test_y %>% head()
dim(as.matrix(knn_predict))
dim(as.matrix(test_y))
knn_predict_transformed <- ifelse(knn_predict == 1, "M", "B")
confusionMatrix(data = as.factor(knn_predict_transformed), reference = test_y)$overall["Accuracy"]
# Mallivastaus
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

# 10b: K-means Clustering
confusionMatrix(data = as.factor(knn_predict_transformed), reference = test_y)
# Mallivastaus
sensitivity(factor(kmeans_preds), test_y, positive = "B") 
sensitivity(factor(kmeans_preds), test_y, positive = "M")

# Q11: Logistic regression model
train_glm <- train(train_x, train_y, method = "glm")
y_hat_glm <- predict(train_glm, test_x, type = "raw") 
summary(y_hat_glm)
mean(y_hat_glm == test_y)
# Mallivastaus
train_glm <- train(train_x, train_y,
                   method = "glm")
glm_preds <- predict(train_glm, test_x)
glm_arvo <- mean(glm_preds == test_y)

# Q12: LDA and QDA models
train_lda <- train(train_x, train_y,
                   method = "lda")
lda_preds <- predict(train_lda, test_x)
lda_arvo <- mean(lda_preds == test_y)
lda_arvo
train_qda <- train(train_x, train_y,
                   method = "qda")
qda_preds <- predict(train_qda, test_x)
qda_arvo <- mean(qda_preds == test_y)
qda_arvo
# Mallivastaus
# Train LDA
train_lda <- train(train_x, train_y,
                   method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)
# Train QDA
train_qda <- train(train_x, train_y,
                   method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

# Q13: Loess model
set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x, train_y,
                   method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)
# Mallivastaus
# set.seed(5, sample.kind = "Rounding")    # simulate R 3.5
train_loess <- train(train_x, train_y,
                     method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)

# Q14: K-nearest neighbors model
set.seed(7, sample.kind = "Rounding")
train_knn <- train(train_x, train_y,
                     method = "knn", tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn$bestTune
train_knn_best <- train(train_x, train_y,
                        method = "knn", tuneGrid = data.frame(k = seq(21, 21, 2)))
knn_preds <- predict(train_knn_best, test_x)
mean(knn_preds == test_y)
# Mallivastaus
set.seed(7, sample.kind = "Rounding")    # simulate R 3.5
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
train_knn$bestTune
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

# Q 15a: Random forest model
library(randomForest)
set.seed(9, sample.kind = "Rounding")    # simulate R 3.5
tuning <- data.frame(mtry = seq(3, 9, 2))
train_rf <- train(train_x, train_y,
                   method = "rf", 
                   tuneGrid = tuning,
                   importance=TRUE)
train_rf$bestTune
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
rf <- randomForest(train_x, train_y, ntree=3)
imp <- importance(rf)
image(matrix(imp, 28, 28))
imp
varImp(train_rf)
# Mallivastaus
# WHich value of mtry gives the highest accuracy?
set.seed(9, sample.kind = "Rounding")    # simulate R 3.5
tuning <- data.frame(mtry = c(3, 5, 7, 9))    # can expand to seq(3, 21, 2), same
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)
train_rf$bestTune
# WHat is the accuracy of the random forest model on the test set?
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
# What is the most important variable in the random forest model?
varImp(train_rf)

# Q 16a: Creating an ensemble
ensembled <- rbind(ifelse(kmeans_preds=="B", 1, 0), 
                   ifelse(glm_preds=="B", 1, 0),
                   ifelse(lda_preds=="B", 1, 0),
                   ifelse(qda_preds=="B", 1, 0),
                   ifelse(loess_preds=="B", 1, 0),
                   ifelse(knn_preds=="B", 1, 0),
                   ifelse(rf_preds=="B", 1, 0))
ensembled_means <- colMeans(ensembled)
dim(as.matrix(ensembled_means))
ensembled_pred <- ifelse(ensembled_means >= 0.5, "B", "M")
mean(ensembled_pred == test_y)
# Mallivastaus
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

# Q 16b: Creating an ensemble
# Mallivastaus
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
