library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
summary(titanic_clean)

# Q1 Training and test sets
y <- titanic_clean$Survived
set.seed(42, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

nrow(train_set)
nrow(test_set)

summary(train_set)
mean(train_set$Survived == "1")

# Q2 Baseline prediction by guessing the outcome
l <- length(test_set$Survived)
set.seed(3, sample.kind="Rounding")
y_hat_random <- sample(c("0", "1"), l, replace = TRUE) %>% factor(levels = levels(test_set$Survived))
summary(y_hat_random)
mean(y_hat_random == test_set$Survived)
# Mallivastaus
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

# Q3a Predicting survival by sex
#sex_female <- test_set$Sex == "female"
#summary(sex_female)
#mean(test_set$Survived[sex_female] == "1") # Female survivors
#mean(test_set$Survived[-sex_female] == "1") # Male survivors
mean(as.numeric(as.character(train_set[which(train_set$Sex== 'male'),]$Survived)))
mean(as.numeric(as.character(train_set[which(train_set$Sex== 'female'),]$Survived)))
# Mallivastaus
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

# Q3b Predicting survival by sex
male_survivors <- mean(as.numeric(as.character(train_set[which(train_set$Sex== 'male'),]$Survived)))
male_survivors
female_survivors <- mean(as.numeric(as.character(train_set[which(train_set$Sex== 'female'),]$Survived)))
female_survivors
y_hat_sex <- ifelse(test_set$Sex == "female", 1, 0) %>% factor(levels = levels(test_set$Survived))
summary(y_hat_sex)
summary(test_set$Survived)
mean(y_hat_sex == test_set$Sex) 
# Mallivastaus
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy

# Q4a Predicting survival by passenger class
summary(train_set$Pclass)
class1_survivors <- mean(as.numeric(as.character(train_set[which(train_set$Pclass == '1'),]$Survived)))
class1_survivors
class2_survivors <- mean(as.numeric(as.character(train_set[which(train_set$Pclass == '2'),]$Survived)))
class2_survivors
class3_survivors <- mean(as.numeric(as.character(train_set[which(train_set$Pclass == '3'),]$Survived)))
class3_survivors

# Q4b
Pclass_model <- ifelse(test_set$Pclass == "1", 1, 0)   
mean(Pclass_model == test_set$Survived)    

# Q4c
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female" & Pclass == "1") %>%
  pull(Survived)
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female" & Pclass == "2") %>%
  pull(Survived)
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female" & Pclass == "3") %>%
  pull(Survived)
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male" & Pclass == "1") %>%
  pull(Survived)
# Mallivastaus
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

# Q4d
test_set$Sex %>% head()
test_set$Survived %>% head()
SexPclass_model <- ifelse((test_set$Sex == "female" & (test_set$Pclass == "1" | test_set$Pclass == "2")), 1, 0) %>% factor(levels = levels(test_set$Survived))
SexPclass_model %>% head()
mean(SexPclass_model == test_set$Survived) 
# Mallivastaus
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

# Q5a & b Confusion matrix
confusionMatrix(data = as.factor(sex_model), reference = test_set$Survived)
confusionMatrix(data = as.factor(Pclass_model), reference = test_set$Survived)
confusionMatrix(data = as.factor(SexPclass_model), reference = test_set$Survived)
# Mallivastaus
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

# Q6 F1 scores
F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(Pclass_model), reference = factor(test_set$Survived))
F_meas(data = factor(SexPclass_model), reference = factor(test_set$Survived))

# Q7 Survival by fare - LDA and QDA
summary(train_set)
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$Survived)$overall["Accuracy"] 

train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
confusionMatrix(predict(train_qda, test_set), test_set$Survived)$overall["Accuracy"]

#Mallivastaus
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

# Q8 Logistic regression models
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
confusionMatrix(predict(train_glm, test_set), test_set$Survived)$overall["Accuracy"] 

train_glm_combined <- train(Survived ~ Sex+Pclass+Fare+Age, method = "glm", data = train_set)
confusionMatrix(predict(train_glm_combined, test_set), test_set$Survived)$overall["Accuracy"] 

train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_preds <- predict(train_glm_all, test_set)
mean(glm_preds == test_set$Survived)

# mallivastaukset
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

# Q9a kNN model
set.seed(6, sample.kind="Rounding")
train_kNN <- train(Survived ~ .,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   data = train_set)
train_kNN$bestTune

# Q9b 
ggplot(train_kNN, highlight = TRUE)

# Q9c
knn_preds <- predict(train_kNN, test_set)
mean(knn_preds == test_set$Survived)

# Q10 Cross-validation
control <- trainControl(method = "cv", number = 10, p = .9)
set.seed(8, sample.kind="Rounding")
train_kNN <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = control)
ggplot(train_kNN, highlight = TRUE)
train_kNN$bestTune
knn_preds2 <- predict(train_kNN, test_set)
mean(knn_preds2 == test_set$Survived)
# Mallivastaus
set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)

# Q11 Classification tree model
set.seed(10, sample.kind = "Rounding")
train_rpart <- train(Survived ~ .,
                   method = "rpart",
                   data = train_set,
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)
# Mallivastaus
set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)

# Q11b
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# Q12 Random forest model
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ ., 
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(1, 7, 1)),
                  ntree = 100,
                  data = train_set)
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)
varImp(train_rf)
# Mallivastaus
set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(Survived ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)
varImp(train_rf)    # first row

