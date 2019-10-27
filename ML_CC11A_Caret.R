# Comprehension Check: Caret Package

# Q1
library(tidyverse)
library(dslabs)
data("tissue_gene_expression")
library(caret)

summary(tissue_gene_expression)
summary(tissue_gene_expression$x)
tissue_gene_expression %>% head()
class(tissue_gene_expression)

set.seed(1991, sample.kind = "Rounding") 
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)

#train_rpart <- train(y ~ ., 
#                     method = "rpart", 
#                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
#                     data = data.frame(tissue_gene_expression))
#ggplot(train_rpart, highlight = TRUE)

# Q2
library(rpart)

set.seed(1991, sample.kind="Rounding")
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)


#set.seed(1991, sample.kind="Rounding")
#fit <- with(tissue_gene_expression, 
#            train(x, y, method = "rpart",control = rpart.control(minsplit = 0),
#                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

#ggplot(fit)
#fit$results

# Q3
#set.seed(1991, sample.kind="Rounding")
#fit2 <- rpart(y ~ x, data = tissue_gene_expression,control = rpart.control(cp = 0, minsplit = 0))
#fit2 <- with(tissue_gene_expression, 
#            train(x, y, method = "rpart",control = rpart.control(cp = 0, minsplit = 0)))
#plot(fit2, margin = 0.1)
#text(fit2, cex = 0.75)

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# Q4
set.seed(1991, sample.kind="Rounding")
fit_rf <- with(tissue_gene_expression, 
                  train(x, y, method = "rf",
                        tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                        nodesize = 1))

# train_rf <- train(x, y, method = "rf", 
#              data = tissue_gene_expression,
#              nodesize = 1,
#              tuneGrid = data.frame(mtry = seq(50, 50, 25)))
ggplot(fit_rf, highlight = TRUE)
fit_rf$bestTune
# malliratkaisu
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

# Q5
imp <- varImp(fit_rf)
imp

# Q6
data("tissue_gene_expression")
library(randomForest)
set.seed(1991, sample.kind="Rounding")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

varImp(fit)

# tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
# tree_terms
# Malliratkaisu
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

