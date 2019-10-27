# Comprehension Check: Generative Models
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

summary(y)
summary(x)

# Q1 
train_lda <- train(x, y, method = "lda", data = tissue_gene_expression)
train_lda$results

# Q2
library(ggplot2)
train_lda$finalModel
train_lda$finalModel$means
class(train_lda$finalModel$means)

rownames(train_lda$finalModel$means)

df_versio <- as.data.frame(train_lda$finalModel$means)
df_versio
class(df_versio)
df_versio

library(reshape2)
df_versio <- melt(train_lda$finalModel$means)
df_versio
summary(df_versio)

df_versio %>% 
  ggplot(aes(Var2, value)) + geom_point()

# Q3
train_qda <- train(x, y, method = "qda", data = tissue_gene_expression)
train_qda$results

# Q4
df_versio2 <- melt(train_qda$finalModel$means)
df_versio2

df_versio2 %>% 
  ggplot(aes(Var2, value)) + geom_point()

# Q5
train_lda_preprocessed <- train(x, y, method = "lda", data = tissue_gene_expression, preProcess = "center")
df_versio3 <- melt(train_lda_preprocessed$finalModel$means)
df_versio3

df_versio3 %>% 
  ggplot(aes(Var2, value)) + geom_point()

# Q6
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda_preprocessed2 <- train(x, y, method = "lda", data = tissue_gene_expression, preProcess = "center")
train_lda_preprocessed2$results
