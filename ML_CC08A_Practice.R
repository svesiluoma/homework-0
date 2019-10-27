library(tidyverse)
library(caret)
# Comprehension Check: Cross-validation
# Q1

set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
dim(x_subset)

x[1:3]
y[1:3]

fit <- train(x_subset, y, method = "glm")
fit$results

# Q2 
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value
summary(pvals)
dim(as.matrix(pvals))
# Q3
cutoff <- pvals < 0.01
sum(cutoff)
cutoff[1:3]
x[1:3]
dim(x)
x[cutoff][1:3]

# Q4
fit2 <- train(x[,cutoff], y, method = "glm")
fit2$results

# Q5
x_subset <- x[,cutoff]

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Q6
library(dslabs)
data("tissue_gene_expression")
summary(tissue_gene_expression)
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)

## CC Bootstrap
# Q1
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
indexes[1]

kolmoset <- unlist(indexes[1]) == 3
sum(kolmoset)

neloset <- unlist(indexes[1]) == 4
sum(neloset)

seiskat <- unlist(indexes[1]) == 7
sum(seiskat)

# Q2
kolmoset <- unlist(indexes) == 3
sum(kolmoset)

# Q3
set.seed(1, sample.kind="Rounding")
B <- 10^4
quantile_vs <- replicate(B, {
  y <- rnorm(100, 0, 1)
  qtle <- quantile(y, 0.75)
})
mean(quantile_vs)
sd(quantile_vs)

# Q4
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
N <- length(y)
B <- 10^4 # 10 bootstrap samples
set.seed(1, sample.kind="Rounding")
quantile_vs <- replicate(B, {
  sample_y <- sample(y, N, replace = TRUE)
  qtle <- quantile(sample_y, 0.75)
})
mean(quantile_vs)
sd(quantile_vs)
