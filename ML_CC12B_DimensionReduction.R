library(caret)
library(dslabs)
library(tidyverse)

# Q1
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
class(tissue_gene_expression)
summary(tissue_gene_expression)
summary(tissue_gene_expression$x)
summary(tissue_gene_expression$y)

pca <- prcomp(tissue_gene_expression$x, rank. = 2)
summary(pca)

data.frame(pca$x, tissue = tissue_gene_expression$y) %>%
  ggplot(aes(PC1, PC2, fill = tissue)) +
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)
# Mallivastaus
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q2
av_pred <- rowMeans(tissue_gene_expression$x)
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, av_pred, color = tissue)) +
  geom_point()
cor(pc$x[,1], av_pred)
# Mallivastaus
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

# Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4
library(ggstatsplot)
pca <- prcomp(tissue_gene_expression$x, rank. = 10)
summary(pca)
data_7 <- data.frame(xvals = pca$x[,7], tissue = tissue_gene_expression$y)
ggplot(data_7, aes(xvals, as.factor(tissue_gene_expression$y), color = tissue_gene_expression$y)) +
  geom_boxplot(outlier.shape = 8, outlier.size = 4)

# Q5
summary(pc)
plot(summary(pc)$importance[3,])
