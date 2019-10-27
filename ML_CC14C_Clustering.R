# Comprehension Check: Clustering

library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")
summary(tissue_gene_expression$x)

# Q1 
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

# Q2 
hc <- hclust(d, method = "complete", members = NULL)
summary(hc)
plot(hc)
# Mallivastaus
h <- hclust(d)
plot(h)

# Q3
# Mallivastaus
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

# Q4
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
