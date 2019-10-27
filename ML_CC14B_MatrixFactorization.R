library(dslabs)
library(tidyverse)
library(caret)

set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

summary(m)
summary(y)
class(y)


# Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)
y[1,]
y[nrow(y),]

# Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Q3
# Use the function svd to compute the SVD of y. This function will return  U ,  V , and the diagonal entries of  D .
s <- svd(y)
names(s)
summary(s)
# You can check that the SVD works by typing
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
# Compute the sum of squares of the columns of  Y  and store them in ss_y
ss_y <- sapply(1:ncol(y), function(i){
  sum(y[,i]^2)
})
ss_y
# Then compute the sum of squares of columns of the transformed  YV  and store them in ss_yv
yv <- y %*% s$v
ss_yv <- sapply(1:ncol(yv), function(i){
  sum(yv[,i]^2)
})
ss_yv
# Confirm that sum(ss_y) is equal to sum(ss_yv)
sum(ss_y)
sum(ss_yv)
# Malliratkaisu
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

# Q4
# Plot ss_y against the column number
plot(ss_y)
# do the same for ss_yv
plot(ss_yv)
# Vastaus: The plots can be made using plot(ss_y) and plot(ss_yv). 
# We see that the variability of the columns of  YV  is decreasing. 
# Furthermore, we see that, relative to the first three, 
# the variability of the columns beyond the third is almost 0.

# Q5
plot(sqrt(ss_yv), s$d)
# Mallivastaus
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

# Q6
# Now compute the percent of the total variability that is explained by just the first three columns of  YV .
pca <- prcomp(yv)
summary(pca)
# Explanation: The total variability explained can be calculated 
# using the following code: sum(s$d[1:3]^2) / sum(s$d^2). We see
# that almost 99% of the variability is explained by the first 
# three columns of  YV=UD . So we get the sense that we should
# be able to explain much of the variability and structure we 
# found while exploring the data with a few columns.
sum(s$d[1:3]^2) / sum(s$d^2)

# Q7
# identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
#identical(s$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
# identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))

# Q8 Compute the average score for each student, plot it against  U1d1,1 , and describe what you find
ss_ud <- s$u %*%   diag(s$d)
plot(ss_ud[ ,1], rowMeans(y))
# Mallivastaus
plot(-s$u[,1]*s$d[1], rowMeans(y))

# Q9 
# Mallivastaus
my_image(s$v)

# Q10
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

# Q11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# Mallivastaus
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

# Q12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# Mallivastaus
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

# Q13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# Mallivastaus
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))
