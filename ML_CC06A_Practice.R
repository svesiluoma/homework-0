x <- matrix(rnorm(1000), 100, 100)
dim(x)
x <- matrix(rnorm(100*10), 100, 10)
dim(x)
x2 <- matrix(1:15, 5, 3)
x2
x3 <- x2 + seq(nrow(x2))
x3
x4 <- 1:nrow(x2)
x4
x5 <- sweep(x2, 2, 1:nrow(x2),"+")
x5
x6 <- sweep(x2, 1, 1:nrow(x2),"+")
x6
x
x2
x <- 1:ncol(x2)
x
x <- 1:col(x2)
x <- sweep(x2, 2, 1:ncol(x2), FUN = "+")
x <- -x2
