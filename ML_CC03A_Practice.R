library(MASS)
library(dplyr)
library(tidyverse)
library(caret)

set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

summary(Sigma)
nrow(sigma)
summary(dat)
nrow(dat)

# Q1
set.seed(1, sample.kind="Rounding")

predicting <- function(d) {
  y <- d$y
  # Muodosta train ja test setit
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- d %>% slice(-test_index)
  test_set <- d %>% slice(test_index)
  # Train a linear model predicting y from x
  x <- d$x
  fit <- lm(y ~ x, data = train_set)
  fit$coef
  # Generate predictions on the test set
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
}

RMSE_set <- replicate(n, predicting(dat))
summary(RMSE_set)
mean(RMSE_set)
sd(RMSE_set)

# Q2 Larger dataset
size_n_predicting <- function(n) {
  # Create the data with n findings
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  # Build 100 linear models and retur RMSEs
  RMSE_set <- replicate(100, predicting(dat))
  #Calculate mean or sd
  #return(list(mean(RMSE_set), sd(RMSE_set)))
  list(mean(RMSE_set), sd(RMSE_set))
}

#Return required values
set.seed(1, sample.kind="Rounding")
#i <- c(100, 500, 1000, 5000, 10000)
size_n_predicting(100)
size_n_predicting(500)
size_n_predicting(1000)
size_n_predicting(5000)
size_n_predicting(10000)

# Q4 
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
RMSE_set <- replicate(n, predicting(dat))
mean(RMSE_set)
sd(RMSE_set)

# Q6
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
# use the caret package to partition into a test and training set of equal size
y <- dat$y
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a linear model for each.
# x_1
x_1 <- dat$x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
# x_2
x_2 <- dat$x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
# x_1 and x_2
x_1 <- dat$x_1
x_2 <- dat$x_2
fit <- lm(y~x_1+x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

# Q8
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# use the caret package to partition into a test and training set of equal size
y <- dat$y
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a linear model for each.
# x_1
x_1 <- dat$x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
# x_2
x_2 <- dat$x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
# x_1 and x_2
x_1 <- dat$x_1
x_2 <- dat$x_2
fit <- lm(y~x_1+x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
