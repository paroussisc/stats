library(ggplot2)
library(reshape2)
library(glmnet)

toy_data <- function(N)
{
  x <- data.frame(x1 = rep(0, N))
  x$x1 <- runif(n = N)
  x$x2 <- runif(n = N)
  x$x3 <- x$x1 + runif(n = N)
  x$x4 <- x$x2 + x$x3 + runif(n = N)
  epsilon <- rnorm(n = N)
  y <- x$x1 - x$x2 + epsilon
  x$y <- y
  return(x)
}

# lambda method
l_type <- "lambda.1se"

# number of samples
S = 1000

# number of data points in each sample
N <- 50
seed <- 111

coefs_ridge <- matrix(nrow = 4, ncol = S)
coefs_lasso <- matrix(nrow = 4, ncol = S)
mse <- data.frame(type = rep(0, 3*S), value = rep(0, 3*S))

for (i in 1:S)
{
  x_s <- toy_data(N)
  x <- model.matrix(x_s$y ~ ., x_s)[, -1]
  
  mod0 <- lm(y ~ . - 1, data = x_s)
  
  ridge.mod.cv <-
    cv.glmnet(x,
              x_s$y,
              alpha = 0,
              standardize = T)
  
  c <- coef(ridge.mod.cv, s = l_type)
  coefs_ridge[1, i] <- c[1]
  coefs_ridge[2, i] <- c[2]
  coefs_ridge[3, i] <- c[3]
  coefs_ridge[4, i] <- c[4]
  
  lasso.mod.cv <-
    cv.glmnet(x,
              x_s$y,
              alpha = 1,
              standardize = T)
  
  c <- coef(lasso.mod.cv, s = l_type)
  coefs_lasso[1, i] <- c[1]
  coefs_lasso[2, i] <- c[2]
  coefs_lasso[3, i] <- c[3]
  coefs_lasso[4, i] <- c[4]
  
  x_test_s <- toy_data(N)
  x_test_matrix <- model.matrix(x_test_s$y ~ ., x_test_s)[,-1]
  
  mse[3*i-2,]$value <- mean((x_test_s$y - predict(mod0, x_test_s)) ^ 2)
  mse[3*i-2,]$type <- 'linear'
  mse[3*i-1,]$value <- mean((x_test_s$y - predict(ridge.mod.cv, x_test_matrix, s = l_type)) ^ 2)
  mse[3*i-1,]$type <- 'ridge'
  mse[3*i,]$value <- mean((x_test_s$y - predict(lasso.mod.cv, x_test_matrix, s = l_type)) ^ 2)
  mse[3*i,]$type <- 'lasso'
}

m <- melt(coefs_ridge)
m$Var1 <- paste0("x", m$Var1)
g <- ggplot(m, aes(x = value))
g <-
  g  +  geom_density(fill = "#FF6666") + geom_vline(xintercept = 0)
g <- g + facet_wrap( ~ Var1)
g

m <- melt(coefs_lasso)
m$Var1 <- paste0("x", m$Var1)
g <- ggplot(m, aes(x = value))
g <-
  g  +  geom_density(fill = "#FF6666") + geom_vline(xintercept = 0)
g <- g + facet_wrap( ~ Var1)
g

# MSE
g <- ggplot() +  geom_density(data = mse, aes(x = value, group = type, fill = type, alpha = 0.1)) 
g
