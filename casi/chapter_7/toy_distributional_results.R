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
  
  ridge.mod <-
    glmnet(
      x,
      x_s$y,
      lambda = ridge.mod.cv$lambda.min,
      alpha = 0,
      standardize = T
    )
  coefs_ridge[1, i] <- ridge.mod$beta[1]
  coefs_ridge[2, i] <- ridge.mod$beta[2]
  coefs_ridge[3, i] <- ridge.mod$beta[3]
  coefs_ridge[4, i] <- ridge.mod$beta[4]
  
  lasso.mod <-
    cv.glmnet(x,
              x_s$y,
              alpha = 1,
              standardize = T)
  
  lasso.mod <-
    glmnet(
      x,
      x_s$y,
      lambda = ridge.mod.cv$lambda.min,
      alpha = 1,
      standardize = T
    )
  coefs_lasso[1, i] <- lasso.mod$beta[1]
  coefs_lasso[2, i] <- lasso.mod$beta[2]
  coefs_lasso[3, i] <- lasso.mod$beta[3]
  coefs_lasso[4, i] <- lasso.mod$beta[4]
  
  x_test_s <- toy_data(N)
  x_test_mrix <- model.matrix(x_test_s$y ~ ., x_test_s)[,-1]
  
  mse[3*i-2,]$value <- mean((x_test_s$y - predict(mod0, x_test_s)) ^ 2)
  mse[3*i-2,]$type <- 'linear'
  mse[3*i-1,]$value <- mean((x_test_s$y - predict(ridge.mod, x_test_matrix)) ^ 2)
  mse[3*i-1,]$type <- 'ridge'
  mse[3*i,]$value <- mean((x_test_s$y - predict(lasso.mod, x_test_matrix)) ^ 2)
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
g <- ggplot() +  geom_density(data = mse, aes(x = value, group = type, fill = type, alpha = 0.5)) 
g
