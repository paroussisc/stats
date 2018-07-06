library(ggplot2)

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

# TODO: INVESTIGATE LAMBDA MIN VS 1SE

# number of samples
S = 100

# number of data points in each sample
N <- 50
seed <- 111

coefs <- Matrix(nrow = 4, ncol = S)

for(i in 1:S)
{
  x_s <- toy_data(N)
  x <- model.matrix(x_s$y ~ ., x_s)[,-1]
  
  ridge.mod.cv <-
    cv.glmnet(x,
              x_s$y,
              alpha = 0,
              standardize = T)
  
  ridge.mod <-
    glmnet(x,
              x_s$y,
              lambda = ridge.mod.cv$lambda.min,
              alpha = 0,
              standardize = T)
  coefs[1, i] <- ridge.mod$beta[1]
  coefs[2, i] <- ridge.mod$beta[2]
  coefs[3, i] <- ridge.mod$beta[3]
  coefs[4, i] <- ridge.mod$beta[4]
}

hist(coefs[1,])
hist(coefs[2,])
hist(coefs[3,])
hist(coefs[4,])
