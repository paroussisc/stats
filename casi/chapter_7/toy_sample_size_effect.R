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

# repeats of samples
S = 100

# number of data points in each sample
start_N <- 5
N <- 100
seed <- 111

coefs_ridge <- matrix(nrow = 4, ncol = S)
coefs_lasso <- matrix(nrow = 4, ncol = S)
mse <-
    data.frame(
        type = rep(0, 3 * N),
        sample_size = rep(0, 3 * N),
        value = rep(0, 3 * N)
    )
mse_linear <- rep(0, S)
mse_ridge <- rep(0, S)
mse_lasso <- rep(0, S)

for (j in start_N:N)
{
    for (i in 1:S)
    {
        x_s <- toy_data(j)
        x <- model.matrix(x_s$y ~ ., x_s)[, -1]
        
        mod0 <- lm(y ~ . - 1, data = x_s)
        
        ridge.mod.cv <-
            cv.glmnet(x,
                      x_s$y,
                      alpha = 0,
                      standardize = T)
        
        c <- coef(ridge.mod.cv, s = "lambda.min")
        coefs_ridge[1, i] <- c[1]
        coefs_ridge[2, i] <- c[2]
        coefs_ridge[3, i] <- c[3]
        coefs_ridge[4, i] <- c[4]
        
        lasso.mod.cv <-
            cv.glmnet(x,
                      x_s$y,
                      alpha = 1,
                      standardize = T)
        
        c <- coef(lasso.mod.cv, s = "lambda.min")
        coefs_lasso[1, i] <- c[1]
        coefs_lasso[2, i] <- c[2]
        coefs_lasso[3, i] <- c[3]
        coefs_lasso[4, i] <- c[4]
        
        x_test_s <- toy_data(50)
        x_test_matrix <- model.matrix(x_test_s$y ~ ., x_test_s)[,-1]
        
        mse_linear[i] <-
            mean((x_test_s$y - predict(mod0, x_test_s)) ^ 2)
        mse_ridge[i] <-
            mean((x_test_s$y - predict(ridge.mod.cv, x_test_matrix, s = "lambda.min")) ^ 2)
        mse_lasso[i] <-
            mean((x_test_s$y - predict(lasso.mod.cv, x_test_matrix, s = "lambda.min")) ^ 2)
    }
    
    mse[3 * j - 2,]$value <- mean(mse_linear)
    mse[3 * j - 2,]$sample_size <- j
    mse[3 * j - 2,]$type <- 'linear'
    mse[3 * j - 1,]$value <- mean(mse_ridge)
    mse[3 * j - 1,]$sample_size <- j
    mse[3 * j - 1,]$type <- 'ridge'
    mse[3 * j,]$value <- mean(mse_lasso)
    mse[3 * j,]$sample_size <- j
    mse[3 * j,]$type <- 'lasso'
    print(paste("finished sample size", j))
}

ggplot(data = mse[(3 * start_N):nrow(mse), ]) + geom_point(aes(x = sample_size, y =
                                                                   value, color = type)) + geom_smooth(aes(
                                                                       x = sample_size,
                                                                       y = value,
                                                                       group = type,
                                                                       color = type
                                                                   ))
