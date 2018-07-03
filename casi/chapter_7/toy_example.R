library(glmnet)

# Simulate data
set.seed(123)
N <- 50

x <- data.frame(x1 = rep(0, N))
x$x1 <- runif(n = N)
x$x2 <- runif(n = N)
x$x3 <- 5 * x$x1 + runif(n = N)
x$x4 <- 2 * x$x2 + x$x3 + runif(n = N)
epsilon <- rnorm(n = N)
y <- x$x1 + x$x2 + epsilon
x$y <- y

############  Linear regression  ###############
mod0 <- lm(y ~ . - 1, data = x)

# All predictors appear insignificant here
summary(mod0)

# Plots look OK:
plot(mod0)

############  Ridge regression  ###############

# scale predictors
x_s <- data.frame(scale(x) / sqrt(nrow(x)))
x_s$y <- y - mean(y)

x <- model.matrix(y ~ ., x_s)[,-1]

ridge.mod <-
  glmnet(
    x,
    x_s$y,
    alpha = 0,
    lambda = seq(0, 0.25, 0.01),
    standardize = F
  )

plot(ridge.mod, xvar = "lambda", label = TRUE, )

# Simulate new data (turn into function) and compare models
# Do some inference first
# DO the params seem significant with a good lambda? CV to optimise lambda?