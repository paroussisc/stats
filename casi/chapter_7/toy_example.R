library(glmnet)
library(broom)
library(ggplot2)

toy_data <- function(N, seed)
{
  set.seed(seed)
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


# Simulate data
x_s <- toy_data(50, 123)

############  Linear regression  ###############
mod0 <- lm(y ~ . - 1, data = x_s)

# All predictors appear insignificant here
summary(mod0)

# QQ plot isn't great:
#plot(mod0)

############  Ridge regression  ###############

x <- model.matrix(x_s$y ~ ., x_s)[,-1]

ridge.mod <-
  glmnet(
    x,
    x_s$y,
    alpha = 0,
    lambda = seq(0, 13.25, 0.01),
    standardize = T,
    intercept = F
  )

# The largest coefficients are x1 and x2, but this doesn't help completely with the
# problem of variable selection - x3's coefficient isn't close enough to zero to ignore
tidied_cv <- tidy(ridge.mod)
ggplot() + geom_line(data = tidied_cv, aes(x = lambda, y = estimate, color = as.factor(term))) + geom_hline(yintercept = 0)


############  Lasso regression  ###############
lasso.mod <-
  glmnet(
    x,
    x_s$y,
    alpha = 1,
    lambda = seq(0, 0.25, 0.001),
    standardize = T,
    intercept = F
  )

# very helpful with variable selection here - correctly identifies
# that x3 and x4 are not as useful features
tidied_cv <- tidy(lasso.mod)
ggplot() + geom_line(data = tidied_cv, aes(x = lambda, y = estimate, color = as.factor(term))) + geom_hline(yintercept = 0)


############  Prediction  ###############

# Linear model is better on our training data
print(paste("MSE on linear model is:", mean(mod0$residuals ^ 2)))

# Now get optimal lambda using cross-validation
ridge.mod.cv <-
  cv.glmnet(x,
            x_s$y,
            alpha = 0,
            standardize = F)

lasso.mod.cv <-
  cv.glmnet(x,
            x_s$y,
            alpha = 1,
            standardize = F)

print(
  paste(
    "Lambda value for ridge regression that gives the best cross-validation error is:",
    ridge.mod.cv$lambda.min,
    "and gives MSE",
    min(ridge.mod.cv$cvm)
  )
)

print(
  paste(
    "Lambda value for lasso regression that gives the best cross-validation error is:",
    lasso.mod.cv$lambda.min,
    "and gives MSE",
    min(lasso.mod.cv$cvm)
  )
)

# Final models using optimised lambda
ridge.final <-
  glmnet(
    x,
    x_s$y,
    alpha = 0,
    lambda = min(ridge.mod.cv$lambda.min),
    standardize = T
  )

lasso.final <-
  glmnet(
    x,
    x_s$y,
    alpha = 1,
    lambda = min(lasso.mod.cv$lambda.min),
    standardize = T
  )

x_test_s <- toy_data(50, 234)
x_test_matrix <- model.matrix(x_test_s$y ~ ., x_test_s)[,-1]

mod0_mse <- mean((x_test_s$y - predict(mod0, x_test_s)) ^ 2)
ridge_mse <-
  mean((x_test_s$y - predict(ridge.final, x_test_matrix)) ^ 2)
lasso_mse <-
  mean((x_test_s$y - predict(lasso.final, x_test_matrix)) ^ 2)


print(paste("Linear model on new data gives MSE:", mod0_mse))
print(paste("Ridge model on new data gives MSE:", ridge_mse))
print(paste("Lasso model on new data gives MSE:", lasso_mse))

# Ridge has the coeffs close(ish) to 1 for x1 and -1 for x2, but doesn't set x3 and x4 close enough to zero
# Lasso knocks out the correlated and independent params, leaving just x1 and x2, relatively close to the
# expected coefficient values (more so with more training data).
print(ridge.final$beta)
print(lasso.final$beta)

ggplot() + geom_point(aes(x = 1:nrow(x_test_s), y = x_test_s$y - predict(mod0, x_test_s))) + 
  geom_point(aes(x = 1:nrow(x_test_s), y = x_test_s$y - predict(ridge.final, x_test_matrix)), color = "red") + 
  geom_point(aes(x = 1:nrow(x_test_s), y = x_test_s$y - predict(lasso.final, x_test_matrix)), color = "blue")

# Overall, in this example, it looks like the gains are mainly on paramter selection and inference, rather than pure prediction.
# The MSE of the linear model isn't worse in all cases (changing the train sample size can have an effect) - large sample size
# recovers the expected coefficients, but the linear model as lower MSE.
