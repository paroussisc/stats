library(glmnet)

dat <- read.csv('./diabetes.csv', header = TRUE)

# scale by mean zero and sum of squares 1
dat_scaled <- data.frame(scale(dat[, 1:10]) / sqrt(nrow(dat)))
dat_scaled$prog <- dat$prog - mean(dat$prog)

# Linear model
mod0 <- lm(formula = prog ~ . - 1, data = dat_scaled)
summary(mod0)

# Ridge method
x <- model.matrix(prog ~ ., dat_scaled)[,-1]
y <- dat$prog - mean(dat$prog)
lambdas <- 10 ^ seq(3, -2, by = -.5)

ridge.mod <-
  glmnet(
    x,
    y,
    alpha = 0,
    lambda = seq(0, 0.25, 0.01),
    standardize = F
  )
plot(ridge.mod)

ridge.mod2 <-
  cv.glmnet(x,
            y,
            alpha = 0,
            standardize = F)

print(paste("Minimum MSE lambda is: ", ridge.mod2$lambda.min))
