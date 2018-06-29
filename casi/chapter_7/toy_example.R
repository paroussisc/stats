# Simulate data
set.seed(123)
N <- 50

x <- data.frame(x1 = rep(0, N))
x$x1 <- 10 * runif(n = N)
x$x2 <- 100 * runif(n = N)
x$x3 <- 1000 * (5 * x1 + runif(n = N))
x$x4 <- 10000 * (2 * x2 + x3 + runif(n = N))
epsilon <- rnorm(n = N)
y <- x1 + x2 + epsilon

# scale predictors
x_s <- data.frame(scale(dat) / sqrt(nrow(x)))
y <- y - mean(y)

# First, try a linear model
mod0 <- lm(y ~ . - 1, data = x)

# All predictors appear significant here
summary(mod0)

# Plots look OK:
# - residuals imply that the data are linear
# - Q-Q plot looks fine - residuals appear gaussian
# - scale-location doesn't argue against constant variance
# - residuals vs leverage show there is no outlier, no point too influential
plot(mod0)

# Next we look at parameters shrinkage