############# Solutions to question 5.5 #############

library(ggplot2)

t <- 1981:1993
t <- t -  1980
y <- c(12, 14, 33, 50, 67, 74, 123, 141, 165, 204, 253, 246, 240)

nll_linear <- function(theta, t, y)
{
  alpha = theta[1]
  beta = theta[2]
  return(-sum(dpois(y, alpha * exp(beta * t), log = TRUE)))
}

nll_quad <- function(theta, t, y)
{
  alpha = theta[1]
  beta = theta[2]
  return(-sum(dpois(y, alpha * exp(beta * beta * t), log = TRUE)))
}

p <- c(4, 1.35)
fit.linear <- optim(p, nll_linear, y = y, t = t)
fit.quad <- optim(p, nll_quad, y = y, t = t)

print(fit.linear$value - fit.quad$value)

# AIC
aic.l <- 2 * (-fit.linear$value - 2)
aic.q <- 2 * (-fit.quad$value - 2)

# GLRT
l <- 2 * (-fit.quad$value + fit.linear$value)
pchisq(-l, 1)

# Doesn't make sense to use GLRT - the two models are not nested, and there isn't a simple restriction on the
# parameters that we can compare.
actual <-  data.frame(t = t, y = y)
lin.pred <-
  data.frame(t = t, y = fit.linear$par[1] * exp(fit.linear$par[2] * t))
quad.pred <-
  data.frame(t = t, y = fit.quad$par[1] * exp(fit.quad$par[2] * fit.quad$par[2] * t))

ggplot() + geom_line(data = lin.pred, aes(t, y), color = "red") + geom_line(data = quad.pred, aes(t, y), color = "blue") +
  geom_line(data = actual, aes(t, y), color = "green")
