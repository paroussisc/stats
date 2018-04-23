############# Solutions to question 5.6 #############

library(ggplot2)
library(MASS)

pf <- function(omega, mu1, sigma1, mu2, sigma2, data)
{
  return(
    omega * dnorm(data$waiting, mu1, sigma1) + (1 - omega) * dnorm(data$waiting, mu2, sigma2)
  )
}

pars <- c(0.4, 60, 5, 80, 14)
nll <- function(pars, data)
{
  return(-sum(log(
    pf(pars[1], pars[2], pars[3], pars[4], pars[5], data)
  )))
}

fit <-
  optim(
    pars,
    nll,
    data = geyser,
    method = "L-BFGS-B",
    lower = c(0,-Inf,-Inf,-Inf,-Inf),
    upper = c(1, Inf, Inf, Inf, Inf),
    hessian = TRUE
  )

# p = 0.5 not within CI
fisher <- solve(fit$hessian)
p_ci <- fit$par[1] + c(-1, 1) * sqrt(fisher[1, 1])

# not identifiable - very sensitive to starting values
# now simulate some values
rdnorm <- function(n, omega, mu1, sigma1, mu2, sigma2)
{
  return(omega * rnorm(n, mu1, sigma1) + (1 - omega) * rnorm(n, mu2, sigma2))
}

pars <- fit$par
n <- length(geyser$waiting)
p <- rbinom(n, 1, pars[1])
rs <-
  data.frame(y = p * rnorm(n, pars[2], pars[3]) + (1 - p) * rnorm(n, pars[4], pars[5]))

# This example seems to fit quite well - unless a large sample of simulations, in which case
# it is not smooth enough betwen distributions
ggplot() + geom_density(data = rs, aes(y), color = "red")  + geom_density(data = dat, aes(y), color = "blue")

# QQplot seems OK - all this sensitive to initial estimates though.
dat <- data.frame(x = sort(rs$y), y = sort(geyser$waiting))
ggplot() + geom_point(data = dat, aes(x, y), color = "red") + geom_abline()
