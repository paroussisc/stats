############# Solutions to question 6.1 #############

ll <- function(mu, sigma, alpha)
{
  sigma <- exp(sigma)
  alpha <- exp(alpha)
  dat <- (nhtemp - mu) / sigma
  return(sum(dt(dat, alpha, log = TRUE)) - log(sigma))
}

nll <- function(init)
{
  return(-ll(init[1], init[2], init[3]))
}

b <- c(50, 5, 4)
fit <- optim(b, nll)


## Metropolis-Hastings
n.rep <- 100000
n.params <- 3
theta <- matrix(0, n.rep, n.params)
theta[1, 3] <- rgeom(1, 0.05)
theta[1, 1] <- 45
n.accept <- 0
dat <- (nhtemp - theta[1, 1]) / exp(theta[1, 2])
ll0 <- sum(dt(dat, theta[1, 3], log = TRUE)) - theta[1, 2]

for (i in 2:n.rep)
{
  theta[i, ] <- theta[i - 1, ] + rnorm(3) * 0.1 ## proposal
  ll1 <- ll(theta[i, 1], theta[i, 2], theta[i, 3])
  if (exp(ll1 - ll0) > runif(1)) {
    ## MH accept/reject
    ll0 <- ll1
    n.accept <- n.accept + 1 ## accept
  } else
    theta[i, ] <- theta[i - 1, ] ## reject
}
n.accept / n.rep

# check convergence
qtplot <- function(theta,
                   n.plot = 100,
                   ylab = "") {
  ## simple MCMC chain diagnostic plot
  cuq <- Vectorize(function(n, x)
    ## cumul. quantile func.
    as.numeric(quantile(x[1:n], c(.025, .5, .975))),
    vectorize.args = "n")
  n.rep <- length(theta)
  plot(
    1:n.rep,
    theta,
    col = "lightgrey",
    xlab = "iter",
    ylab = ylab,
    type = "l"
  )
  iter <- round(seq(1, n.rep, length = n.plot + 1)[-1])
  tq <- cuq(iter, theta)
  lines(iter, tq[2, ])
  lines(iter, tq[1, ], lty = 2)
  lines(iter, tq[3, ], lty = 2)
}

# simulate data
idx <- runif(60, min = 2000, max = n.rep)

plot((theta[idx, 2]) * rt(length(idx), theta[idx, 3]) + theta[idx, 1])
qtplot(theta[, 1])
qtplot(theta[, 2])
qtplot(theta[, 3])


ll2 <- function(theta)
{
  mu <- theta[1]
  sigma <- theta[2]
  alpha <- theta[3]
  sigma <- exp(sigma)
  alpha <- exp(alpha)
  dat <- (nhtemp - mu) / sigma
  return(sum(dt(dat, alpha, log = TRUE)) - log(sigma))
}

library(mcmc)
md <- metrop(ll2, c(0, 0, 0), 500000)
