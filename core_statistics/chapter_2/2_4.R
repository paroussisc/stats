############## Solutions to question 2.4 #############
library(ggplot2)

ll <- function(theta, data, data_cens)
{
  lik = sum(dexp(data, theta, log = TRUE))
  cens_lik = sum(-theta * data_cens)
  return (lik + cens_lik)
}


llf <- function(theta, data, data_cens, mll_z, alpha)
{
  return(2 * (mll_z - ll(theta, data, data_cens)) - qchisq(1 - alpha, 1))
}

allo <- c(28, 32, 49, 84, 357)
allo_cens <- c(933, 1078, 1183, 1560, 2114, 2144)

# ggplot contour
thetas <- seq(0.00001, 0.001, 0.00000001)
df.lik <- setNames(expand.grid(thetas), c('theta'))
df.lik$z  <-
  apply(df.lik, 1, function(x) {
    ll(x[1], data = allo, data_cens = allo_cens)
  })
ggplot(df.lik, aes(theta, z = z)) + geom_point(aes(x = theta, y = z))

# max param and likelihood values
idx <-  which.max(df.lik$z)
mll <- df.lik[idx, ]$theta
mll_z <- df.lik[idx, ]$z

# CI - generalised likelihood ratio test
alpha <- 0.05
l <-
  uniroot(
    llf,
    c(.00000000, mll),
    data = allo,
    data_cens = allo_cens,
    mll_z = mll_z,
    alpha = alpha,
    check.conv = TRUE,
    tol = .Machine$double.eps
  )$root # lower
u <-
  uniroot(
    llf,
    c(mll, 1),
    data = allo,
    data_cens = allo_cens,
    mll_z = mll_z,
    alpha = alpha,
    check.conv = TRUE,
    tol = .Machine$double.eps
  )$root # upper

# interval on days
print(c(1 / u, 1 / l))
