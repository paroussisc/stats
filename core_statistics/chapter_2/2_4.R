############## Solutions to question 2.4 #############
library(ggplot2)

ll <- function(theta)
{
  allo <- c(28, 32, 49, 84, 357)
  allo_cens <- c(933, 1078, 1183, 1560, 2114, 2144)
  
  allo_lik = sum(dexp(allo, theta, log = TRUE))
  allo_cens_lik = sum(-theta * allo_cens)
  return (allo_lik + allo_cens_lik)
}

llf <- function(theta)
{
  return(2*(mll_z - ll(theta)) - qchisq(0.95,1))
}

# ggplot contour
thetas <- seq(0.00001, 0.001, 0.00001)
df.lik <- setNames(expand.grid(thetas), c('theta'))
vfun1 <- Vectorize(ll, SIMPLIFY = TRUE)

df.lik$z <- vfun1(df.lik$theta)
ggplot(df.lik, aes(theta, z = z)) + geom_point(aes(x = theta, y = z))

# max
mll <- df.lik[52, ]$theta
mll_z <- df.lik[52, ]$z

# CI - generalised likelihood ratio test
uniroot(llf,c(.000000001,mll))$root # lower
uniroot(llf,c(mll,1))$root # upper
