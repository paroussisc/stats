
N0 <- 10
sig_e <- 1
K <- 20
r <- 3.8
size <- 50

next_n <- function(n0, K, sig_e,r)
{
  return(exp(r)*n0*exp(-n0/(K) + rnorm(1,0,sig_e)))
}


n <- rep(0,size)
n[1] <- (N0)
for (i in 2:size)
{
  n[i] <- next_n(n[i-1], K, sig_e, r)
}

y <- rpois(length(n), n)

library("rstan")
setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

# Seems to recover the parameters
fit <- stan(file = '6_6.stan', data = list(y=y, N=length(y)),
            iter = 10000, chains = 4)
fit

