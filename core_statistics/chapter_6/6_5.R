
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

# Needs more work
fit <- stan(file = '6_5.stan', data = list(y=y, N=size),
            iter = 1000, chains = 4)
fit

# CIs on params
plot(
  fit$,
  show_density = TRUE,
  ci_level = 0.95,
  fill_color = "purple"
)

# chains look to have converged
traceplot(fit, "K")
traceplot(fit, "r")
traceplot(fit, "sig_e")

