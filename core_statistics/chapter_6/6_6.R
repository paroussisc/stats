library(MASS)

setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

# Seems to recover the parameters
fit <- stan(file = '6_6.stan', data = list(y=y, N=length(y)),
            iter = 10000, chains = 4)
fit

