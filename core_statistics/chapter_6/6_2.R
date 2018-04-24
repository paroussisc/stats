library("rstan")
setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())
N <- length(nhtemp)

fit <- stan(file = '6_2.stan', data = list(y = c(nhtemp), N = N), init = 0,
            iter = 1000, chains = 4)
fit

