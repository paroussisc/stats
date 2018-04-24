library("rstan")
setwd("~/workspace/s_wood/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())
N <- length(nhtemp)

times <- 1912:1971
t <- (times-mean(times))/sd(times)
fit3 <- stan(file = 'norm.stan', data = list(y = c(nhtemp), t = t, N = N), init = 0,
            iter = 1000, chains = 4)
fit3

# first try prior on mu
# now with time trend

library("bayesplot")
library("ggplot2")
library("rstanarm")   

yrep <- posterior_predict(fit3, draws = 500)
s <- as.data.frame(extract(fit3))
