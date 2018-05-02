library(MASS)

setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

## Fit the model
fit <- stan(file = '6_6.stan', data = list(waiting=geyser$waiting, N=length(geyser$waiting)),
            iter = 1000, chains = 4)
fit

## Identifiability issues - some chains converge to different points, which affects the averages
traceplot(fit, "mu1")
traceplot(fit, "sig1")
traceplot(fit, "mu2")
traceplot(fit, "sig2")

## CIs on params
plot(
  fit,
  show_density = TRUE,
  ci_level = 0.95,
  fill_color = "red"
)

## Next try a better model
plot(geyser$duration, geyser$waiting)

## Plot looks like low waiting times give longer durations
## so this might help identify the distributions