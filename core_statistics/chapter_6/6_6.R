library(MASS)
library(ggplot2)
library(rstan)

setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

## Fit the model
fit <-
  stan(
    file = '6_6.stan',
    data = list(waiting = geyser$waiting, N = length(geyser$waiting)),
    iter = 1000,
    chains = 4
  )
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
## Priors can fix the identifiability issues
## Fit the model
fit_ext <-
  stan(
    file = '6_6_extended.stan',
    data = list(
      waiting = geyser$waiting,
      duration = geyser$duration ,
      N = length(geyser$waiting)
    ),
    iter = 1000,
    chains = 4
  )
fit_ext

traceplot(fit_ext, "mu_w1")
traceplot(fit_ext, "sig_w1")
traceplot(fit_ext, "mu_w2")
traceplot(fit_ext, "sig_w2")
traceplot(fit_ext, "mu_d1")
traceplot(fit_ext, "sig_d1")
traceplot(fit_ext, "mu_d2")
traceplot(fit_ext, "sig_d2")

# CIs on params
plot(
  fit_ext,
  show_density = TRUE,
  ci_level = 0.95,
  fill_color = "purple"
)

## Look at posterior samples
w_pred <- extract(fit_ext, 'w_pred')
w_pred <- unlist(w_pred, use.names=FALSE)
d_pred <- extract(fit_ext, 'd_pred')
d_pred <- unlist(d_pred, use.names=FALSE)
