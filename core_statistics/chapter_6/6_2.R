library(rstan)
library(ggplot2)
library(dplyr)

setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())
N <- length(nhtemp)

fit <-
  stan(
    file = '6_2.stan',
    data = list(y = c(nhtemp), N = N),
    init = 0,
    iter = 1000,
    chains = 4
  )
fit

# CIs on params
plot(
  fit,
  show_density = TRUE,
  ci_level = 0.95,
  fill_color = "purple"
)

# chains look to have converged
traceplot(fit, "mu")
traceplot(fit, "sigma")
traceplot(fit, "alpha")

# check posterior simulations
simulated_data  <- fit %>%
  as.data.frame %>%
  select(contains("y_pred"))

# Seems legit
ggplot(data = simulated_data) + geom_point(aes(x = 1:nrow(simulated_data), y = simulated_data))

# Next try time trend
t <- 1912:1971 - 1912
fit_trend <-
  stan(
    file = '6_2_time_trend.stan',
    data = list(y = c(nhtemp), N = N, t = t),
    init = 0,
    iter = 1000,
    chains = 4
  )

# CI on beta is >0 - temperature looks to be increasing!
fit_trend

# Looks to be better with a time trend in the model.
dic <- -2 * sum(data.frame(extract(fit))$lp__) + 3
dic_trend <- -2 * sum(data.frame(extract(fit_trend))$lp__) + 4
