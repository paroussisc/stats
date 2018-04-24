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