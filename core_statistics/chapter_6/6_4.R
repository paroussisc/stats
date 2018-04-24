allo <- c(28, 32, 49, 84, 357)
allo_cens <- c(933, 1078, 1183, 1560, 2114, 2144)
auto <- c(42, 53, 57, 63, 81, 140, 176, 252, 524)
auto_cens <- c(210, 476, 1037)


library("rstan")
setwd("~/workspace/stats/core_statistics/chapter_6")
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())


# Scale inputs? Check why some threads error - could be my dodgy theta manipulation
fit <-
  stan(
    file = '6_4.stan',
    data = list(
      al_n = length(allo),
      allo = allo,
      al_n_c = length(allo_cens),
      allo_c = allo_cens,
      au_n = length(auto),
      aut = auto,
      au_n_c = length(auto_cens),
      aut_c = auto_cens
    ),
    init = 0,
    iter = 10000,
    chains = 4
  )

fit
s <- as.data.frame(extract(fit))
l <- sort(1 / s$theta_l)
u <- sort(1 / s$theta_u)

# allo is better according to this CI!
l[c(50, 1950)]
u[c(50, 1950)]
