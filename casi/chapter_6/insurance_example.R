marg_density <- function(pars, x)
{
  v <- pars[1]
  sig <- pars[2]
  g <- sig / (1 + sig)
  return(g ^ (v + x) * gamma(v + x) / (sig ^ v * gamma(v) * factorial(x)))
}

nll_gamma <- function(pars, data)
{
  return (-sum(data * log(marg_density(
    pars, c(0, 1, 2, 3, 4, 5, 6, 7)
  ))))
}

robins <- function(x, f_x, f_x1)
{
  return((x + 1) * f_x1 / f_x)
}


#######################################################################
data <- c(7840, 1317, 239, 42, 14, 4, 4, 1)

thetas <- rep(0, length(data))

# Marginal density is the empirical mean here
fx <- data / sum(data)

for (i in 1:(length(data) - 1))
{
  thetas[i] <- robins(i - 1, fx[i], fx[i + 1])
}

# As in the table in the example...
print(thetas)

# Try assumed gamma prior + marginal density
par <- optim(c(1, 10), nll_gamma, data = data)$par
thetas_g <- rep(0, length(data))
fx <-
  marg_density(par, c(0, 1, 2, 3, 4, 5, 6, 7))

for (i in 1:(length(data) - 1))
{
  thetas_g[i] <- robins(i - 1, fx[i], fx[i + 1])
}
