library(ggplot2)

dat <- read.table('./student_score.txt', header = TRUE)

prior_unif <- function(x)
{
  return(0.5)
}

prior_jeff <- function(x)
{
  return(1 / (1 - x ^ 2))
}

prior_shrink <- function(x)
{
  return((1 - abs(x)))
}

lik <- function (theta, theta_h, data)
{
  n <- nrow(data)
  left <-
    (n - 2) * (1 - theta ^ 2) ^ ((n - 1) / 2) * (1 - theta_h ^ 2) ^ ((n -
                                                                        4) / 2)
  f <- function(w, theta, theta_h)
  {
    return((cosh(w) - theta * theta_h) ^ (1 - n))
  }
  
  right <-
    integrate(f, 0, Inf, theta = theta, theta_h = theta_h)$value
  return(left * right)
  
}

post_unif <- function(x)
  prior_unif(x) * lik(x, theta_h, dat)
post_jeff <- function(x)
  prior_jeff(x) * lik(x, theta_h, dat)
post_shrink <- function(x)
  prior_shrink(x) * lik(x, theta_h, dat)

theta_h <- cor(dat$mech, dat$vecs)

df <- data.frame(x = seq(-0.3, 0.9999, 0.001))
y_flat <- apply(df,
                1,
                FUN = post_unif)
y_jeff <- apply(df,
                1,
                FUN = post_jeff)
y_shrink <- apply(df,
                  1,
                  FUN = post_shrink)

# normalise
df$y_flat <-
  y_flat / integrate(Vectorize(function(x)
    prior_unif(x) * lik(x, theta_h, dat)),
    lower = 0,
    upper = 1)$value
df$y_jeff <-
  y_jeff / integrate(Vectorize(function(x)
    prior_jeff(x) * lik(x, theta_h, dat)),
    lower = 0,
    upper = 1)$value
df$y_shrink <-
  y_shrink / integrate(Vectorize(function(x)
    prior_shrink(x) * lik(x, theta_h, dat)),
    lower = 0,
    upper = 1)$value

ggplot(data = df) + geom_line(aes(x = x, y = y_flat)) +
  geom_line(aes(x = x, y = y_jeff), color = 'red', linetype = 'dashed') +
  geom_line(aes(x = x, y = y_shrink), color = 'blue', linetype = 'dotted') + ylab('density') + xlab('theta')
