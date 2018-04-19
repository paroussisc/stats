############# Solutions to question 5.4 #############

t <- 2:14
y <- c(35, 33, 33, 39, 24, 25, 18, 20, 23, 13, 14, 20, 18)

d_nll <- function(delta, t, y)
{
  rate <- 50 * exp(-delta * t)
  return(-sum(dpois(y, rate, log = TRUE)))
}

d0 <- 1
o <- optim(
  d0,
  d_nll,
  method = "BFGS",
  hessian = TRUE,
  t = t,
  y = y
)

# Large sample estimate of CI
se <- sqrt(1 / o$hessian)

print("Large sample CI:")
print(o$par + c(-1, 1) * se)

# Generalised likelihood ratio test CI:
glrt <- function(d, d_mle, t, y)
{
  -2 * (d_mle - d_nll(d, t = t, y = y)) - qchisq(0.95, 1)
}


ci.lower <- uniroot(glrt,
                    c(0, o$par),
                    d_mle = o$value,
                    t = t,
                    y = y)$root
ci.upper <- uniroot(glrt,
                    c(o$par, 1),
                    d_mle = o$value,
                    t = t,
                    y = y)$root

print("GLRT CI:")
print(c(ci.lower, ci.upper))