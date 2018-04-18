############# Solutions to question 5.1 #############

Rosenbrock <- function(x, z, a = 100, b = 1)
{
  return(a * (z - x ^ 2) ^ 2 + (b - x) ^ 2)
}

# ggplot contour
xs <- seq(-1.5, 1.5, 0.01)
zs <- seq(-0.5, 1.5, 0.01)
df.lik <- setNames(expand.grid(xs, zs), c('x', 'z'))
vfun1 <- Vectorize(Rosenbrock, SIMPLIFY = TRUE)

df.lik$f <- vfun1(df.lik$x, df.lik$z)
p <-
  ggplot(df.lik, aes(x, z, z = f)) + stat_contour(aes(colour = ..level..))
p


# use optim
ll_rosen <- function(theta)
{
  x <- theta[1]
  z <- theta[2]
  return(Rosenbrock(x, z))
}

theta0 <- c(0, 0)
bfgs <- optim(theta0, ll_rosen, method = "BFGS")
nelder <- optim(theta0, ll_rosen, method = "Nelder-Mead")

print(paste("Difference in BFGS and Nelder-Mead:", bfgs$par - nelder$par))

# nlm
bfgs_nlm <- nlm(ll_rosen, theta0)
bfgs_nlminb <- nlminb(theta0, ll_rosen)
