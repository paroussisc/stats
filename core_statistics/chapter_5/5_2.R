############# Solutions to question 5.1 #############

Rosenbrock <- function(x, z)
{
  a <- 10
  b <- 1
  a * (z - x ^ 2) ^ 2 + (b - x) ^ 2
}

# create custom Newton solver
library(pracma)
library(Deriv)

n_solver <- function(theta0, fn)
{
  theta <- theta0
  grd_fn <- Deriv(fn)
  hess_fn <- Deriv(grd_fn)
  tol <- 1e-3
  
  for (i in 1:500)
  {
    f <- fn(theta[1], theta[2])
    grad <- grd_fn(theta[1], theta[2])
    hess <- matrix(hess_fn(theta[1], theta[2]), 2, 2)
    
    # check for convergence
    if (all(abs(grad) < tol) && isposdef(hess, psd = TRUE))
    {
      break
    }
    
    if (!isposdef(hess))
    {
      # decompose and use abs eigen values
      B <- eigen(hess)
      U <- B$vectors
      L <- diag((B$values))
      hess <- U%*%L%*%t(U)
    }
    
    delt <- -solve(hess) %*% grad
    
    # check to see if step is too large (will be in the right direction so
    # if it does not give a lower value then the step is too large).
    while (fn(theta[1] + delt[1], theta[2] + delt[2]) > fn(theta[1], theta[2]))
    {
      delt <- delt / 2
    }
    
    theta <- theta + delt
  }
  
  return (theta)
}

theta0 <- c(0.5, 0.5)
fit <- n_solver(theta0, Rosenbrock)
