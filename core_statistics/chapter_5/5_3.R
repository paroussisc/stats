############# Solutions to question 5.1 #############

Rosenbrock <- function(x, z)
{
  a <- 10
  b <- 1
  a * (z - x ^ 2) ^ 2 + (b - x) ^ 2
}

# create custom BFGS solver
library(pracma)
library(Deriv)
library(nlme)


b_solver <- function(theta0, fn)
{
  grd_fn <- Deriv(fn)
  c1 <- 10e-4
  c2 <- 0.9
  tol <- 1e-4
  theta_k <- zeros(length(theta0))
  theta_k1 <- theta0
  
  # initial step with fd  hessian
  
  f <- fn(theta_k1[1], theta_k1[2])
  grad <- grd_fn(theta_k1[1], theta_k1[2])
  
  fnn <- function(t)
    fn(t[1], t[2])
  
  hess <- fdHess(theta_k1, fnn)$Hessian
  
  delt <- -solve(hess) %*% grad
  
  while (fn(theta_k1[1] + delt[1], theta_k1[2] + delt[2]) > fn(theta_k1[1], theta_k1[2]))
  {
    delt <- delt / 2
  }
  
  theta_k <- theta_k1
  theta_k1 <- theta_k1 + delt
  B_k <- solve(hess)
  
  for (i in 1:1000)
  {
    grad <- grd_fn(theta_k1[1], theta_k1[2])
    
    if (all(abs(grad) < tol) && isposdef(B_k, psd = TRUE))
    {
      break
    }
    
    s_k <- theta_k1 - theta_k
    y_k <-
      grd_fn(theta_k1[1], theta_k1[2]) - grd_fn(theta_k[1], theta_k[2])
    rho_k <- sum((t(s_k) %*% y_k) ^ -1)
    B_k <-
      (diag(length(theta_k1)) - rho_k * s_k %*% t(y_k)) %*% B_k %*% (diag(length(theta_k1)) - rho_k * y_k %*% t(s_k)) +
      rho_k * s_k %*% t(s_k)
    
    #update
    delt <- -B_k %*% grad
    grad_new <- grd_fn(theta_k1[1] + delt[1], theta_k1[2] + delt[2])
    while (fn(theta_k1[1] + delt[1], theta_k1[2] + delt[2]) >= fn(theta_k1[1], theta_k1[2]) + c1 * t(grad) %*% delt
           || t(grad_new) %*% delt <= c2 * t(grad) %*% delt)
    {
      delt <- delt / 2
    }
    
    theta_k <- theta_k1
    theta_k1 <- theta_k1 + delt
    
  }
  
  return (theta_k1)
}

w <- c(-1, 1)
b_solver(w, Rosenbrock)

