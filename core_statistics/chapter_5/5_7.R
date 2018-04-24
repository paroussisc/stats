############# Solutions to question 5.7 #############

library(faraway)
library(dplyr)
v0e <-
  expression(((
    st * exp(a + gam_k + delt_m + phi_q + height + b_j) / (1 + exp(a + gam_k + delt_m + phi_q + height + b_j)) +
      (1 - st) * (1 - exp(a + gam_k + delt_m + phi_q + height + b_j) / (1 + exp(
        a + gam_k + delt_m + phi_q + height + b_j
      )))
  ) *
    (1 / sqrt(2 * pi * sig_b ^ 2) * exp(
      -(b_j - mu) ^ 2 / (2 * sig_b ^ 2)
    ))))

v0 <-
  deriv(
    v0e,
    c("b_j"),
    hessian = TRUE,
    function.arg = c(
      "a",
      "gam_k",
      "delt_m",
      "phi_q",
      "height",
      "b_j",
      "mu",
      "sig_b",
      "st"
    )
  )

lfyb <- function(b, beta, x, v0, sig_b, Z)
{
  v <- v0(
    x$X.Intercept. * beta[1],
    x$Sexmale * beta[2],
    x$Surfacenorm * beta[3],
    x$Visiondome * beta[4] + x$Visionopen * beta[5],
    x$Height * beta[6],
    Z %*% b,
    0,
    sig_b,
    ctsib$stable
  )
  
  vv <- data.frame(v)
  vv$subject <- ctsib$Subject
  nll <- data.frame(vv %>%
                      summarise(nll = sum(v)))
  
  gr <- data.frame(attr(v, "gradient"))
  gr$subject <- ctsib$Subject
  gr <- data.frame(gr %>%
                     group_by(subject) %>%
                     summarise(log_grad = sum(b_j)))
  
  hs <- data.frame(attr(v, "hessian"))
  hs$subject <- ctsib$Subject
  hs <- data.frame(hs %>%
                     group_by(subject) %>%
                     summarise(log_grad = sum(b_j.b_j)))
  return(list(
    lf = nll$nll,
    g = as.numeric(gr$log_grad),
    H = diag(hs$log_grad)
  ))
}


b <- rep(0.0, 40)
beta <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
sig_b <- 2.0

lf <- lfyb(b, beta, x, v0, sig_b, Z)

pdR <- function(H,
                k.mult = 20,
                tol = .Machine$double.eps ^ .8) {
  k <- 1
  tol <- tol * norm(H)
  n <- ncol(H)
  while (inherits(try(R <- chol(H + (k - 1) * tol * diag(n)),
                      silent = TRUE)
                  ,"try-error"))
    k <- k * k.mult
  return(R)
}

# Laplace approximation
llu <- function(beta, x, v0, sig_b, Z) {
  tol <- .Machine$double.eps ^ .8
  if (exists(".inib", envir = environment(llu))) {
    b <- get(".inib", envir = environment(llu))
  } else
    b <- c(rep(0.0, 40))
  ## init
  lf <- lfyb(b, beta, x, v0, sig_b, Z)
  
  for (i in 1:500) {
    ## Newton loop...
    R <- pdR(-lf$H) ## R’R = (perturbed) Hessian
    step <- backsolve(R, forwardsolve(t(R), lf$g)) ## Newton
    conv <- ok <- FALSE
    while (!ok) {
      ## step halving
      lf1 <- lfyb(b + step, beta, x, v0, sig_b, Z)
      
      if (!is.na(lf1$lf) &&
          sum(abs(lf1$g) > abs(lf1$lf) * tol) == 0)
        conv <- TRUE
      kk <- 0
      if (!conv && kk < 30 &&
          (!is.finite(sum(lf1$lf)) || lf1$lf < lf$lf)) {
        step <- step / 2
        kk <- kk + 1
      } else
        ok <- TRUE
    }
    lf <- lf1
    b <- b + step
    if (kk == 30 || conv)
      break ## if converged or failed
  } ## end of Newton loop
  
  assign(".inib", b, envir = environment(llu))
  R <- pdR(-lf$H, 10)
  ll <- lf$lf - sum(log(diag(R))) + log(2 * pi) * 40
  return(-ll)
}

beta <- rep(0, 6)
fit <-
  optim(
    beta,
    llu,
    method = "BFGS",
    x = x,
    v0 = v0,
    sig_b = 1.1,
    Z = Z,
    hessian = TRUE
  )

