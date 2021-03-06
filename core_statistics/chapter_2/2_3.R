############# Solutions to question 2.3 #############
library(ggplot2)

ll <- function(mu, sigma, y)
{
  alpha <- 3
  n <- length(y)
  return (n * log(1/sigma) + sum(dt((y - mu)/sigma, alpha, log = TRUE)))
}

# ggplot contour
mus <- seq(30,70, 0.1)
sigs <- seq(0.1,15,0.1)
df.lik <- setNames(expand.grid(mus, sigs), c('mu', 'sigma')) 
vfun1 <- Vectorize(ll, SIMPLIFY = TRUE)

df.lik$z  <- apply(df.lik, 1, function(x){ll(x[1], x[2], y = nhtemp)})
ggplot(df.lik, aes(mu, sigma, z=z)) + stat_contour(bins = 30, aes(colour = ..level..))
