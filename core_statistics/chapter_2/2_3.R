############# Solutions to question 2.3 #############
library(ggplot2)


ll <- function(mu, sigma)
{
  alpha = 3
  return (sum(log((1/sigma) * dt((nhtemp - mu)/sigma, 3))))
}

# ggplot contour
mus <- seq(30,70, 0.1)
sigs <- seq(0.1,15,0.1)
df.lik <- setNames(expand.grid(mus, sigs), c('mu', 'sigma')) 
vfun1 <- Vectorize(ll, SIMPLIFY = TRUE)

df.lik$z <- vfun1(df.lik$mu,df.lik$sigma)       
p <- ggplot(df.lik, aes(mu, sigma, z=z)) + stat_contour(aes(colour = ..level..))
p

