library(ggplot2)

dat <- read.table('./butterfly.txt', header = TRUE)
t = seq(0, 1, 0.1)
means <-
  data.frame(t = t,
             e = rep(0, length(t)),
             sd = rep(0, length(t)))

for (j in 1:nrow(means))
{
  i <- means[j, ]$t
  pos <- seq(1, nrow(dat), 2)
  neg <- seq(2, nrow(dat), 2)
  e <- dat$y * i ^ dat$x
  means[j, ]$e <- sum(e[pos] - e[neg])
  
  ## could the book be wrong?
  means[j, ]$sd <- sqrt(sum(dat$y * i ^ (2 * dat$x)))
}