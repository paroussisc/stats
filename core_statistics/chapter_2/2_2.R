############# Solutions to question 2.2 #############
library(ggplot2)

mu <- mean(nhtemp)
sig <- sd(nhtemp)
sorted <- sort(nhtemp)

theoretical_q <- qnorm(c(seq(0.01,0.99,1/(length(nhtemp)+1))), mu, sig)
dat <- data.frame(theor = theoretical_q, real = sorted)

# Could be adequate but not great in the tails.
ggplot(dat, aes(theor, real)) + geom_point() + geom_abline()
