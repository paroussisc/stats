###### Regression example for kidney data ########
library(ggplot2)
library(boot)

winsor <- function(x) {
  probs=c(0.25,0.75)
  xq<-quantile(x,probs=probs, type = 1)
  x[x < xq[1]]<-xq[1]
  x[x > xq[2]]<-xq[2]
  return(mean(x))
}

win_fun <- function(data, idx)
{
  dat <- data[idx, ]
  return(winsor(dat))
}

med_fun <- function(data, idx)
{
  dat <- data[idx, ]
  return(median(dat))
}

dat <- read.table('./gfr.txt', header = FALSE)
ggplot() + geom_histogram(data = dat, aes(x = V1), color = "green", binwidth = 3)

mean(dat$V1)
sd(dat$V1 /sqrt(dim(dat)[1]))

preds <- boot(dat, win_fun, 10000)
winsor(preds$t)
sd(preds$t)

preds <- boot(dat, med_fun, 10000)
median(preds$t)
sd(preds$t)


