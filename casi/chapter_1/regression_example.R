###### Regression example for kidney data ########
library(boot)

dat <- read.table('./kidney.txt', header = TRUE)
mod <- lm(tot ~ age, data = dat)
yerr <- summary(mod)$sigma

new_dat <- data.frame(age = seq(20, 80, 10))
new_dat <-
  cbind(new_dat, predict(mod, new_dat, interval = "confidence"))
colnames(new_dat)[2] <- "tot"

ggplot(dat, aes(x = age, y = tot)) + geom_point() + geom_line(data = new_dat,
                                                              aes(x = age, y = tot),
                                                              color = "red") +  geom_errorbar(data = new_dat,
                                                                                              aes(x = age, ymin = lwr, ymax = upr),
                                                                                              color = "blue")

# Lowess
mod_l <- data.frame(lowess(dat$age, dat$tot, 1 / 3))
ggplot(dat, aes(x = age, y = tot)) + geom_point()  + geom_line(data = mod_l, aes(x = x, y = y), color =
                                                                 "red")

# boostrap
low_fun <- function(data, idx)
{
  dat <- data[idx, ]
  mod_l <- data.frame(lowess(dat$age, dat$tot, 1 / 3))
  return((mod_l$y))
}

preds <- boot(dat, low_fun, 157)$t
d <-
  data.frame(age = dat$age,
             tot = colMeans(preds),
             sd = apply(preds, 2, sd))

ggplot(dat, aes(x = age, y = tot)) + geom_point() + geom_line(data = d,
                                                              aes(x = age, y = tot),
                                                              color = "red") +  geom_errorbar(data = d[seq(1, 157, 15), ],
                                                                                              aes(
                                                                                                x = age,
                                                                                                ymin = tot - sd,
                                                                                                ymax = tot + sd
                                                                                              ),
                                                                                              color = "blue")
