dat <- read.table('./cellinfusion.txt', header = TRUE)
dat$ratio <- as.factor(dat$ratio)
dat$time <- as.factor(dat$time)

mod <-
  glm(
    (dat$thrived / dat$N) ~ ratio + time,
    family = binomial,
    data = dat,
    weights = N
  )
p <- predict(mod, type = "response")
print(matrix(round(p, 2), 5, 5))

