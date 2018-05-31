dat <- read.table('./baseball.txt', header = TRUE)

n <- 90
p <- mean(dat$MLE)
v <- p * (1 - p) / n
N <- nrow(dat)

dat$JS <-
  p + (1 - (N - 3) * v / sum((dat$MLE - p) ^ 2)) * (dat$MLE - p)

print(paste("MSE of MLE: ", sum((dat$MLE - dat$TRUTH) ^ 2)))
print(paste("MSE of JS: ", sum((dat$JS - dat$TRUTH) ^ 2)))

