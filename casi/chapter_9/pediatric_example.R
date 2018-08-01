library(survival)

dat <- read.table('./pediatric.txt', header = TRUE)
dat[,c("age","entry","far")]  <- scale(dat[,c("age","entry","far")])

coxph(Surv(t,d) ~ ., data = dat)
