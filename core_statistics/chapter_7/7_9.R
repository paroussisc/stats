############# Solutions to question 7.9 #############
library(MASS)

mod <- lm(time ~ dist + climb, data = hills)

# qq-plot and errors aren't great:
plot(mod)

# plots aren't great here also
mod1 <- lm(time ~ dist * climb, data = hills)

# take away insignificant terms
mod2 <- lm(time ~ dist + dist:climb - 1, data = hills)

# qq-plots are awful so look at non-linear terms
mod3 <- lm(time ~ dist + dist:I(climb ^ 2) - 1, data = hills)

AIC(mod1, mod2, mod3)

# Clear issue with outliers as per:
# https://projecteuclid.org/euclid.ss/euclid.ss/1177013624
# 7, 18, 33 highly influential
# time of year is missing as a covariate in the dataset

# Refit without outliers
mod1 <-
  lm(time ~ dist * climb - 1, data = hills[-c(7, 11, 18, 33, 35), ])
mod2 <-
  lm(time ~ dist + dist:climb - 1, data = hills[-c(7, 11, 18, 33, 35), ])
mod3 <-
  lm(time ~ dist + dist:I(climb ^ 2) - 1, data = hills[-c(7, 11, 18, 33, 35), ])

# mod1 better by AIC
AIC(mod1, mod2, mod3)

# can't say the extra term in mod1 is signficant
# necessarily, at least by ANOVA
anova(mod2, mod1)
summary(mod1)
plot(mod1)

# This race looks fairly standard
plot(hills$dist, hills$climb)
race <- data.frame(dist = 7.0, climb = 2400)

# our estimate
predict(mod1, race, interval = "confidence")
