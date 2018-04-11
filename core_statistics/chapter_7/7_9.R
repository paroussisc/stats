############# Solutions to question 7.9 ############# 
library(MASS)

mod <- lm(time ~ dist + climb, data = hills)

# qq-plot and errors aren't great:
plot(mod)

# plots aren't great here also
mod1 <- lm(time ~ dist*climb, data = hills)

# take away insignificant terms
mod2 <- lm(time ~ dist + dist:climb - 1, data = hills)

# qq-plots are awful so look at non-linear terms
mod3 <- lm(time ~ dist + dist:I(climb^2) - 1, data = hills)

#  Could just be that we have some outliers in the data!

# This race looks fairly standard
plot(hills$dist, hills$climb)
race <- data.frame(dist = 7.0, climb = 2400)

# our estimate
predict(mod3, race, interval = "confidence")
