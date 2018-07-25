library(plotly)
library(reshape2)

# Preprocessing
dat_orig <- read.table('./galaxy.txt', header = TRUE)
colnames(dat_orig) <- as.numeric(substring(colnames(dat_orig), 3))
colnames(dat_orig)[c(14, 15)] <-
  -as.numeric(colnames(dat_orig)[c(14, 15)])
dat <- melt(as.matrix(dat_orig))
colnames(dat) <- c("m", "r", "count")

# Fit model
mod <-
  glm(count ~  r + m + I(r ^ 2) + I(r * m) + I(m ^ 2),
      family = poisson(link = "log"),
      data = dat)

# Plot data
p <- plot_ly(z = as.matrix(dat_orig)) %>% add_surface()
p

# Predictions density
dat$pred <-
  apply(dat, 1, function(x) {
    predict(mod, data.frame(r = x['r'], m = x['m']), type = "response")
  })

p <-
  plot_ly(z = matrix(dat$pred, nrow(dat_orig), ncol(dat_orig))) %>% add_surface()
p

# Predictions contour
ggplot(data = dat, aes(x = -r, y = m, z = pred)) + geom_contour(aes(colour = ..level..), bins = 20)
