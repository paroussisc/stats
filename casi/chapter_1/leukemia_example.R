library(ggplot2)

leukemia_big <- read.csv("./leukemia_big.csv")

# Get the two groups in a nice format
aml_cols <-
  grep("AML*",
       colnames(leukemia_big),
       value = TRUE,
       ignore.case = T)
all_cols <-
  grep("ALL*",
       colnames(leukemia_big),
       value = TRUE,
       ignore.case = T)

aml <- leukemia_big[, aml_cols]
all <- leukemia_big[, all_cols]

t_test <- function(row_vec, aml_cols, all_cols)
{
  t.test(row_vec[aml_cols], row_vec[all_cols])$statistic
}

ts <- data.frame(t.stat = apply(leukemia_big, 1, function(x)
  t_test(x, aml_cols = aml_cols, all_cols = all_cols)))

t.rand <- data.frame(x = rt(dim(leukemia_big)[1], 70))

# implies something wrong with the theoretical null distribution - the samples aren't following a student t distribution
ggplot() + geom_histogram(data = ts, aes(x = t.stat), color = "green") + geom_freqpoly(data = t.rand, aes(x = x), color = "red")
