############# Solutions to question 2.6 #############
x <- rnorm(10) + 1

# integrate params out of prior * likelihood to get marginal distribution of the model
bayes_2 <- function(theta)
{
  return(dunif(theta,-2, 2, log = TRUE) + sum(dnorm(x, theta, 1, log = TRUE)))
}

bayes_20 <- function(theta)
{
  return(dunif(theta,-20, 20, log = TRUE) + sum(dnorm(x, theta, 1, log = TRUE)))
}

# Integrate over params and evaluate Bayes' factor
M0 <- integrate(bayes_2, -2, 2)
M1 <- integrate(bayes_20, -20, 20)

# value of >10 is very strong evidence against M0
print(2 * log(M1$value / M0$value))

# this highlights the issue of Bayes' factor being sensitive to priors -
# it is only worth using this factor to compare models if the priors are
# precise and meaningful representations of prior knowledge.
