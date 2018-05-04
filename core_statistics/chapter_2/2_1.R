############# Solutions to question 2.1 #############
n <- length(nhtemp)
mu <- mean(nhtemp)
sig <- sd(nhtemp)
st_err <- sig/sqrt(n)
ci_mu <- qnorm(c(0.025, 0.975), mu, st_err)
ci_alt <- mu + st_err * qnorm(c(0.025, 0.975))

# https://en.wikipedia.org/wiki/Standard_deviation
# (n-1) * sigma_hat^2/sigma^2 \sim chisq_{n-1}
# comes from sum of n lots of N(0,1)^2 is chi-squared
df <- n - 1
ci_sd <-
  sqrt(sig^2 * df / qchisq(c(.025, .975), df = df, lower.tail = FALSE))

print("CI on mu:")
print(ci_mu)
print("CI on sd:")
print(ci_sd)
