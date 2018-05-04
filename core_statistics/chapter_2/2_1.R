############# Solutions to question 2.1 #############
n <- length(nhtemp)
mu <- mean(nhtemp)
sig <- sd(nhtemp)
st_err <- sig/sqrt(n)
ci_mu <- qnorm(c(0.025, 0.975), mu, st_err)
ci_alt <- mu + st_err * qnorm(c(0.025, 0.975))

#
df <- n - 1
ci_sd <-
  sig *  sqrt(df / qchisq(c(.025, .975), df = df, lower.tail = FALSE))

print("CI on mu:")
print(ci_mu)
print("CI on sd:")
print(ci_sd)
