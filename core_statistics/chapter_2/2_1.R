############# Solutions to question 2.1 #############

mu <- mean(nhtemp)
sig <- sd(nhtemp)
ci_mu <- qnorm(c(0.025, 0.975), mu, sig)
ci_alt <- mu + sig * qnorm(c(0.025, 0.975))

#
df <- length(nhtemp) - 1
ci_sd <-
  sig *  sqrt(df / qchisq(c(.025, .975), df = df, lower.tail = FALSE))

print("CI on mu:")
print(ci_mu)
print("CI on sd:")
print(ci_sd)
