############# Solutions to question 2.1 #############

mu <- mean(nhtemp)
sig <- sd(nhtemp)
ci_mu <- qnorm(c(0.025, 0.975), mu, sig)
ci_alt <- mu + sig * qnorm(c(0.025, 0.975))

ci_sd <- c(sqrt(((length(
  nhtemp
) - 1) * sd(nhtemp) ^ 2) / qchisq(
  c(.025), df = length(nhtemp) - 1, lower.tail = FALSE
)),
sqrt(((length(
  nhtemp
) - 1) * sd(nhtemp) ^ 2) / qchisq(
  c(.975), df = length(nhtemp) - 1, lower.tail = FALSE
)))

print("CI on mu:")
print(ci_mu)
print("CI on sd:")
print(ci_sd)