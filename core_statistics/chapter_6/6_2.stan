data { 
  int<lower=0> N;
  real<lower=0> y[N];
} 
parameters {
  real<lower=0> mu;
  real<lower=0> sigma;
  real<lower=0> alpha;
} 
model {
  alpha ~ gamma(1,0.05);
  for (n in 1:N) 
    y[n] ~ student_t(alpha, mu, sigma);
}
generated quantities{
  real y_pred;
  y_pred = student_t_rng(alpha, mu, sigma);
}
