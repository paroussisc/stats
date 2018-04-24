data { 
  int<lower=0> N;
  int<lower=0> t[N];
  real<lower=0> y[N];
} 
parameters {
  real<lower=0> mu;
  real<lower=0> sigma;
  real<lower=0> alpha;
  real beta;
} 
model {
  alpha ~ gamma(1,0.05);
  for (n in 1:N) 
    y[n] ~ student_t(alpha, mu + t[n] * beta, sigma);
}
generated quantities{
  real y_pred[N];
  for (n in 1:N)
    y_pred[n] = student_t_rng(alpha, mu + t[n] * beta, sigma);
}
