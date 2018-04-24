data { 
  int<lower=0> N;
  real<lower=0> y[N];
} 
parameters {
  real<lower=0> mu;
  real<lower=0> sigma;
  real<lower=0, upper = 15> alpha;
} 
model {
  for (n in 1:N) 
    y[n] ~ student_t(alpha, mu, sigma);
}
