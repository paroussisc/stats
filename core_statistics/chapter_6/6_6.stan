data { 
  int<lower=0> N;
  real waiting[N];
} 
parameters {
  real<lower=0, upper=1> phi;
  real<lower=0> mu1;
  real<lower=0> sig1;
  real<lower=0> mu2;
  real<lower=0> sig2;
} 
model {
  // weak priors
  phi ~ normal(0.3,0.1);
  mu1 ~ normal(55,5);
  sig1 ~ normal(0.3,1);
  mu2 ~ normal(85,5);
  sig2 ~ normal(0.3,1);
  for (n in 1:N)
  {
    target += log(phi / sqrt(2* pi() * sig1^2) * exp(-0.5 * ((waiting[n] - mu1)/sig1)^2)
            + (1-phi) / sqrt(2* pi() * sig2^2) * exp(-0.5 * ((waiting[n] - mu2)/sig2)^2));
  }
}
