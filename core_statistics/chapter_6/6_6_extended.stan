data { 
  int<lower=0> N;
  real waiting[N];
  real duration[N];
} 
parameters {
  real<lower=0, upper=1> phi;
  real<lower=0> mu_w1;
  real<lower=0> sig_w1;
  real<lower=0> mu_w2;
  real<lower=0> sig_w2;
  real<lower=0> mu_d1;
  real<lower=0> sig_d1;
  real<lower=0> mu_d2;
  real<lower=0> sig_d2;
} 
model {
  // weak priors
  phi ~ normal(0.3,0.1);
  mu_w1 ~ normal(55,1);
  sig_w1 ~ normal(0.3,0.1);
  mu_w2 ~ normal(85,1);
  sig_w2 ~ normal(0.3,0.1);
  mu_d1 ~ normal(3,1);
  sig_d1 ~ normal(0.3,0.1);
  mu_d2 ~ normal(3,1);
  sig_d2 ~ normal(0.3,0.1);
  for (n in 1:N)
  {
    target += log(phi / sqrt(2* pi() * sig_w1^2) * exp(-0.5 * ((waiting[n] - mu_w1)/sig_w1)^2)
                * (1/sqrt(2* pi() * sig_d1^2)) * exp(-0.5 * ((duration[n] - mu_d1)/sig_d1)^2)
            + (1-phi) / sqrt(2* pi() * sig_w2^2) * exp(-0.5 * ((waiting[n] - mu_w2)/sig_w2)^2)
            * (1/sqrt(2* pi() * sig_d2^2)) * exp(-0.5 * ((duration[n] - mu_d2)/sig_d2)^2));
  }
}
generated quantities{
  real w_pred;
  real d_pred;
  real p;
  p = bernoulli_rng(phi);
  w_pred = p * normal_rng(mu_w1, sig_w1) + (1- p) * normal_rng(mu_w2, sig_w2);
  d_pred = p * normal_rng(mu_d1, sig_d1) + (1- p) * normal_rng(mu_d2, sig_d2);
}





