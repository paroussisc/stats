data { 
  int<lower=0> N;
  real waiting[N];
  real duration[N];
} 
parameters {
  real<lower=0, upper=1> phi_w;
  real<lower=0, upper=1> phi_d;
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
  phi_w ~ normal(0.3,0.1);
  mu_w1 ~ normal(85,1);
  sig_w1 ~ normal(0.3,0.1);
  mu_w2 ~ normal(55,1);
  sig_w2 ~ normal(0.3,0.1);
  mu_d1 ~ normal(4,1);
  sig_d1 ~ normal(0.3,0.1);
  mu_d2 ~ normal(2,1);
  sig_d2 ~ normal(0.3,0.1);
  for (n in 1:N)
  {
    target += log(phi_w / sqrt(2* pi() * sig_w1^2) * exp(-0.5 * ((waiting[n] - mu_w1)/sig_w1)^2)
                * (phi_d * (1/sqrt(2* pi() * sig_d1^2)) * exp(-0.5 * ((duration[n] - mu_d1)/sig_d1)^2) 
                + (1 - phi_d) * (1/sqrt(2* pi() * sig_d2^2)) * exp(-0.5 * ((duration[n] - mu_d2)/sig_d2)^2))
            + (1-phi_w) / sqrt(2* pi() * sig_w2^2) * exp(-0.5 * ((waiting[n] - mu_w2)/sig_w2)^2)
            * (1/sqrt(2* pi() * sig_d2^2)) * exp(-0.5 * ((duration[n] - mu_d2)/sig_d2)^2));
  }
}
generated quantities{
  real w_pred;
  real d_pred;
  real pw;
  real pd;
  pw = bernoulli_rng(phi_w);
  pd = bernoulli_rng(phi_d);
  w_pred = pw * normal_rng(mu_w1, sig_w1) + (1 - pw) * normal_rng(mu_w2, sig_w2);
  
  // pw * N1 + (1-pw)*( pd * N1 + (1 - pd) * N2)
  d_pred = pw * (pd * normal_rng(mu_d1, sig_d1) + (1 - pd) * normal_rng(mu_d2, sig_d2)) + 
          (1 - pw) * normal_rng(mu_d2, sig_d2);
}





