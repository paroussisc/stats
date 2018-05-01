data { 
  int<lower=0> N;
  int y[N];
} 
parameters {
  vector[N] e_t;
  real<lower=0> K;
  real<lower=0> r;
  real<lower=0> sig_e;
} 
transformed parameters {
  real log_n[N];
  real N_t[N];
  log_n[1] = log(10);
  for (i in 2:N) 
  {  
    log_n[i] = r + log_n[i-1] + (-exp(log_n[i-1])/K + e_t[i-1]);
  }
  
  N_t = exp(log_n);
}
model {
  e_t ~ normal(0, sig_e);
  y ~ poisson(N_t);
}
