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
  real n[N];
  real NN[N];
  n[1] = log(10);
  for (i in 2:N) 
  {  
    n[i] = r + n[i-1] + (-exp(n[i-1])/K + e_t[i-1]);
  }
  
  NN = exp(n);
}
model {
  e_t ~ normal(0, sig_e);
  y ~ poisson(NN);
}
