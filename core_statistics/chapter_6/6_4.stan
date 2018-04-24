data { 
  int<lower=0> al_n;
  real allo[al_n];
  int<lower=0> al_n_c;
  real allo_c[al_n_c];
  int<lower=0> au_n;
  real aut[au_n];
  int<lower=0> au_n_c;
  real aut_c[au_n_c];
} 
parameters {
  real<lower=0> theta_l;
  real<lower=0> theta_u;
} 
model {
  
  for (n in 1:al_n) 
    target += log(theta_l * exp(-theta_l * allo[n]));
  
  for (n in 1:al_n_c) 
    target += (-theta_l * allo_c[n]);
    
  for (n in 1:au_n)
    target += log(theta_u * exp(-theta_u * aut[n]));
  
  for (n in 1:au_n_c) 
    target += (-theta_u * aut_c[n]);
}
