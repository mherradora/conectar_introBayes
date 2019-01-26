// Modelo Poison con efecto jerarquico en tasa por departamento
data {
  int<lower=1> n; // cantidad de datos 
  int y[n];  // Defunciones observados
  vector[n] N;  // Nacimientos observados 
  int<lower=1> G; // Grupos: los departamentos de Uruguay
  int<lower=1> gr[n]; // variable que indica el depto
  //real mu0;
}
parameters {
  vector<lower=0>[G] tmi; 
  real<lower=0> alpha;
  real<lower=0> beta;
}
transformed parameters {
  real<lower=0> mu;
  mu = alpha/beta;
}
model {
    vector[n] lam; 
    lam = N .*tmi[gr]/1000;
    
    // previas
    alpha ~ exponential(1);
    beta ~ exponential(1);
    tmi ~ gamma(alpha, beta);
    
    // data model
    y ~ poisson( lam ); 
}
generated quantities {
  // Predictiva posterior para el departamento de Flores, 
  // en un año con N = 350 nacimientos 
  int ytilde_flores; 
  ytilde_flores = poisson_rng( 350*tmi[1]/1000 );
}
