data {
  int n_estratos;
  int p; // coaliciones
  int n; // tamaño de la muestra
  array[n,p] int votos;
  array[n] int estrato;
}

parameters {
  //matrix[p, n_estratos] alpha;
  array[n_estratos] vector<lower=0>[p] alpha;
  corr_matrix[J] Omega;
  vector<lower=0>[p] sigma;
}

transformed parameters {
  cov_matrix[P] Sigma;

  Sigma = quad_form_diag(Omega, sigma); 
}

model {
  for(i in 1:n){
    votos[i,] ~ dirichlet_multinomial(alpha[estrato[i]]);
  }
  for(s in 1:n_estratos){
    alpha[s] ~ exponential(exp(log_lambda[s));
    log_lambda[s] ~ multi_normal(log_lambda_0, Sigma);
  }
  sigma ~ normal(0, 0.5);
  Omega ~ lkj_corr(1);
  log_lambda_0 ~ normal(0, 1);
}
