data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int p; // number of parties
  int<lower=0> y_f[N_f, p] ; // observed vote counts
  int in_sample[N_f];
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;


  // sample
  int N; // number of stations
  int<lower=0> y[N, p]; // observed vote counts
  vector<lower=0>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates_f] x;
  real<lower=0> p_obs;

  // conf
  vector[2] beta_0_param;
  real sigma_param;
  vector[2] kappa_param;
  real sigma_coefs;
  real f_bias;
}

parameters {
  real beta_0[p];
  matrix[n_covariates_f, p] beta;
  matrix<lower=0>[n_strata_f, p] kappa;
  real<lower=0> sigma[p];
  matrix[n_strata_f,p] beta_st_raw;
}

transformed parameters {
   matrix<lower=0, upper=1>[N,p] theta;
   matrix<lower=0>[N,p] alpha_bn;
   matrix[n_strata_f,p] beta_st;
   matrix[N,p] pred;

  for(k in 1:p){
   beta_st[,k] = beta_0[k] + beta_st_raw [,k]* sigma[k];
   pred[,k] = x * beta[, k];
   theta[,k] = inv_logit(beta_st[stratum, k] + pred[,k]);
   alpha_bn[,k] = n .* theta[,k];
  }
}

model {

  beta_0 ~ normal(beta_0_param[1], beta_0_param[2]);
  for(k in 1:p){
    beta[,k] ~ normal(0 , sigma_coefs);
    beta_st_raw[,k] ~ normal(0, 1);
    kappa[,k] ~ gamma(kappa_param[1], kappa_param[2]);
  }
  sigma ~ normal(0, sigma_param);

  for(k in 1:p){
    y[,k] ~ neg_binomial_2( alpha_bn[,k] , alpha_bn[,k] ./ kappa[stratum,k]);
  }
}

generated quantities {
  int y_out[p];
  real prop_votos[p];
  real theta_f;
  real alpha_bn_f;
  real pred_f;
  real total;
  real w_bias;

 for(k in 1:p){
    w_bias = normal_rng(0, (1 - p_obs) / f_bias);
    y_out[k] = 0;
    for(i in 1:N_f){
      if(in_sample[i] == 1){
        y_out[k] += y_f[i,k];
      } else {
        pred_f = dot_product(x_f[i,], beta[,k]);
        theta_f = inv_logit(beta_st[stratum_f[i],k] + pred_f + w_bias);
        alpha_bn_f =  n_f[i] * theta_f;
        y_out[k] += neg_binomial_2_rng(alpha_bn_f , alpha_bn_f/kappa[stratum_f[i],k]);
      }
    }
  }
  total = sum(y_out);
  for(k in 1:p){
    prop_votos[k] = y_out[k] / total;
  }
}
