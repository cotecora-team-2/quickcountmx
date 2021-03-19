data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int<lower=0> y_f[N_f] ; // observed vote counts
  int in_sample[N_f];
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;

  // sample
  int N; // number of stations
  int<lower=0> y[N]; // observed vote counts
  vector<lower=0>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates_f] x;

  // conf
  vector[2] beta_0_param;
  real sigma_param;
  vector[2] kappa_param;
  real sigma_coefs;

}

parameters {
  real beta_0;
  vector[n_covariates_f] beta;
  vector<lower=0>[n_strata_f] kappa;
  real<lower=0> sigma;
  vector[n_strata_f] beta_st_raw;
}

transformed parameters {
   vector<lower=0, upper=1>[N] theta;
   vector<lower=0>[N] alpha_bn;
   vector[n_strata_f] beta_st;
   vector[N] pred;

  beta_st = beta_0 + beta_st_raw * sigma;
  pred = x * beta;
  theta = inv_logit(beta_st[stratum] + pred);
  alpha_bn = n .* theta;


}

model {

  beta_0 ~ normal(beta_0_param[1], beta_0_param[2]);
  beta ~ normal(0 , sigma_coefs);
  beta_st_raw ~ normal(0, 1);
  sigma ~ normal(0, sigma_param);
  kappa ~ gamma(kappa_param[1], kappa_param[2]);

  y ~ neg_binomial_2( alpha_bn , alpha_bn ./ kappa[stratum]);

}

generated quantities {
  int y_out;
  real theta_f;
  real alpha_bn_f;
  real pred_f;

  y_out = 0;
  for(i in 1:N_f){
    if(in_sample[i] == 1){
      y_out += y_f[i];
    } else {
      pred_f = dot_product(x_f[i,], beta);
      theta_f = inv_logit(beta_st[stratum_f[i]] + pred_f);
      alpha_bn_f =  n_f[i] * theta_f;
      y_out += neg_binomial_2_rng(alpha_bn_f , alpha_bn_f/kappa[stratum_f[i]]);
    }
  }
}
