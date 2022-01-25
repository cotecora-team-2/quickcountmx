data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int p; // number of parties
  int<lower=0> y_f[N_f, p] ; // observed vote counts
  int in_sample[N_f];
  vector<lower=0>[N_f] n_f; // nominal counts
  int<lower=0> nominal_max;
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

transformed data {
  int<lower=0> total_f[N_f] ; // observed totals
  int<lower=0> total[N] ; // observed totals
  real<lower=0> total_nominal;

  for(i in 1:N_f){
    total_f[i] = sum(y_f[i, ]);
  }
  for(i in 1:N){
    total[i] = sum(y[i, ]);
  }
  total_nominal = 0;
  for(i in 1:N_f){
    if(n_f[i] < nominal_max){
      total_nominal += n_f[i];
    }
  }

}

parameters {
  row_vector[p] beta_0;
  real beta_0_part;
  matrix[n_covariates_f, p] beta;
  vector[n_covariates_f] beta_part;
  row_vector<lower=0>[p] kappa_0;
  vector<lower=0>[p] sigma;
  vector<lower=0>[p] sigma_kappa;
  real<lower=0> sigma_part;
  matrix[n_strata_f, p] beta_st_raw;
  matrix[n_strata_f, p] kappa_st_raw;
  vector[n_strata_f] beta_st_part_raw;
  vector<lower=0>[n_strata_f] kappa_part;

}

transformed parameters {
   matrix[N,p] pred;
   vector[p] theta[N];
   vector[N] alpha_bn[p];
   matrix[n_strata_f,p] beta_st;
   vector[N] theta_part;
   vector[N] alpha_bn_part;
   vector[n_strata_f] beta_st_part;
   vector[N] pred_part;
   matrix<lower=0>[n_strata_f, p] kappa;

   kappa = exp(rep_matrix(kappa_0, n_strata_f) + diag_post_multiply(kappa_st_raw, sigma_kappa));

   beta_st_part = beta_0_part + beta_st_part_raw * sigma_part;
   pred_part = sigma_coefs * (x * beta_part);
   theta_part = inv_logit(beta_st_part[stratum] + pred_part);
   alpha_bn_part = n .* theta_part;

    beta_st = rep_matrix(beta_0, n_strata_f) + diag_post_multiply(beta_st_raw, sigma);

    pred = sigma_coefs * (x * beta);

    for(i in 1:N){
      theta[i] = softmax(to_vector(beta_st[stratum[i], ] + pred[i,]));
    }
    for(k in 1:p){
      alpha_bn[k] = (n .* theta_part) .* to_vector(theta[,k]) ;
    }




}

model {

  beta_0 ~ normal(beta_0_param[1], beta_0_param[2]);
  beta_0_part ~ normal(beta_0_param[1], beta_0_param[2]);

  for(k in 1:p){
    beta[,k] ~ std_normal();
    beta_st_raw[,k] ~ std_normal();
    kappa_st_raw[, k] ~ std_normal();
  }
  beta_part ~ std_normal();
  beta_st_part_raw ~ std_normal();

  kappa_0 ~ normal(2, 1);

  //for(k in 1:p){
  //    kappa[,k] ~ gamma(kappa_param[1], kappa_param[2]);
  //}
  kappa_part ~ gamma(kappa_param[1], kappa_param[2]);

  sigma ~ normal(0, sigma_param);
  sigma_kappa ~ normal(0, 0.25);
  sigma_part ~ normal(0, sigma_param);


  for(k in 1:p){
    y[,k] ~ neg_binomial_2( alpha_bn[k], alpha_bn[k] ./ to_vector(kappa[stratum, k]));
  }
  total ~ neg_binomial_2( alpha_bn_part, alpha_bn_part  ./ kappa_part[stratum]);
}

generated quantities {
  vector[p] y_out;
  real prop_votos[p];
  vector[p] theta_f;
  real alpha_bn_f_part;
  vector[p] alpha_bn_f;
  vector[p] pred_f;
  real pred_f_part;
  real theta_f_total[N_f];
  real total_cnt;
  vector[p] w_bias;
  real participacion;
  real total_est[N_f];

  // total
  total_cnt = 0;

  for(i in 1:N_f){
      if(in_sample[i] == 1){
        total_est[i] = total_f[i];
        total_cnt += total_f[i];
      } else {
        pred_f_part = sigma_coefs * dot_product(x_f[i,], beta_part);
        theta_f_total[i] = inv_logit(beta_st_part[stratum_f[i]] + pred_f_part);
        alpha_bn_f_part =  n_f[i] * theta_f_total[i];
        total_est[i] = neg_binomial_2_rng(alpha_bn_f_part , alpha_bn_f_part/kappa_part[stratum_f[i]]);
        total_cnt += total_est[i];
      }
    }

// party vote

    for(k in 1:p){
      y_out[k] = 0.0;
      w_bias[k] = normal_rng(0, (1 - p_obs) / f_bias);
    }
    for(i in 1:N_f){
      if(in_sample[i] == 1){
        y_out = y_out + to_vector(y_f[i,]);
      } else {
        pred_f = sigma_coefs * to_vector(x_f[i,] * beta);
        theta_f = softmax(to_vector(beta_st[stratum_f[i],]) + pred_f + w_bias);
        alpha_bn_f =  n_f[i] * theta_f * theta_f_total[i];

        for(k in 1:p){
          y_out[k] += neg_binomial_2_rng(alpha_bn_f[k], alpha_bn_f[k] / kappa[stratum_f[i], k]);
        }
      }
    }
  for(k in 1:p){
    prop_votos[k] = y_out[k] / total_cnt;
  }
  participacion = total_cnt / total_nominal;
}


