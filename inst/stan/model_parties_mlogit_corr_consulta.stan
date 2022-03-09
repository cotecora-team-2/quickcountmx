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
  matrix[N, n_covariates_f + 1] x1;
  matrix[N_f, n_covariates_f + 1] x1_f;

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
  x1 = append_col(rep_vector(1.0, N), x);
  x1_f = append_col(rep_vector(1.0, N_f), x_f);
}

parameters {
  // participation parameters
  vector[n_covariates_f + 1] beta_0_part;
  matrix[n_covariates_f + 1, n_strata_f] beta_part_raw;
  vector<lower=0>[n_covariates_f + 1] sigma_part;
  cholesky_factor_corr[n_covariates_f + 1] part_Omega;
  // candidate votes parameters
  vector[n_covariates_f + 1] beta_0[p];
  matrix[n_covariates_f + 1, n_strata_f] beta_raw[p];
  vector<lower=0>[n_covariates_f + 1] sigma[p];
  cholesky_factor_corr[n_covariates_f + 1] Omega[p];

  row_vector<lower=0>[p+1] kappa_0;
  vector<lower=0>[p+1] sigma_kappa;
  matrix[n_strata_f, p+1] kappa_st_raw;
  real<lower=0, upper=1> prob_outlier;

}

transformed parameters {
   matrix[N,p] pred;
   simplex[p] theta[N];
   vector<lower=0>[N] alpha_bn[p];
   matrix[n_strata_f, n_covariates_f + 1] beta[p];
   matrix<lower=0>[n_strata_f, p+1] kappa;
   matrix[n_strata_f, n_covariates_f + 1] beta_part;
   vector<lower=0, upper = 1>[N] theta_part;
   vector<lower=0>[N] alpha_bn_part;
   vector[N] pred_part;

   // hierarchical beta coefficients for participation
   beta_part = rep_matrix(beta_0_part, n_strata_f)' + (diag_pre_multiply(sigma_part, part_Omega) * beta_part_raw)';
   pred_part = rows_dot_product(beta_part[stratum], x1);
   theta_part = inv_logit(pred_part);
   alpha_bn_part = n .* theta_part;

   // hierarchical beta coefficients for candidates
   for (k in 1:p){
    beta[k] = rep_matrix(beta_0[k], n_strata_f)' + (diag_pre_multiply(sigma[k], Omega[k]) * beta_raw[k])';
    pred[,k] = rows_dot_product((beta[k])[stratum], x1);
   }

   for(i in 1:N){
      theta[i] = softmax(to_vector(pred[i, ]));
   }
   for(k in 1:p){
      alpha_bn[k] = to_vector(total) .* to_vector(theta[,k]) ;
   }

   // overdispersion over strata
   kappa = exp(rep_matrix(kappa_0, n_strata_f) + diag_post_multiply(kappa_st_raw, sigma_kappa));



}

model {
  for(k in 1:p){
      beta_0[k][1] ~  normal(0, 1);
      to_vector(beta_0[k][2:(n_covariates_f + 1)]) ~ normal(0, 1);
  }
  to_vector(beta_0_part) ~ normal(0, 1);
  to_vector(beta_part_raw) ~ std_normal();
  for(k in 1:p){
    to_vector(beta_raw[k]) ~ std_normal();
    sigma[k][1] ~ normal(0, 1);
    sigma[k][2:(n_covariates_f + 1)] ~ normal(0, 1);
    Omega[k] ~ lkj_corr_cholesky(2);
    kappa_st_raw[, k] ~ std_normal();
  }
  to_vector(beta_part_raw) ~ std_normal();
  kappa_0 ~ normal(3, 0.5);

  sigma_kappa ~ normal(0, 0.25);
  //sigma_part ~ normal(0, sigma_param);
  sigma_part[1] ~ normal(0, 1);
  sigma_part[2:(n_covariates_f + 1)] ~ normal(0, 1);
  part_Omega ~ lkj_corr_cholesky(2);

  for(i in 1:N)
    if(total[i] > 0)
      for(k in 1:p)
        y[i,k] ~ neg_binomial_2(alpha_bn[k][i], alpha_bn[k][i] ./ kappa[stratum[i], k]);


  total ~ neg_binomial_2(alpha_bn_part, alpha_bn_part ./ kappa[stratum, p+1]);

  prob_outlier ~ beta(200, 50000);
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
  vector[p] outlier_station;
  real participacion;
  real total_est[N_f];
  real suma;

  // total
  total_cnt = 0;

  for(i in 1:N_f){
      if(in_sample[i] == 1){
        total_est[i] = total_f[i];
        total_cnt += total_f[i];
      } else {
        pred_f_part = dot_product(beta_part[stratum_f[i],], x1_f[i,]);
        theta_f_total[i] = inv_logit(pred_f_part);
        alpha_bn_f_part =  n_f[i] * theta_f_total[i];
        //total_est[i] = neg_binomial_2_rng(alpha_bn_f_part , alpha_bn_f_part / kappa_part[stratum_f[i]]);
        total_est[i] = neg_binomial_2_rng(alpha_bn_f_part , alpha_bn_f_part / kappa[stratum_f[i], p+1]);
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
        for(k in 1:p){
          outlier_station[k] = 0;
        }
        if(bernoulli_rng(prob_outlier)==1){
            outlier_station[3] = uniform_rng(0, 5);
          }
        for(k in 1:p){
          pred_f[k] = dot_product(beta[k][stratum_f[i],], x1_f[i,]);

        }
        theta_f = softmax(to_vector(pred_f + w_bias + outlier_station));
        alpha_bn_f =  n_f[i] * theta_f * theta_f_total[i];
        for(k in 1:p){
          y_out[k] += neg_binomial_2_rng(alpha_bn_f[k], alpha_bn_f[k] / kappa[stratum_f[i], k]);
        }
      }
    }
  suma = sum(y_out);
  for(k in 1:p){
    prop_votos[k] = y_out[k] / suma;
  }
  participacion = total_cnt / total_nominal;
}


