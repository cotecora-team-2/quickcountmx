data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int p; // number of parties
  array[N_f, p] int<lower=0> y_f; // vote counts
  array[N_f] int in_sample;
  vector<lower=0>[N_f] n_f; // nominal counts
  int<lower=0> nominal_max;
  array[N_f] int stratum_f;
  matrix[N_f, n_covariates_f] x_f;

  // sample
  int N; // number of stations
  array[N, p] int<lower=0> y; // observed vote counts
  vector<lower=0>[N] n; // nominal counts
  array[N] int stratum;
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
  array[N_f] int<lower=0> total_f; // observed totals
  array[N] int<lower=0> total; // observed totals
  real<lower=0> total_nominal;
  matrix[N, n_covariates_f + 1] x1;
  matrix[N_f, n_covariates_f + 1] x1_f;
  int<lower=0> num_outlier = 0;
  real epsilon = 1e-8;

  for(i in 1:N_f){
    total_f[i] = sum(y_f[i, ]);
  }
  for(i in 1:N){
    if(y[i,3] > 50){
      num_outlier += 1;
    }
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
  array[p] vector[n_covariates_f + 1] beta_0;
  array[p] matrix[n_covariates_f + 1, n_strata_f] beta_raw;
  array[p] vector<lower=0>[n_covariates_f + 1] sigma;
  array[p] cholesky_factor_corr[n_covariates_f + 1] Omega;

  row_vector<lower=0>[p] kappa_0;
  vector<lower=0>[p] sigma_kappa;
  matrix[n_strata_f, p] kappa_st_raw;

  real<lower=0, upper =1> prob_outlier;
}

transformed parameters {
   matrix[N,p] pred;
   array[N] simplex[p] theta;
   array[p] vector<lower=0>[N] alpha_bn;
   array[p] matrix[n_strata_f, n_covariates_f + 1] beta;
   matrix<lower=0>[n_strata_f, p] kappa;
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
      alpha_bn[k] = n .* theta_part .* to_vector(theta[,k]) ;
   }

   // overdispersion over strata
   kappa = exp(rep_matrix(kappa_0, n_strata_f) + diag_post_multiply(kappa_st_raw, sigma_kappa));



}

model {
  for(k in 1:p){
      to_vector(beta_0[k]) ~ normal(0, 2);
  }
  to_vector(beta_0_part) ~ normal(0, 1);
  to_vector(beta_part_raw) ~ std_normal();
  for(k in 1:p){
    to_vector(beta_raw[k]) ~ std_normal();
    //sigma[k] ~ normal(0, sigma_param);
    sigma[k][1] ~ normal(0, 3);
    sigma[k][2:(n_covariates_f + 1)] ~ normal(0, 1);
    Omega[k] ~ lkj_corr_cholesky(2);
    kappa_st_raw[, k] ~ std_normal();
  }
  to_vector(beta_part_raw) ~ std_normal();
  kappa_0 ~ normal(2, 1);

  sigma_kappa ~ normal(0, 0.05);
  //sigma_part ~ normal(0, sigma_param);
  sigma_part[1] ~ normal(0, 1);
  sigma_part[2:(n_covariates_f + 1)] ~ normal(0, 1);
  part_Omega ~ lkj_corr_cholesky(2);

  for(k in 1:p){
    y[,k] ~ neg_binomial_2( alpha_bn[k], epsilon + alpha_bn[k] ./ (kappa[stratum, k]));
  }

  prob_outlier ~ beta(5, 1000);
  //num_outlier ~ binomial(N, prob_outlier);
}

generated quantities {
  vector[p] y_out;
  array[p] real prop_votos;
  vector[p] theta_f;
  real alpha_bn_f_part;
  vector[p] alpha_bn_f;
  vector[p] pred_f;
  real pred_f_part;
  array[N_f] real theta_f_total;
  vector[p] w_bias;
  real participacion;
  real suma;
  vector[p] outlier_station;
  // total

  for(i in 1:N_f){
      if(in_sample[i] == 1){
        theta_f_total[i] = 0;
      } else {
        pred_f_part = dot_product(beta_part[stratum_f[i],], x1_f[i,]);
        theta_f_total[i] = inv_logit(pred_f_part);
      }
    }

// party vote

    for(k in 1:p){
      y_out[k] = 0.0;
      w_bias[k] = normal_rng(0, (1 - p_obs*0.7) / f_bias);
    }
    for(i in 1:N_f){
      if(in_sample[i] == 1){
        y_out = y_out + to_vector(y_f[i,]);
      } else {
        for(k in 1:p){
          outlier_station[k] = 0;
          if(bernoulli_rng(prob_outlier)==1){
            outlier_station[3] = uniform_rng(0, 5);
          }
        }
        for(k in 1:p){
          pred_f[k] = dot_product(beta[k][stratum_f[i],], x1_f[i,]);
        }
        theta_f = softmax(to_vector(pred_f + w_bias + outlier_station));
        alpha_bn_f =  n_f[i] * theta_f_total[i] * theta_f ;
        for(k in 1:p){
          y_out[k] += neg_binomial_2_rng(alpha_bn_f[k], epsilon +alpha_bn_f[k] / kappa[stratum_f[i], k]);
        }
      }
    }
  suma = sum(y_out);
  for(k in 1:p){
    prop_votos[k] = y_out[k] / suma;
  }
  participacion = suma / total_nominal;
}


