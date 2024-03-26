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

  //regions
  int n_regions_f;
  array[n_regions_f] int n_strata_reg;
  array[n_regions_f] int stratum_inicial;
  array[n_regions_f] int stratum_final;


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
  array[N_f] int<lower=0> total_f; // observed totals frame
  array[N] int<lower=0> total; // observed totals sample
  real<lower=0> total_nominal;
  matrix[N, n_covariates_f + 1] x1;
  matrix[N_f, n_covariates_f + 1] x1_f;
  vector[N] zeros;
  matrix[n_strata_f, n_covariates_f+1] mat_zeros;

  mat_zeros = rep_matrix(rep_vector(0.0, n_covariates_f + 1), n_strata_f)';
  zeros = rep_vector(0.0, N);
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
  array[n_regions_f] vector[n_covariates_f + 1] beta_0_part_prop;
  matrix[n_covariates_f + 1, n_strata_f] beta_part_prop_raw;
  array[n_regions_f] vector<lower=0>[n_covariates_f + 1] sigma_part_prop;
  //vector<lower=0>[n_strata_f] kappa_part_prop;
  array[n_regions_f] cholesky_factor_corr[n_covariates_f + 1] part_Omega_prop;
  // candidate votes parameters
  array[n_regions_f,p] vector[n_covariates_f + 1] beta_0;
  array[p] matrix[n_covariates_f + 1, n_strata_f] beta_raw;
  array[n_regions_f,p] vector<lower=0>[n_covariates_f + 1] sigma;
  array[n_regions_f,p] cholesky_factor_corr[n_covariates_f + 1] Omega;

  array[n_regions_f] row_vector<lower=0>[p] kappa_0;
  array[n_regions_f] vector<lower=0>[p] sigma_kappa;
  matrix[n_strata_f, p] kappa_st_raw;

}

transformed parameters {
   matrix[N,p] pred;
   array[N] simplex[p] theta;
   array[p] vector<lower=0>[N] alpha_bn;
   array[p] matrix[n_strata_f, n_covariates_f + 1] beta;
   matrix<lower=0>[n_strata_f, p] kappa;
   //matrix[n_strata_f, n_covariates_f + 1] beta_part;
   matrix[n_strata_f, n_covariates_f + 1] beta_part_prop;
   //vector<lower=0, upper = 1>[N] theta_part;
   vector<lower=0, upper = 1>[N] theta_part_prop;
   //vector<lower=0>[N] alpha_bn_part;
   //vector[N] pred_part;
   vector[N] pred_part_prop;

   // polling station level turnout
   for(r in 1:n_regions_f){
    beta_part_prop[stratum_inicial[r]:stratum_final[r],] = rep_matrix(beta_0_part_prop[r], n_strata_reg[r])' +
      (diag_pre_multiply(sigma_part_prop[r], part_Omega_prop[r]) * beta_part_prop_raw[, stratum_inicial[r]:stratum_final[r]])';
   }
   pred_part_prop = rows_dot_product(beta_part_prop[stratum], x1);
   theta_part_prop = inv_logit(pred_part_prop);

   // hierarchical beta coefficients for candidates
    for (k in 1:p){
      for(r in 1:n_regions_f){
        beta[k][stratum_inicial[r]:stratum_final[r],] = rep_matrix(beta_0[r,k],  n_strata_reg[r])' +
          (diag_pre_multiply(sigma[r,k], Omega[r,k]) * beta_raw[k][, stratum_inicial[r]:stratum_final[r]])';
      }
      pred[,k] = rows_dot_product(beta[k][stratum], x1);
    }



   // vote shares
   for(i in 1:N){
      theta[i] = softmax(to_vector(pred[i, ]));
   }

   // means for polling station
   for(k in 1:p){
      alpha_bn[k] = to_vector(n) .* theta_part_prop .* to_vector(theta[,k]) ;
   }

   // overdispersion over strata
   for(r in 1:n_regions_f){
    kappa[stratum_inicial[r]:stratum_final[r],] = exp(rep_matrix(kappa_0[r], n_strata_reg[r]) +
      diag_post_multiply(kappa_st_raw[stratum_inicial[r]:stratum_final[r],], sigma_kappa[r]));
   }


}

model {
  for(r in 1:n_regions_f){
    for(k in 1:(p-1)){
      beta_0[r][k][1] ~ normal(0, 3);
      to_vector(beta_0[r][k][2:(n_covariates_f + 1)]) ~ normal(0, 0.5);
    }
    to_vector(beta_0_part_prop[r]) ~ std_normal();
    sigma_part_prop[r] ~ normal(0, 1);
    part_Omega_prop[r] ~ lkj_corr_cholesky(2);
  }

  to_vector(beta_part_prop_raw) ~ std_normal();
  for(r in 1:n_regions_f){
    for(k in 1:(p-1)){
      sigma[r][k][1] ~ normal(0, 2);
      sigma[r][k][2:(n_covariates_f + 1)] ~ normal(0, 0.25);
      Omega[r][k] ~ lkj_corr_cholesky(2);
    }
    kappa_0[r] ~ normal(2, 1);
    sigma_kappa[r] ~ normal(0, 0.25);
  }
  for(k in 1:p){
    to_vector(beta_raw[k]) ~ std_normal();
    kappa_st_raw[, k] ~ std_normal();
  }


  for(k in 1:p){
    y[,k] ~ neg_binomial_2(alpha_bn[k],  alpha_bn[k] ./ kappa[stratum, k]);
  }

}

generated quantities {
  vector[p] y_out;
  array[p] real prop_votos;
  vector[p] theta_f;
  vector[p] alpha_bn_f;
  vector[p] pred_f;
  real pred_f_part_prop;
  array[N_f] real theta_f_total_prop;
  vector[p] w_bias;
  array[N_f] real total_est;
  real participacion;
  real sum_votes;


  for(i in 1:N_f){
      if(in_sample[i] == 0){
        pred_f_part_prop = dot_product(beta_part_prop[stratum_f[i],], x1_f[i,]);
        theta_f_total_prop[i] = inv_logit(pred_f_part_prop);
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
          pred_f[k] = dot_product(beta[k][stratum_f[i],], x1_f[i,]);
        }
        theta_f = softmax(to_vector(pred_f + w_bias));
        alpha_bn_f =  n_f[i] * theta_f_total_prop[i] * theta_f + 0.001;
        for(k in 1:p){
          y_out[k] += neg_binomial_2_rng(alpha_bn_f[k], alpha_bn_f[k] / kappa[stratum_f[i], k]);
        }
      }
    }
  sum_votes = sum(y_out);
  for(k in 1:p){
    prop_votos[k] = y_out[k] / sum_votes;
  }
  participacion = sum_votes / total_nominal;
}


