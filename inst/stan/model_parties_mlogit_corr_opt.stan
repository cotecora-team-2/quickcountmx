// model_parties_mlogit_corr_opt.stan
//
// Computation-only optimization of quickcountmx's model_parties_mlogit_corr.stan.
// Mathematically identical posterior (identical priors, bounds, unconstrained
// parameter order, inv_metric layout, and generated quantities simulation logic;
// log-density differs only by the data-only constant sum(lgamma(y + 1))).
//
// Key optimizations (see reports/stan-performance-update.qmd):
//   1. Transpose-free C x S hierarchical coefficient construction + stratum-grouped
//      dense block GEMM across all p outcomes (skipping empty strata cleanly).
//   2. Vectorized matrix-algebra softmax using fixed reference category (pred[, p] = 0)
//      and S x p inverse-kappa multiplication instead of N x p division.
//   3. Exact algebraic cancellation of neg_binomial_2(y | alpha, alpha / kappa):
//      drops alpha from likelihood, collapses y*log(kappa/(1+kappa)) to S x p
//      stratum totals Y_strata, skips y == 0, and uses exact rising-factorial
//      log identities for y in {1, 2, 3}.
//   4. 2D matrix parameter layout (column-major identical to array[p-1] vector/matrix)
//      enabling full stanc --O1 Struct-of-Arrays (var_value<Eigen::MatrixXd>)
//      promotion while avoiding Stan Math 2.40 to_vector(transpose/row_block) bugs.

data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int p; // number of parties
  array[N_f, p] int<lower=0> y_f; // vote counts
  array[N_f] int in_sample;
  vector<lower=0>[N_f] n_f; // nominal counts
  array[N_f] int ln_zero_f;
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
  int C = n_covariates_f + 1;
  int CS = C * n_strata_f;
  real<lower=0> total_nominal = 0;
  matrix[N, C] x1 = append_col(rep_vector(1.0, N), x);
  matrix[N_f, C] x1_f = append_col(rep_vector(1.0, N_f), x_f);
  vector[p - 1] ones_pm1 = rep_vector(1.0, p - 1);
  matrix[n_strata_f, C] mat_zeros = rep_matrix(0.0, n_strata_f, C);

  // Group sample stations contiguously by stratum for dense block GEMM
  // (empty strata with st_count[s] == 0 are skipped cleanly)
  array[n_strata_f] int st_count = rep_array(0, n_strata_f);
  int n_active = 0;
  for (i in 1:N) {
    st_count[stratum[i]] += 1;
  }
  for (s in 1:n_strata_f) {
    if (st_count[s] > 0) n_active += 1;
  }
  array[n_active] int act_s;
  array[n_active] int act_start;
  array[n_active] int act_len;
  array[n_strata_f] int st_cursor = rep_array(0, n_strata_f);
  {
    int pos = 1;
    int a = 0;
    for (s in 1:n_strata_f) {
      if (st_count[s] > 0) {
        a += 1;
        act_s[a] = s;
        act_start[a] = pos;
        act_len[a] = st_count[s];
        st_cursor[s] = pos;
        pos += st_count[s];
      }
    }
  }
  array[N] int perm;
  for (i in 1:N) {
    int s = stratum[i];
    perm[st_cursor[s]] = i;
    st_cursor[s] += 1;
  }
  matrix[N, C] x1_ord = x1[perm, ];
  vector[N] n_ord = n[perm];
  array[N] int stratum_ord = stratum[perm];

  // Stratum vote totals and count-regime index sets for exact NB2 simplification
  matrix[n_strata_f, p] Y_strata = rep_matrix(0.0, n_strata_f, p);
  int n_123 = 0;
  int n_23 = 0;
  int n_3 = 0;
  int n_ge4 = 0;
  for (k in 1:p) {
    for (i in 1:N) {
      int yik = y[perm[i], k];
      Y_strata[stratum_ord[i], k] += yik;
      if (yik >= 4) {
        n_ge4 += 1;
      } else if (yik >= 1) {
        n_123 += 1;
        if (yik >= 2) n_23 += 1;
        if (yik == 3) n_3 += 1;
      }
    }
  }
  vector[n_strata_f * p] Y_strata_vec = to_vector(Y_strata);

  array[n_123] int idx_123;
  array[n_23] int idx_23;
  array[n_3] int idx_3;
  array[n_ge4] int idx_ge4;
  vector[n_ge4] y_ge4;
  {
    int c123 = 0;
    int c23 = 0;
    int c3 = 0;
    int cge4 = 0;
    for (k in 1:p) {
      int col_off = (k - 1) * N;
      for (i in 1:N) {
        int yik = y[perm[i], k];
        if (yik >= 4) {
          cge4 += 1;
          idx_ge4[cge4] = col_off + i;
          y_ge4[cge4] = yik;
        } else if (yik >= 1) {
          c123 += 1;
          idx_123[c123] = col_off + i;
          if (yik >= 2) {
            c23 += 1;
            idx_23[c23] = col_off + i;
          }
          if (yik == 3) {
            c3 += 1;
            idx_3[c3] = col_off + i;
          }
        }
      }
    }
  }

  for (i in 1:N_f) {
    if (ln_zero_f[i] == 0) {
      total_nominal += n_f[i];
    }
  }
}

parameters {
  // Exact same unconstrained parameter layout & inv_metric as model_parties_mlogit_corr.stan
  vector[n_covariates_f + 1] beta_0_part_prop;
  matrix[n_covariates_f + 1, n_strata_f] beta_part_prop_raw;
  vector<lower=0>[n_covariates_f + 1] sigma_part_prop;
  cholesky_factor_corr[n_covariates_f + 1] part_Omega_prop;

  matrix[n_covariates_f + 1, p - 1] beta_0;
  matrix[n_covariates_f + 1, n_strata_f * (p - 1)] beta_raw;
  matrix<lower=0>[n_covariates_f + 1, p - 1] sigma;
  array[p - 1] cholesky_factor_corr[n_covariates_f + 1] Omega;

  row_vector<lower=0>[p] kappa_0;
  vector<lower=0>[p] sigma_kappa;
  matrix[n_strata_f, p] kappa_st_raw;
}

model {
  // Row-wise loop avoids Stan Math 2.40 --O1 to_vector(row_block) bug
  beta_0[1, ] ~ normal(0, 3);
  for (c in 2:C) {
    beta_0[c, ] ~ normal(0, 0.5);
  }
  beta_0_part_prop ~ std_normal();
  to_vector(beta_part_prop_raw) ~ std_normal();
  sigma_part_prop ~ std_normal();
  part_Omega_prop ~ lkj_corr_cholesky(2);

  to_vector(beta_raw) ~ std_normal();
  sigma[1, ] ~ normal(0, 2);
  for (c in 2:C) {
    sigma[c, ] ~ normal(0, 0.25);
  }
  for (k in 1:(p - 1)) {
    Omega[k] ~ lkj_corr_cholesky(2);
  }

  to_vector(kappa_st_raw) ~ std_normal();
  kappa_0 ~ normal(2, 1);
  sigma_kappa ~ normal(0, 0.25);

  {
    // Transpose-free C x S -> CS x p stacking (col 1..p-1 = parties, col p = turnout)
    matrix[CS, p] B_all;
    B_all[, p] = to_vector(
      rep_matrix(beta_0_part_prop, n_strata_f)
      + diag_pre_multiply(sigma_part_prop, part_Omega_prop) * beta_part_prop_raw
    );
    for (k in 1:(p - 1)) {
      B_all[, k] = to_vector(
        rep_matrix(beta_0[, k], n_strata_f)
        + diag_pre_multiply(sigma[, k], Omega[k])
          * block(beta_raw, 1, (k - 1) * n_strata_f + 1, C, n_strata_f)
      );
    }

    // Stratum-grouped dense block GEMM across all p outcomes simultaneously
    matrix[N, p] pred_all;
    for (a in 1:n_active) {
      int s = act_s[a];
      int i0 = act_start[a];
      int len = act_len[a];
      pred_all[i0:(i0 + len - 1), ] =
        block(x1_ord, i0, 1, len, C) * block(B_all, (s - 1) * C + 1, 1, C, p);
    }

    matrix[n_strata_f, p] log_kappa =
      rep_matrix(kappa_0, n_strata_f) + diag_post_multiply(kappa_st_raw, sigma_kappa);
    matrix[n_strata_f, p] inv_kappa = exp(-log_kappa);

    // Vectorized matrix-algebra softmax + phi = alpha / kappa
    matrix[N, p - 1] exp_free = exp(pred_all[, 1:(p - 1)]);
    vector[N] sum_exp = 1.0 + exp_free * ones_pm1;
    vector[N] scale = (n_ord .* inv_logit(pred_all[, p])) ./ sum_exp;
    matrix[N, p] phi_mat;
    phi_mat[, 1:(p - 1)] = diag_pre_multiply(scale, exp_free);
    phi_mat[, p] = scale;
    phi_mat = phi_mat .* inv_kappa[stratum_ord, ];

    // Exact neg_binomial_2(y | alpha, alpha / kappa) log-likelihood up to data constant
    vector[N * p] phi_vec = to_vector(phi_mat);
    matrix[n_strata_f, p] log1p_exp_lk = log1p_exp(log_kappa);
    real lp = -dot_product(phi_vec, to_vector(log1p_exp_lk[stratum_ord, ]))
              - dot_product(Y_strata_vec, to_vector(log1p_exp(-log_kappa)));
    if (n_ge4 > 0) {
      vector[n_ge4] phi_ge4 = phi_vec[idx_ge4];
      lp += sum(lgamma(y_ge4 + phi_ge4)) - sum(lgamma(phi_ge4));
    }
    if (n_123 > 0) lp += sum(log(phi_vec[idx_123]));
    if (n_23 > 0)  lp += sum(log1p(phi_vec[idx_23]));
    if (n_3 > 0)   lp += sum(log(phi_vec[idx_3] + 2.0));
    target += lp;
  }
}

generated quantities {
  vector[p] y_out;
  array[p] real prop_votos;
  real participacion;
  real sum_votes;
  real total_out;

  total_out = 0.0;
  {
    matrix[n_strata_f, C] beta_part_prop =
      (rep_matrix(beta_0_part_prop, n_strata_f)
       + diag_pre_multiply(sigma_part_prop, part_Omega_prop) * beta_part_prop_raw)';
    array[p] matrix[n_strata_f, C] beta;
    for (k in 1:(p - 1)) {
      beta[k] = (rep_matrix(beta_0[, k], n_strata_f)
                 + diag_pre_multiply(sigma[, k], Omega[k])
                   * block(beta_raw, 1, (k - 1) * n_strata_f + 1, C, n_strata_f))';
    }
    beta[p] = mat_zeros;
    matrix[n_strata_f, p] kappa =
      exp(rep_matrix(kappa_0, n_strata_f) + diag_post_multiply(kappa_st_raw, sigma_kappa));

    vector[p] theta_f;
    vector[p] alpha_bn_f;
    vector[p] pred_f;
    real pred_f_part_prop;
    array[N_f] real theta_f_total_prop;
    vector[p] w_bias;

    for (i in 1:N_f) {
      if (in_sample[i] == 0) {
        pred_f_part_prop = dot_product(beta_part_prop[stratum_f[i], ], x1_f[i, ]);
        theta_f_total_prop[i] = inv_logit(pred_f_part_prop);
      }
    }

    for (k in 1:p) {
      y_out[k] = 0.0;
      w_bias[k] = normal_rng(0, (1 - p_obs) / f_bias);
    }
    for (i in 1:N_f) {
      vector[p] temp_y_out = rep_vector(0, p);
      if (in_sample[i] == 1) {
        temp_y_out = to_vector(y_f[i, ]);
        total_out += sum(temp_y_out);
      } else {
        for (k in 1:p) {
          pred_f[k] = dot_product(beta[k][stratum_f[i], ], x1_f[i, ]);
        }
        theta_f = softmax(to_vector(pred_f + w_bias));
        alpha_bn_f = n_f[i] * theta_f_total_prop[i] * theta_f;
        for (k in 1:p) {
          temp_y_out[k] = neg_binomial_2_rng(alpha_bn_f[k], alpha_bn_f[k] / kappa[stratum_f[i], k]);
        }
        total_out = total_out + n_f[i] * theta_f_total_prop[i];
      }
      real sum_temp_y_out = sum(temp_y_out);
      if (sum_temp_y_out > n_f[i]) {
        temp_y_out = temp_y_out * (n_f[i] / sum_temp_y_out);
      }
      y_out = y_out + temp_y_out;
    }
    sum_votes = sum(y_out);
    for (k in 1:p) {
      prop_votos[k] = y_out[k] / sum_votes;
    }
    participacion = total_out / total_nominal;
  }
}
