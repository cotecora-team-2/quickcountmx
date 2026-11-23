set.seed(823)
if(is.null(cmdstanr::cmdstan_default_path()))
  cmdstanr::install_cmdstan(cores = 2)

test_tbl <- dplyr::tibble(
  id_station = 1:29,
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 4)),
  ln = rep(50, 29),
  x1 = rnorm(29), x_2 = rnorm(29),
  cand_1 = rep(5, 29), cand_2 = rep(10, 29), otro = rep(1, 29)
) %>% dplyr::mutate(total = cand_1 + cand_2 + otro)

data_stratum <- test_tbl %>%
  dplyr::group_by(state) %>%
  dplyr::count()

test_that("create data", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  sample_tbl$strata <- sample_tbl$state
  test_tbl$strata <- test_tbl$state
  proc_tbl <- create_hb_data(sample_tbl, test_tbl,
                             parties = cand_1:otro,
                             covariates = x1:x_2,
                             prop_obs = 0.9)
  expect_equal(proc_tbl$N, 8)
  expect_equal(proc_tbl$x,
               sample_tbl %>% arrange(id_station) %>% ungroup() %>% select(x1, x_2) %>% as.matrix)
})


test_that("test call", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  fit <- hb_estimation(sample_tbl, stratum = state,
                                sampling_frame = test_tbl,
                                prop_obs = 0.9, seed = 12,
                                parties = cand_1:otro, covariates = x1:x_2,
                             num_iter = 100, chains = 1)
  estimates <- fit$estimates
  expect_is(estimates, "tbl")
  expect_equal(nrow(estimates), 4)
  expect_lt(mean(abs(estimates$median - c(5/16, 10/16, 1/16, 16/50))), 0.05)
})

sample_tbl <-
  select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
fit <- hb_estimation(sample_tbl, stratum = state,
                     sampling_frame = test_tbl,
                     prop_obs = 0.9, seed = 12,
                     model = "mlogit-corr",
                     parties = cand_1:otro, covariates = x1:x_2,
                     num_iter = 200, chains = 1, return_fit = TRUE)

test_that("test call corr", {
  estimates <- fit$estimates
  expect_is(estimates, "tbl")
  expect_equal(nrow(estimates), 4)
  expect_lt(mean(abs(estimates$median - c(5/16, 10/16, 1/16, 16/50))), 0.05)
})

inv_metric_test <- fit$fit$inv_metric()[[1]] |> diag()

test_that("test call inv metric", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  fit <- hb_estimation(sample_tbl, stratum = state,
                       sampling_frame = test_tbl,
                       prop_obs = 0.9, seed = 12,
                       model = "mlogit-corr",
                       parties = cand_1:otro, covariates = x1:x_2,
                       num_iter = 200, chains = 1, inv_metric = inv_metric_test)
  estimates <- fit$estimates
  expect_is(estimates, "tbl")
  expect_equal(nrow(estimates), 4)
  expect_lt(mean(abs(estimates$median - c(5/16, 10/16, 1/16, 16/50))), 0.05)
})
test_that("test call mlogit-corr-opt", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  fit_opt <- hb_estimation(sample_tbl, stratum = state,
                           id_station = id_station,
                           sampling_frame = test_tbl,
                           prop_obs = 0.9, seed = 12,
                           model = "mlogit-corr-opt",
                           parties = cand_1:otro, covariates = x1:x_2,
                           num_iter = 200, chains = 1,
                           inv_metric = inv_metric_test)
  estimates <- fit_opt$estimates
  expect_is(estimates, "tbl")
  expect_equal(nrow(estimates), 4)
  expect_equal(length(fit_opt$inv_metric), length(inv_metric_test))
  expect_lt(mean(abs(estimates$median - c(5/16, 10/16, 1/16, 16/50))), 0.05)
})

test_that("hb_estimation rejects unknown model", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  expect_error(
    hb_estimation(sample_tbl, stratum = state,
                  id_station = id_station,
                  sampling_frame = test_tbl,
                  prop_obs = 0.9, seed = 12,
                  model = "mlogit-corr-optt",
                  parties = cand_1:otro, covariates = x1:x_2,
                  num_iter = 10, chains = 1),
    "should be one of"
  )
})

test_that("mlogit-corr-opt matches mlogit-corr gradients and handles unsampled strata", {
  sample_empty <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912) |>
    dplyr::filter(state != "D")

  frame_tmp <- test_tbl |> dplyr::rename(strata = state)
  sample_tmp <- sample_empty |> dplyr::ungroup() |> dplyr::rename(strata = state)
  json_path <- system.file("stan", "prior_data_corr.json", package = "quickcountmx")
  parameters <- jsonlite::read_json(json_path, simplifyVector = TRUE)
  parameters$nominal_max <- 1000
  data_list <- create_hb_data(sample_tmp, frame_tmp,
                              parties = cand_1:otro,
                              covariates = x1:x_2,
                              nominal_max = 1000,
                              prop_obs = 0.9)
  stan_data <- c(parameters, data_list)
  stan_data$parties_name <- NULL

  mod_orig <- compile_hb_model("mlogit-corr")
  mod_opt <- compile_hb_model("mlogit-corr-opt")

  diag_orig <- mod_orig$diagnose(data = stan_data, seed = 123, init = 0.5)
  diag_opt <- mod_opt$diagnose(data = stan_data, seed = 123, init = 0.5)
  g_orig <- diag_orig$gradients()
  g_opt <- diag_opt$gradients()

  expect_equal(nrow(g_opt), nrow(g_orig))
  expect_equal(g_opt$value, g_orig$value, tolerance = 1e-12)
  expect_equal(g_opt$model, g_orig$model, tolerance = 1e-8)

  fit_empty <- hb_estimation(sample_empty, stratum = state,
                             id_station = id_station,
                             sampling_frame = test_tbl,
                             prop_obs = 0.9, seed = 12,
                             model = "mlogit-corr-opt",
                             parties = cand_1:otro, covariates = x1:x_2,
                             num_iter = 100, num_warmup = 100, chains = 1,
                             inv_metric = inv_metric_test)
  expect_is(fit_empty$estimates, "tbl")
  expect_equal(nrow(fit_empty$estimates), 4)
  expect_true(all(is.finite(fit_empty$estimates$median)))
  expect_equal(length(fit_empty$inv_metric), length(inv_metric_test))
})
