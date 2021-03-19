set.seed(823)
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
  expect_equal(proc_tbl$N, 9)
  expect_equal(proc_tbl$x,
               sample_tbl %>% arrange(id_station) %>% select(x1, x_2) %>% as.matrix)
})


test_that("test call", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.3, seed = 912)
  estimates <- hb_estimation(sample_tbl, stratum = state,
                                data_stratum = test_tbl,
                                prop_obs = 0.9, seed = 12,
                                parties = cand_1:otro, covariates = x1:x_2,
                             num_iter = 100, chains = 1)
  expect_is(estimates, "tbl")
  expect_equal(nrow(estimates), 3)
  expect_lt(mean(abs(estimates$median - c(5/16, 10/16, 1/16))), 0.05)
})
