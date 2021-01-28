test_tbl <- dplyr::tibble(
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 4)),
  x = rnorm(29),
  cand_1 = rep(5, 29), cand_2 = rep(10, 29), otro = rep(1, 29)
) %>% dplyr::mutate(total = cand_1 + cand_2 + otro)

data_stratum <- test_tbl %>%
  dplyr::group_by(state) %>%
  dplyr::count()

test_that("test point estimates", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.5, seed = 912)
  estimates <- ratio_estimation(sample_tbl, stratum = state,
                                data_stratum = data_stratum,
                                n_stratum = n,
                                std_errors = TRUE, seed = 12, parties = cand_1:otro)
  expect_equal(estimates$prop, 100 * c(10, 5, 1) / sum(10, 5, 1))
  expect_equal(estimates$std_error, c(0, 0, 0))
})
