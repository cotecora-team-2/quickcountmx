set.seed(882)
test_tbl <- dplyr::tibble(
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 4)),
  x = rnorm(29),
  cand_1 = sample(4:7, 29, replace=T),
  cand_2 = sample(8:12, 29, replace=T),
  otro = sample(0:3, 29, replace=T)) |>
  dplyr::mutate(total = cand_1 + cand_2 + otro,
                ln = sample(60:90, 29, replace=T), LISTA_NOMINAL=ln)


data_stratum <- test_tbl |>
  dplyr::group_by(state) |>
  dplyr::count()

test_that("test point estimates", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.5, seed = 912)
  estimates <- normal_estimation(sample_tbl, stratum = state,
                                data_stratum = data_stratum,
                                n_stratum = n,
                                std_errors = TRUE, seed = 12, parties = cand_1:otro)
  expect_equal(estimates$prop,  c(0.623, 0.282, 0.0942), tolerance = 0.0005)
  expect_equal(estimates$std_error, c(0.0301, 0.0291, 0.0181), tolerance = 0.005)
})
