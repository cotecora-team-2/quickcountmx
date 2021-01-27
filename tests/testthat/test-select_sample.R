test_tbl <- dplyr::tibble(
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 2)),
  x = rnorm(27)
)

test_that("test fractions", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.5, seed = 9912) %>%
    dplyr::count(state) %>%
    dplyr::arrange(state)
  expect_equal(sample_tbl$n, c(5, 5, 2, 1))
  expect_length(sample_tbl, 2)
})
