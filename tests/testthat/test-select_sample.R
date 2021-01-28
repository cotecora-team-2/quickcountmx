test_tbl <- dplyr::tibble(
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 2)),
  x = rnorm(27)
)
allocation_tbl <- dplyr::tibble(
  state = LETTERS[1:4],
  n_state = c(3, 5, 2, 2)
)

allocation_frac_tbl <- dplyr::tibble(
  state = LETTERS[1:4],
  n_state = c(0.3, 0.5, 0.2, 1.0)
)

test_that("test prop sampling", {
  sample_tbl <-
    select_sample_prop(test_tbl, stratum = state, frac = 0.5, seed = 9912) %>%
    dplyr::count(state) %>%
    dplyr::arrange(state)
  expect_equal(sample_tbl$n, c(5, 5, 2, 1))
  expect_length(sample_tbl, 2)
})

test_that("allocation sampling ", {
  # check with sample size
  sample_tbl <-
    select_sample_str(test_tbl, allocation_tbl, sample_size = n_state,
                      stratum = state, seed = 9100)
  counts_tbl <- sample_tbl %>%
    dplyr::count(state) %>%
    dplyr::arrange(state)

  # check with sample proportion
  sample_frac_tbl <-
    select_sample_str(test_tbl, allocation_frac_tbl,
                      is_frac = TRUE,
                      sample_size = n_state,
                      stratum = state, seed = 9100)
  counts_frac_tbl <- sample_frac_tbl %>%
    dplyr::count(state) %>%
    dplyr::arrange(state)

  expect_equal(counts_tbl$n, allocation_tbl$n_state)
  expect_equal(counts_frac_tbl$n, c(3, 5, 1, 2))
  expect_equal(colnames(sample_tbl), colnames(test_tbl))
  expect_equal(colnames(sample_frac_tbl), colnames(test_tbl))
})
