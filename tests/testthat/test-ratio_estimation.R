test_tbl <- dplyr::tibble(
  state = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 4)),
  x = rnorm(29),
  cand_1 = rep(5, 29), cand_2 = rep(10, 29), otro = rep(1, 29)
) %>% dplyr::mutate(total = cand_1 + cand_2 + otro, LISTA_NOMINAL = rep(70, 29))

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
  sum_votes <-  sum(10, 5, 1)
  expect_equal(estimates$prop, 100 * c(10 / sum_votes, 5 / sum_votes, 16 / 70, 1 / sum_votes) )
  expect_equal(estimates$std_error, c(0, 0, 0, 0))
})


test_diputados_tbl <- dplyr::tibble(
  strata = c(rep("A", 10), rep("B", 10), rep("C", 5), rep("D", 4)),
  x = seq(58, 1, length = 29), x_y = rep(1, 29), y = rep(3, 29), z = rep(5:33)) |>
  dplyr::mutate(LISTA_NOMINAL = rep(70, 29)) |>
  mutate(y = ifelse(strata == "C", y + 50, y))
stratum_tbl <- dplyr::tibble(strata = c("A", "B", "C", "D"), n_strata = c(20, 70, 50, 40))
coalitions_tbl <- dplyr::tibble(
  coalition = c("x", "x_y", "x_y", "y", "z"),
  party = c("x", "x", "y", "y", "z"))

test_that("point estimates dip", {
  estimates <- bootstrap_diputados(test_diputados_tbl, stratum = strata,
                                    stratum_tbl = stratum_tbl, n_stratum = n_strata,
                                    coalitions_tbl = coalitions_tbl, B = 2, seed = 12)
  expect_equal(nrow(estimates$point_estimate$estimates_total), 4)
  expect_equal(nrow(estimates$point_estimate$estimates_strata), 12)
})

test_that("bootstrap reps dip", {
  estimates <- bootstrap_diputados(test_diputados_tbl, stratum = strata,
                                   stratum_tbl = stratum_tbl, n_stratum = n_strata,
                                   coalitions_tbl = coalitions_tbl, B = 2, seed = 12)
  expect_equal(length(estimates$bootstrap_reps), 2)
})

test_that("bootstrap majority assignment", {
  estimates <- bootstrap_diputados(test_diputados_tbl, stratum = strata,
                                   stratum_tbl = stratum_tbl, n_stratum = n_strata,
                                   coalitions_tbl = coalitions_tbl, B = 10,
                                   seed = 12, samples_table = TRUE)
  assignment_coalitions_tbl <- tibble(
    strata = c("A", "B", "C", "D")) |>
    cross_join(tibble(party = c("x", "y", "z"))) |>
    mutate(candidate = party) |>
    mutate(candidate = ifelse(strata == "A" & candidate != "z", "x", candidate)) |>
    mutate(candidate = ifelse(strata == "B" & candidate != "z", "x", candidate))
  assigment_tbl <- assign_majority(estimates$strata_tbl, assignment_coalitions_tbl,
                                   party_name = party, candidate_name = candidate)
  expect_equal(assigment_tbl$party[1:4], c("x", "x", "y", "z"))
 })
