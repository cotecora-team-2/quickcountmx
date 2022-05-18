#' Normal model estimator to compute proportion of votes allocated to each party
#'
#' Compute estimator for each candidate, standard errors are computed
#' with parametric boostrap within each stratum and computing the standard
#' error of the samples (no corrections).
#' @param data_tbl \code{tibble}
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param data_stratum Data frame with stratum variable (named exactly as in
#'   \code{data}) and number of polling stations per strata.
#' @param n_stratum Unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param parties Unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param std_errors Logical value indicating whether to compute standard errors
#'  (using bootstrap), defaults to TRUE.
#' @param B Number of parametric bootstrap replicates used to compute standard errors,
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional). It will only be used when computing standard errors.
#' @return A \code{tibble} including the ratio estimation for each party
#'   and standard errors (if requested).
#' @examples
#' # count number of polling stations per stratum
#' library(dplyr)
#' conteo_2018 <- conteo_2018 %>%
#'     rename(ln = LISTA_NOMINAL_CASILLA) %>%
#'     mutate(ln = ifelse(ln == 0, 1200, ln))
#' stratum_sizes <- conteo_2018 %>%
#'     group_by(ID_DISTRITO) %>%
#'     summarise(n_stratum = n())
#' # stratified random sample (size 6%), sample size proportional to strata size
#' sample <- select_sample_prop(conteo_2018, stratum = ID_DISTRITO, 0.06)
#' normal_estimation(sample, stratum = ID_DISTRITO,
#'   data_stratum = stratum_sizes, n_stratum = n_stratum, any_of(c("AMLO", "JAMK")))
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @export
normal_estimation <- function(data_tbl, stratum, data_stratum, n_stratum, parties,
                             std_errors = TRUE, B = 50, seed = NA){

  party_names <- names(data_tbl |> select( {{ parties}}))
  data_stratum <- data_stratum %>%
    rename(strata = {{ stratum }}, n_strata = {{ n_stratum }})

  # calculate estimates
  data_tbl <- data_tbl %>%
    ungroup() %>%
    rename(strata = {{ stratum }})

  # collapse strata if needed
  if(n_distinct(data_tbl$strata) < n_distinct(data_stratum$strata)) {
    data_stratum_collapsed <- collapse_strata(data_tbl, data_stratum)
  } else {
    data_stratum_collapsed <- data_stratum
  }
  # filter strata with only one point
  count_tbl <- data_tbl |> count(strata) |> filter(n <= 1) |>
    select(strata)
  if(nrow(count_tbl) > 0){
    data_tbl <- anti_join(data_tbl, count_tbl, by = "strata")
  }

  data_tbl <- data_tbl %>%
    left_join(data_stratum_collapsed, by = "strata")
  data_long_tbl <- data_tbl %>%
    mutate(internal_id = row_number())  %>%
    group_by(strata) %>%
    mutate(n_h = n()) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = {{ parties }}, names_to = "party", values_to = "n_votes")
  estimates_tbl <- data_long_tbl |>
    group_by(strata, party, n_strata) |>
    tidyr::nest() |>
    mutate(model = map(data, ~ lm(n_votes ~ -1 + ln, data = .x,  weights =  1 / ln ))) |>
    mutate(estimate = map_dbl(model, ~ coef(.x)[1]))
  total_estimates_tbl <- estimates_tbl |>
    tidyr::unnest(c(data)) |>
    group_by(party, strata, n_strata) |>
    summarise(mean_est_cand = mean( estimate * ln), .groups = "drop") |>
    mutate(total_est_cand = n_strata * mean_est_cand) |>
    group_by(party) |>
    summarise(total_est_cand = sum(total_est_cand)) |>
    ungroup() |>
    mutate(total_est = sum(total_est_cand)) |>
    mutate(prop = total_est_cand / total_est) |>
    select(party, prop)
#    group_by(party) %>%
#    summarise(total_votes = sum(n_votes), .groups = "drop") %>%
#    mutate(prop = 100 * total_votes / sum(total_votes)) %>%
#    select(-total_votes)

#  ratio_part <-  data_long_tbl %>%
#    group_by(strata, n_h, n_strata, internal_id, LISTA_NOMINAL) %>%
#    summarise(total_votes = sum(n_votes)) %>%
#    group_by(strata, n_h, n_strata) %>%
#    summarise(total_votes_str = sum(total_votes),
#              total_ln_str = sum(LISTA_NOMINAL)) %>%
#    mutate(total = total_votes_str * n_strata / n_h,
#           total_nominal = total_ln_str * n_strata / n_h) %>%
#    ungroup() %>%
#    summarise(prop = 100 * sum(total) / sum(total_nominal)) %>%
#    mutate(party = "part")

#  ratios <- bind_rows(ratios, ratio_part)
  if (std_errors == TRUE) {
    estimates_sd <- sd_normal_estimation(
                          data_tbl = data_tbl,
                          models_tbl = estimates_tbl |> select(strata, n_strata, party, data, model),
                          data_stratum = data_stratum,
                          B = B, party_names = party_names)
    total_estimates_tbl <- left_join(total_estimates_tbl, estimates_sd, by = "party") %>%
      arrange(desc(prop))
  }
  return(total_estimates_tbl)
}
sd_normal_estimation <- function(data_tbl, models_tbl, data_stratum, B, party_names){
  # B bootstrap replicates
  #party_names <- names(data_tbl |> select( {{ parties}}))
  model_reps <- purrr::rerun(B,
      sd_normal_estimation_aux(data_tbl = data_tbl, models_tbl = models_tbl,
                              data_stratum = data_stratum, party_names = party_names))
  std_errors <- bind_rows(model_reps) %>%
    group_by(party) %>%
    summarise(std_error = stats::sd(prop), .groups = "drop")
  return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_normal_estimation_aux <- function(data_tbl, models_tbl, data_stratum, party_names){
  #party_names <- names(data_tbl |> select( {{ parties}}))
  #sample_boot <- select_sample_prop(data_tbl, stratum = strata, frac = 1,
  #                                  replace = TRUE)
  sample_boot <- models_tbl |>
    mutate(params = map(model,  ~ arm::sim(.x, n.sims = 1))) |>
    mutate(est_mean = map_dbl(params, ~ .x@coef[1,]),
           est_sigma = map_dbl(params, ~ .x@sigma[1])) |>
    tidyr::unnest(c(data)) |>
    mutate(mean_votes = est_mean * ln) |>
    mutate(n_votes = rnorm(length(est_mean), mean_votes, est_sigma)) |>
    ungroup() |>
    dplyr::select(internal_id, strata, party, ln, n_votes) |>
    tidyr::pivot_wider(names_from = "party", values_from = "n_votes")
  normal_estimation(data_tbl = sample_boot,
                   stratum = strata, data_stratum = data_stratum, n_stratum = n_strata,
                   parties = all_of(party_names), std_errors = FALSE)

}

