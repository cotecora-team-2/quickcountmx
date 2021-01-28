#' Ratio estimator to compute proportion of votes allocated to each party
#'
#' Compute ratio estimator for each candidate, standard errors are computed
#' with bootstrap resampling within each stratum and computing the standard
#' error of the samples (no corrections).
#' @details The bootstrap approach we use is not suitable
#' when the number of sampled polling stations within a strata is small.
#' Coverage might improve if confidence intervals are constructed with BCas or
#' t-tables.
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
#' @param B Number of bootstrap replicates used to compute standard errors,
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional). It will only be used when computing standard errors.
#' @return A \code{tibble} including the ratio estimation for each party
#'   and standard errors (if requested).
#' @examples
#' # count number of polling stations per stratum
#' library(dplyr)
#' stratum_sizes <- conteo_2018 %>%
#'     dplyr::group_by(ID_DISTRITO) %>%
#'     dplyr::summarise(n_stratum = n())
#' # stratified random sample (size 6%), sample size proportional to strata size
#' sample <- select_sample_prop(conteo_2018, stratum = ID_DISTRITO, 0.06)
#' ratio_estimation(sample, stratum = ID_DISTRITO,
#'   data_stratum = stratum_sizes, n_stratum = n_stratum, any_of(c("AMLO", "JAMK")))
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
ratio_estimation <- function(data_tbl, stratum, data_stratum, n_stratum, parties,
                             std_errors = TRUE, B = 50, seed = NA){

  parties_enquo <- dplyr::enquo(parties)
  pos <- tidyselect::eval_select(parties_enquo, data = data_tbl)
  data_stratum <- data_stratum %>%
    dplyr::rename(strata = {{ stratum }}, n_strata = {{ n_stratum }})

  # calculate estimates
  data_tbl <- data_tbl %>%
    dplyr::ungroup() %>%
    dplyr::rename(strata = {{ stratum }})  %>%
    dplyr::left_join(data_stratum, by = "strata")
  ratios <- data_tbl %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(n_h = length(strata)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = {{ parties }}, names_to = "party", values_to = "n_votes") %>%
    dplyr::mutate(
      n_aux = (n_strata / n_h) * n_votes
    ) %>%
    dplyr::group_by(strata, party) %>%
    dplyr::summarise(n_votes = sum(n_aux), .groups = "drop") %>%
    dplyr::group_by(party) %>%
    dplyr::summarise(y = sum(n_votes), .groups = "drop") %>%
    dplyr::mutate(r = 100 * y / sum(y)) %>%
    dplyr::select(-y)

  if (std_errors == TRUE) {
    ratios_sd <- sd_ratio_estimation(data_tbl = data_tbl,
                                     data_stratum = data_stratum,
                                     B = B, parties = pos)
    ratios <- dplyr::left_join(ratios, ratios_sd, by = "party") %>%
      dplyr::arrange(desc(r))
  }
  return(ratios)
}
sd_ratio_estimation <- function(data_tbl, data_stratum, B, parties){
  # B bootstrap replicates
  ratio_reps <- purrr::rerun(B,
      sd_ratio_estimation_aux(data_tbl = data_tbl,
                              data_stratum = data_stratum, parties = parties))
  std_errors <- dplyr::bind_rows(ratio_reps) %>%
    dplyr::group_by(party) %>%
    dplyr::summarise(std_error = sd(r), .groups = "drop")
  return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_ratio_estimation_aux <- function(data_tbl, data_stratum, parties){
  parties_enquo <- dplyr::enquo(parties)
  pos <- tidyselect::eval_select(parties_enquo, data = data_tbl)
  sample_boot <- select_sample_prop(data_tbl, stratum = strata, frac = 1,
                                    replace = TRUE)
  ratio_estimation(data_tbl = sample_boot %>% dplyr::select(-n_strata),
                   stratum = strata, data_stratum = data_stratum, n_stratum = n_strata,
                   parties = pos, std_errors = FALSE)

}
