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

  parties_enquo <- enquo(parties)
  party_select <- tidyselect::eval_select(parties_enquo, data = data_tbl)
  data_stratum <- data_stratum %>%
    rename(strata = {{ stratum }}, n_strata = {{ n_stratum }})

  # calculate estimates
  data_tbl <- data_tbl %>%
    ungroup() %>%
    rename(strata = {{ stratum }})

  # collapse strata if needed
  if(n_distinct(data_tbl$strata) < n_distinct(data_stratum$strata)) {
    data_stratum <- collapse_strata(data_tbl, data_stratum)
  }
  print(n_distinct(data_stratum$strata))
  data_tbl <- data_tbl %>%
    left_join(data_stratum, by = "strata")
  ratios <- data_tbl %>%
    group_by(strata) %>%
    mutate(n_h = n()) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = {{ parties }}, names_to = "party", values_to = "n_votes") %>%
    mutate(
      n_aux = (n_strata / n_h) * n_votes
    ) %>%
    group_by(strata, party) %>%
    summarise(n_votes = sum(n_aux), .groups = "drop") %>%
    group_by(party) %>%
    summarise(total_votes = sum(n_votes), .groups = "drop") %>%
    mutate(prop = 100 * total_votes / sum(total_votes)) %>%
    select(-total_votes)

  if (std_errors == TRUE) {
    ratios_sd <- sd_ratio_estimation(data_tbl = data_tbl,
                                     data_stratum = data_stratum,
                                     B = B, parties = party_select)
    ratios <- left_join(ratios, ratios_sd, by = "party") %>%
      arrange(desc(prop))
  }
  return(ratios)
}
sd_ratio_estimation <- function(data_tbl, data_stratum, B, parties){
  # B bootstrap replicates
  ratio_reps <- purrr::rerun(B,
      sd_ratio_estimation_aux(data_tbl = data_tbl,
                              data_stratum = data_stratum, parties = parties))
  std_errors <- bind_rows(ratio_reps) %>%
    group_by(party) %>%
    summarise(std_error = stats::sd(prop), .groups = "drop")
  return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_ratio_estimation_aux <- function(data_tbl, data_stratum, parties){
  parties_enquo <- enquo(parties)
  party_select <- tidyselect::eval_select(parties_enquo, data = data_tbl)
  sample_boot <- select_sample_prop(data_tbl, stratum = strata, frac = 1,
                                    replace = TRUE)
  ratio_estimation(data_tbl = sample_boot %>% dplyr::select(-n_strata),
                   stratum = strata, data_stratum = data_stratum, n_stratum = n_strata,
                   parties = party_select, std_errors = FALSE)

}
# auxiliary function, to collapse strata
collapse_strata <- function(data_tbl, data_stratum){
  data_obs <- data_tbl %>%
    count(strata, name = "n_observed") %>%
    mutate(strata = as.character(strata))
  data_missings <- data_stratum %>%
    mutate(strata = as.character(strata)) %>%
    left_join(data_obs, by = "strata")
  data_strata_collapsed <- data_missings %>%
    rowwise() %>%
    mutate(strata = ifelse(is.na(n_observed),
                           sample(stats::na.omit(data_missings$strata), 1),
                           strata)) %>%
    group_by(strata) %>%
    summarise(n_strata = sum(n_strata)) %>%
    ungroup()
  data_strata_collapsed
}
