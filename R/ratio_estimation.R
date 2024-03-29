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
#' conteo_2018 <- conteo_2018 %>%
#'     dplyr::rename(LISTA_NOMINAL = LISTA_NOMINAL_CASILLA)
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
  data_tbl <- data_tbl %>%
    left_join(data_stratum_collapsed, by = "strata")
  data_long_tbl <- data_tbl %>%
    mutate(internal_id = row_number())  %>%
    group_by(strata) %>%
    mutate(n_h = n()) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = {{ parties }}, names_to = "party", values_to = "n_votes")
  ratios <-  data_long_tbl %>%
    mutate(n_aux = (n_strata / n_h) * n_votes) %>%
    group_by(strata, party) %>%
    summarise(n_votes = sum(n_aux), .groups = "drop") %>%
    group_by(party) %>%
    summarise(total_votes = sum(n_votes), .groups = "drop") %>%
    mutate(prop = 100 * total_votes / sum(total_votes)) %>%
    select(-total_votes)

  ratio_part <-  data_long_tbl %>%
    group_by(strata, n_h, n_strata, internal_id, LISTA_NOMINAL) %>%
    summarise(total_votes = sum(n_votes)) %>%
    group_by(strata, n_h, n_strata) %>%
    summarise(total_votes_str = sum(total_votes),
              total_ln_str = sum(LISTA_NOMINAL)) %>%
    mutate(total = total_votes_str * n_strata / n_h,
           total_nominal = total_ln_str * n_strata / n_h) %>%
    ungroup() %>%
    summarise(prop = 100 * sum(total) / sum(total_nominal)) %>%
    mutate(party = "part")

  ratios <- bind_rows(ratios, ratio_part)
  if (std_errors == TRUE) {
    ratios_sd <- sd_ratio_estimation(data_tbl = data_tbl,
                                     data_stratum = data_stratum,
                                     B = B, parties = {{ parties }})
    ratios <- left_join(ratios, ratios_sd, by = "party") %>%
      arrange(desc(prop))
  }
  return(ratios)
}
sd_ratio_estimation <- function(data_tbl, data_stratum, B, parties){
  # B bootstrap replicates
  ratio_reps <- purrr::map(1:B, function(b){
      sd_ratio_estimation_aux(data_tbl = data_tbl,
                              data_stratum = data_stratum, parties = {{ parties }})})
  std_errors <- bind_rows(ratio_reps) %>%
    group_by(party) %>%
    summarise(std_error = stats::sd(prop), .groups = "drop")
  return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_ratio_estimation_aux <- function(data_tbl, data_stratum, parties){
  sample_boot <- select_sample_prop(data_tbl, stratum = strata, frac = 1,
                                    replace = TRUE)
  ratio_estimation(data_tbl = sample_boot %>% dplyr::select(-n_strata),
                   stratum = strata, data_stratum = data_stratum, n_stratum = n_strata,
                   parties = {{ parties }}, std_errors = FALSE)

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

#' Bootstrap replicates to estimate diputados results
#'
#' Compute bootstrap resamples of ratio estimator for each party at national level,
#' along with proportion of votes for each stratum and party.
#' @param data_tbl \code{tibble}
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param data_stratum Data frame with stratum variable (named exactly as in
#'   \code{data}) and number of polling stations per strata.
#' @param n_stratum Unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param coalitions_tbl Tibble with coalitions names and corresponding parties.
#' @param B Number of bootstrap replicates,
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional).
#' @return A \code{list} including two componentes: point estimates at national level
#' and at stratum level, and bootstrap replications of these quantities.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
bootstrap_diputados <- function(data_tbl, stratum, stratum_tbl, n_stratum,
                                coalitions_tbl, B = 50, seed = NA){

  stratum_tbl <- stratum_tbl |>
    rename(strata = {{ stratum }}, n_strata = {{ n_stratum }})

  data_tbl <- data_tbl |>
    ungroup() |>
    rename(strata = {{ stratum }})
  parties_chr <- unique(coalitions_tbl$party)
  coalitions <- unique(coalitions_tbl$coalition)

  coalitions_tbl <- coalitions_tbl |>
    group_by(coalition) |>
    mutate(multiplier = 1/n()) |>
    ungroup()

  data_parties_long_tbl <- data_tbl |>
    mutate(internal_id = row_number()) |>
    select(internal_id, strata, all_of(coalitions), LISTA_NOMINAL) |>
    tidyr::pivot_longer(cols = all_of(coalitions), names_to = "coalition", values_to = "n_votes") |>
    dplyr::left_join(coalitions_tbl, by = "coalition", relationship = "many-to-many") |>
    dplyr::mutate(n_votes_weighted = n_votes * multiplier)|>
    group_by(internal_id, strata, party, LISTA_NOMINAL) |>
    summarise(n_votes = sum(n_votes_weighted), .groups = "drop")

  point_estimate <- calculate_diputados(data_parties_long_tbl, stratum, stratum_tbl, n_stratum,
                                        coalitions_tbl, parties_chr)
  if(is.na(seed)){
    seed <- 2212
  }
  bootstrap_reps <- NULL
  if(B > 0){
    sample_ids <- unique(data_parties_long_tbl$internal_id)
    set.seed(seed)
    bootstrap_reps <- purrr::map(1:B, function(b){
      sample_ids_bootstrap <- tibble(internal_id = sample(sample_ids, replace = TRUE))
      data_parties_long_tbl_bootstrap <- data_parties_long_tbl |>
        semi_join(sample_ids_bootstrap, by = "internal_id")
      calculate_diputados(data_parties_long_tbl_bootstrap, stratum, stratum_tbl, n_stratum,
                          coalitions_tbl, parties_chr)
    })
  }
  return(list(point_estimate = point_estimate, bootstrap_reps = bootstrap_reps))
}


calculate_diputados <- function(data_parties_long_tbl, stratum, stratum_tbl, n_stratum,
                                coalitions_tbl, parties_chr){

  data_parties_tbl <- data_parties_long_tbl |>
    tidyr::pivot_wider(names_from = party, values_from = n_votes, values_fill = 0)

  ratio <- ratio_estimation(data_parties_tbl, strata, stratum_tbl,n_stratum = n_strata,
                            parties = tidyr::all_of(parties_chr), B=0, std_errors = FALSE)

  estimates_strata_tbl <- data_parties_long_tbl |>
    group_by(strata, party) |>
    summarise(total_votes = sum(n_votes), .groups = "drop_last") |>
    mutate(prop_votes = total_votes / sum(total_votes)) |>
    ungroup()

  list(estimates_total = ratio, estimates_strata = estimates_strata_tbl)
}
