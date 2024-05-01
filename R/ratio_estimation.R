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

  data_tbl <- data_tbl %>%
    left_join(data_stratum, by = "strata")

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

  # collapse strata if any empty
  if(n_distinct(data_tbl$strata) < n_distinct(data_stratum$strata)) {
    data_stratum_collapsed <- collapse_strata(data_tbl, data_stratum)
  } else {
    data_stratum_collapsed <- data_stratum
  }

  sample_boot <- select_sample_prop(data_tbl, stratum = strata, frac = 1,
                                    replace = TRUE)
  ratio_estimation(data_tbl = sample_boot %>% dplyr::select(-n_strata),
                   stratum = strata, data_stratum = data_stratum_collapsed,
                   n_stratum = n_strata,
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
#' @param stratum_tbl Data frame with stratum variable (named exactly as in
#'   \code{data}) and number of polling stations per strata.
#' @param n_stratum Unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param coalitions_tbl Tibble with coalitions names and corresponding parties.
#' @param B Number of bootstrap replicates,
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional).
#' @param samples_table logical indicating if the function should return the samples
#' as a list of tibbles for bootstrap samples
#' @return A \code{list} including two componentes: point estimates at national level
#' and at stratum level, and bootstrap replications of these quantities.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
bootstrap_diputados <- function(data_tbl, stratum, stratum_tbl, n_stratum,
                                coalitions_tbl, B = 50, seed = NA, samples_table = FALSE){

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
    sample_ids <- distinct(data_parties_long_tbl, internal_id, strata) |>
      group_by(strata)
    set.seed(seed)
    bootstrap_reps <- purrr::map(1:B, function(b){
      sample_ids_bootstrap <- slice_sample(sample_ids, prop = 1.0, replace = TRUE) |>
        ungroup() |>
        mutate(internal_id_bs = 1:n())
      data_parties_long_tbl_bootstrap <- data_parties_long_tbl |>
        right_join(sample_ids_bootstrap, by = c("internal_id", "strata"), relationship = "many-to-many") |>
        ungroup() |>
        select(-internal_id)
      calculate_diputados(data_parties_long_tbl_bootstrap, stratum, stratum_tbl, n_stratum,
                          coalitions_tbl, parties_chr)
    })
  }
  output <- list(point_estimate = point_estimate, bootstrap_reps = bootstrap_reps)
  if(samples_table){
    total_tbl <- purrr::map_dfr(1:B,  ~ output$bootstrap_reps[[.x]]$estimates_total |> mutate(rep = .x))
    strata_tbl <- purrr::map_dfr(1:B, ~ output$bootstrap_reps[[.x]]$estimates_strata |> mutate(rep = .x))
    output <- list(total_tbl = total_tbl, strata_tbl = strata_tbl)
  }
  return(output)
}


calculate_diputados <- function(data_parties_long_tbl, stratum, stratum_tbl, n_stratum,
                                coalitions_tbl, parties_chr){

  data_parties_tbl <- data_parties_long_tbl |>
    tidyr::pivot_wider(names_from = party, values_from = n_votes, values_fill = 0)

  ratio <- ratio_estimation(data_parties_tbl, strata, stratum_tbl,
                            n_stratum = n_strata,
                            parties = tidyr::all_of(parties_chr), B=0, std_errors = FALSE) |>
    mutate(prop = prop / 100)

  estimates_strata_tbl <- data_parties_long_tbl |>
    group_by(strata, party) |>
    summarise(total_votes = sum(n_votes), .groups = "drop_last") |>
    mutate(prop_votes = total_votes / sum(total_votes)) |>
    ungroup()

  list(estimates_total = ratio, estimates_strata = estimates_strata_tbl)
}

#' @name assign_deputy_seats
#' @aliases assign_all_seats
#' @aliases assign_majority
#' @aliases assign_prop
#'
#' @title  Functions that help with the  assignment of the Chamber of Deputies.
#'
#' @description There are 500 seats to be assigned in the Chamber of Deputies in Mexico.
#' The process involves different steps.
#' `assign_all_seats`: Assigns the total of 500 seats of majority and proportional seats, checking for maximum constraints per
# party and replication.
#' `assign_majority`: Add column defining the majority party of the first 300 seats to each stratum and repetition.
#' `add_max_seats`: Adds the maximum seats allowed out of the 500 seats to each party and repetition.
#' `assign_prop`: Assigns n_assign seats to parties using proportional representation in national voting and "resto mayor".
#' @param reps_list A named list (output of bootstrap_diputados) with elements strata_tbl and total_tbl
#' @param estimates_strata_tbl Tibble with estimates of proportion of votes per party per repetition per stratum (output of bootstrap_diputados)
#' @param assignment_tbl Tibble with party names and corresponding assigned party by stratum.
#' @param party_name Unquoted variable indicating the party name.
#' @param candidate_name Unquoted variable indicating the assigned party.
#' @param total_tbl Tibble with estimates of proportion of votes per party per repetition (output of bootstrap_diputados)
#' total_tbl must include columns: n_assign seats to allocate, per replication and
#' topped: a logical value whether the party has reached it's maximum number of seats (TRUE)
#' @param majority_seats_tbl Tibble with output from `assign_majority`. A tibble with columns:
#' `rep` number of repetition, `candidate` the party, and
#' `n_seats_maj` integer, number of seats assigned by majority for each of the parties out of the first 300;
#'
#' @rdname assign_deputy_seats
#' @return  `assign_majority` returns a tibble with columns:
#' `rep` number of repetition, `candidate` the party, and
#' `n_seats_maj` integer, number of seats assigned by majority for each of the parties out of the first 300;
#'
#' @export
assign_majority <- function(estimates_strata_tbl, assignment_tbl,
                            party_name, candidate_name){
  assignment_tbl <- assignment_tbl |>
    rename(party = {{ party_name }}) |>
    rename(candidate = {{ candidate_name }})

  aggregate_coalitions_tbl <- estimates_strata_tbl |>
    left_join(assignment_tbl, by = c("party", "strata")) |>
    mutate(candidate = ifelse(is.na(candidate), party, candidate)) |>
    group_by(rep, strata, candidate) |>
    summarise(prop_votes = sum(prop_votes), .groups = "drop_last") |>
    mutate(
      prop_votes_aux = prop_votes + stats::runif(dplyr::n(), 0, 1e-8), # fix ties
      is_majority = prop_votes_aux == max(prop_votes_aux)) |>
    ungroup() |>
    select(-prop_votes_aux) |>
    group_by(rep, candidate) |>
    summarise(n_seats_maj = sum(is_majority), .groups = "drop")
  aggregate_coalitions_tbl
}

#' @rdname assign_deputy_seats
#' @return  `assign_all_seats` returns a tibble with columns:
#' `rep` number of repetition, `party` the party, and
#' `n_seats_maj` integer, number of seats assigned by majority for each of the parties out of the first 300;
#' `n_seats_prop` integer, is the number of seats assigned by proportionality and "resto mayor", out of the 200, for each of the parties;
#' `n_seats_total` integer, is the total number of seats, out of the 500, for each of the parties;
#'
#' @export
assign_all_seats <- function(reps_list, assignment_tbl) {

  majority_seats_tbl <- assign_majority(reps_list$strata_tbl, assignment_tbl ,
    party_name = party, candidate_name = candidato)

  total_tbl <- add_max_seats(reps_list$total_tbl, majority_seats_tbl)

  total_seats_tbl <- assign_prop(total_tbl)

  to_assign <- total_seats_tbl |>
    dplyr::summarise(miss_assign = 500 - sum(n_seats_total))

  # if seats left assign in second round
  while(sum(to_assign$miss_assign) > 0) {
    total_seats_tbl <- total_seats_tbl |>
      dplyr::mutate(n_assign = 500 - sum(n_seats_maj) - sum(topped * n_seats_prop)) |>
      dplyr::select(party, rep, prop, prop_vot_nal, n_seats_maj, n_seats_max,
                    topped, n_assign)

    total_seats_tbl <- assign_prop(total_seats_tbl)
    to_assign <- total_seats_tbl |>
      dplyr::summarise(miss_assign = 500 - sum(n_seats_total))
  }
  total_seats_tbl
}

#'
#' @rdname assign_deputy_seats
#' @return  `add_max_seats` returns a tibble with columns:
#' `rep` number of repetition, `party` the party, and
#' `prop_vot_nal` normalised proportion to calculate maximum allowed;
#' `n_seats_maj` integer, number of seats assigned by majority for each of the parties out of the first 300;
#' `n_seats_max` integer, is the maximum number of seats allowed per party.
#' If the party reached/passed its maximum with the majority assignment, `n_seats_max` is set to `n_seats_maj`;
#' `topped` logical, indicates if the party has reached its maximum;
#' @export
add_max_seats <- function(total_tbl, majority_seats_tbl) {

  total_tbl <- total_tbl |>
    dplyr::filter(party != "part")

  # These do not count towars "votacion nacional emitida", same for parties with <3%
  parties_ignore <- c("CNR", "NULOS", "CI")

  total_tbl |>
    dplyr::group_by(rep) |>
    dplyr::mutate(
      #votación nal. emitida
      prop_vot_nal = ifelse(prop < 0.03 |
                          stringr::str_detect(party, paste(parties_ignore, collapse = "|")),
                        0, prop),
      prop_vot_nal = prop_vot_nal / sum(prop_vot_nal),
    ) |>
    dplyr::left_join(majority_seats_tbl, by = c("rep" = "rep", "party" = "candidate")) |>
    dplyr::mutate(
      n_seats_max_raw = floor(500 * (prop_vot_nal + 0.08)),
      n_seats_max = pmax(n_seats_max_raw, n_seats_maj), #we do not delete majority seats even if over limit
      topped = n_seats_maj >= n_seats_max_raw,
      n_assign = 200
    )
}
#'
#' @rdname assign_deputy_seats
#' @return  `assign_prop` returns a tibble where :
#' `rep` is the number of repetition, `party` is the name of the party, and
#' `n_seats_prop` integer, is the number of seats assigned by proportionality and "resto mayor" for each of the parties;
#' `n_seats_total` integer, is the number of seats assigned by majority and proportional;
#' `topped` logical, indicates if a party has reached its maximum number of seats;
#'
#' @export
assign_prop <- function(total_tbl) {

  total_tbl |>
    dplyr::group_by(rep) |>
    dplyr::mutate(
      prop_adj = ifelse(topped, 0, prop_vot_nal),
      prop_adj = prop_adj / sum(prop_adj),
      n_seats_prop_raw = floor(n_assign * prop_adj),
      resto = n_assign * prop_adj - n_seats_prop_raw,
      to_distribute = ifelse(n_assign > 0, n_assign - sum(n_seats_prop_raw), 0),
      rank_resto = dplyr::row_number(-resto),
      n_seats_prop_raw = ifelse(rank_resto <= to_distribute, n_seats_prop_raw + 1, n_seats_prop_raw)
    ) |>
    dplyr::mutate(
      n_seats_prop = ifelse(topped, n_seats_max - n_seats_maj,
                            pmin(n_seats_max - n_seats_maj, n_seats_prop_raw)),
      n_seats_total = ifelse(topped, n_seats_max, n_seats_prop + n_seats_maj),
      topped = n_seats_max <= n_seats_total
    )
}

#' Bootstrap estimates for diputados
#'
#' Compute bootstrap confidence intervals using ratio estimator for each party at national level,
#' along with proportion of votes for each stratum and party.
#' @inherit bootstrap_diputados
#' @param assignment_tbl data.frame indicating the strata where coalitions apply
#' and the candidate assigned to each. Party columns must be named party, the candidate party
#' must be named party.
#' @export
ratio_estimation_diputados <- function(data_tbl, stratum, stratum_tbl, n_stratum,
                                           coalitions_tbl, assignment_tbl, B = 50,
                                       seed = NA) {
  data_tbl
  estimates <- bootstrap_diputados(data_tbl = data_tbl, stratum = {{stratum}},
                                   stratum_tbl = stratum_tbl, n_stratum = {{n_stratum}},
                                   coalitions_tbl = coalitions_tbl,
                                   B = B, seed = seed, samples_table = TRUE)

  assign_seats_rep <- assign_all_seats(estimates, assignment_tbl)
  part_tbl <- estimates$total_tbl |>
    filter(party == "part") |>
    group_by(party) |>
    dplyr::summarise(dplyr::across(c(prop), list(median = median,
                                                 inf = ~ quantile(., 0.02),
                                                 sup = ~ quantile(., 0.98)))) |>
    ungroup()

  assign_seats_rep |>
    dplyr::mutate(party = ifelse(stringr::str_detect(party, "^CI"), "IND", party)) |>
    dplyr::group_by(party) |>
    dplyr::summarise(dplyr::across(c(prop, n_seats_total), list(median = median,
                                                  inf = ~ quantile(., 0.02),
                                                  sup = ~ quantile(., 0.98)))) |>
    ungroup() |>
    dplyr::bind_rows(part_tbl)

}
