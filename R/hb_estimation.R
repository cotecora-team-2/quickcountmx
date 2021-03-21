#' Bayesian hierarchical model to estimate proportion of votes allocated to each party
#'
#' Compute point estimate and credible intervals for each candidate.
#' @details Posterior simulations of parameters are computed using stan,
#' and each party's votes are simulated en every polling station. There is one
#' independent model for each party, and proportions are calculated from posterior simulations
#' of total votes.
#' @param data_tbl \code{tibble} of sample data
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param id_station Unquoted variable indicating the idfor each polling
#'   station.
#' @param data_stratum Data frame with stratum variable (named exactly as in
#'   \code{data}) and number of polling stations per strata.
#' @param parties Unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param covariates Unquoted variables indicating the covariates in each polling
#'   station.
#' @param prop_obs Proportion of size of observed sample to total designed sample.
#' @param seed integer value used to set the state of the random number
#' generator (optional).
#' @param return_fit Returns summary if FALSE (default), otherwise return cmdstanr fit
#' @param num_iter Number of post warmup iterations
#' @param chains Number of chains (will be run in parallel)
#' @return A \code{tibble} including point estimates for each party (median)
#'   and limits of credible intervals.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
hb_estimation <- function(data_tbl, stratum, id_station, data_stratum, parties,
                          covariates,
                          prop_obs = 0.995, seed = NA, return_fit = FALSE,
                          num_iter = 1000, chains = 3){

  parties_enquo <- enquo(parties)
  covariates_enquo <- enquo(covariates)

  data_stratum <- data_stratum %>%
    rename(strata = {{ stratum }}) %>%
    rename(id_station = {{ id_station }})

  data_tbl <- data_tbl %>%
    ungroup() %>%
    rename(strata = {{ stratum }}) %>%
    rename(id_station = {{ id_station }})

  # Prepare data for stan model
  json_path <- system.file("stan", "prior_data.json", package = "quickcountmx")
  parameters <- jsonlite::read_json(json_path, simplifyVector = TRUE)

  data_list <- create_hb_data(data_tbl, data_stratum,
                              parties = !!parties_enquo, covariates = !!covariates_enquo,
                              prop_obs = prop_obs)
  stan_data <- c(parameters, data_list)
  parties_name <- stan_data$parties_name
  stan_data$parties_name <- NULL
  # Compile model
  path <- system.file("stan", "model_parties.stan", package = "quickcountmx")
  model <- cmdstanr::cmdstan_model(path)
  ## fit
  fit <- model$sample(data = stan_data,
                      seed = 883298,
                      iter_sampling = num_iter,
                      iter_warmup = 1000,
                      chains = chains,
                      refresh = 250,
                      parallel_chains = chains,
                      adapt_delta = 0.98,
                      max_treedepth = 12)
  if(return_fit == TRUE){
    return(fit)
  } else {
    sims_tbl <- fit$draws("prop_votos") %>%
      posterior::as_draws_df() %>%
      dplyr::as_tibble()
    names(sims_tbl)[1:length(parties_name)] <- parties_name
    estimates_tbl <- sims_tbl %>%
      tidyr::pivot_longer(cols = all_of(parties_name), names_to = "party",
                   values_to = "value") %>%
      group_by(party) %>%
      summarise(median = median(value),
                inf = quantile(value, 0.025),
                sup = quantile(value, 0.975),
                n_sim = length(value))
    return(estimates_tbl)
  }

}

create_hb_data <- function(data_tbl, data_stratum, parties,
                             covariates, prop_obs = 0.995){
  levels_strata_f <- unique(data_stratum$strata)
  data_stratum <- data_stratum %>%
    mutate(strata_num_f = as.integer(factor(strata, levels = levels_strata_f)))
  data_tbl <- data_tbl %>%
    mutate(strata_num = as.integer(factor(strata, levels = levels_strata_f)))
  in_sample_na <- data_stratum %>% select(id_station) %>%
    left_join(data_tbl %>% select(id_station, strata_num),
              by = "id_station") %>%
    pull(strata_num)
  in_sample <- ifelse(is.na(in_sample_na), 0, 1)
  votes <- data_tbl %>% select({{ parties }})
  votes_frame_tbl <- left_join(data_stratum %>% select(id_station),
                           data_tbl) %>%
    select({{ parties }}) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))
  stan_data <- list()
  # frame data
  stan_data$parties_name <- colnames(votes)
  stan_data$N_f = nrow(data_stratum)
  stan_data$n_strata_f = length(levels_strata_f)
  stan_data$p <- ncol(votes_frame_tbl)
  stan_data$y_f <- votes_frame_tbl %>% as.matrix()
  stan_data$in_sample <- in_sample
  stan_data$n_f <- data_stratum$ln
  stan_data$stratum_f <- data_stratum$strata_num_f
  stan_data$x_f <- data_stratum %>% select({{ covariates }}) %>%
    as.matrix()
  stan_data$n_covariates_f <- ncol(stan_data$x)
  # sample data
  stan_data$N <- nrow(data_tbl)
  stan_data$y <- stan_data$y_f[in_sample==1, , drop = FALSE]
  stan_data$x <- stan_data$x_f[in_sample==1, , drop = FALSE]
  stan_data$stratum <- stan_data$stratum_f[in_sample == 1]
  stan_data$n <- stan_data$n_f[in_sample == 1]
  stan_data$p_obs <- prop_obs

  return(stan_data)
}
