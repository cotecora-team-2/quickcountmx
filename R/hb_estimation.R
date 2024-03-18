#' Bayesian hierarchical model to estimate proportion of votes allocated to each party
#'
#' Compute point estimate and credible intervals for each candidate.
#' @details Posterior simulations of parameters are computed using stan,
#' and each party's votes are simulated for every polling station (logit model)
#' or with a softmax link for the default (mlogit model). There is one
#' independent model for each party, and proportions are calculated from posterior simulations
#' of total votes.
#' @param data_tbl \code{tibble} of sample data.
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param id_station Unquoted variable indicating the id for each polling
#'   station.
#' @param sampling_frame \code{tibble} of sampling frame with stratum variable (named exactly as in
#'   \code{data_tbl}) and covariates.
#' @param parties Unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param covariates Unquoted variables indicating the covariates in each polling
#'   station.
#' @param prop_obs Proportion of size of observed sample to total designed sample.
#' @param seed integer value used to set the state of the random number
#' generator (optional).
#' @param return_fit Returns summary if FALSE (default), otherwise return cmdstanr fit
#' @param num_iter Number of post warmup iterations
#' @param num_warmup Number of warmup iterations
#' @param adapt_delta The adaptation target acceptance statistic (default 0.80.
#' @param max_treedepth The maximum allowed tree depth for the NUTS engine (default 10)
#' @param chains Number of chains (will be run in parallel)
#' @param model One of "mlogit" (the default) or "logit"
#' @param nominal_max Maximum number of nominal count for stations. Used for
#' stations without fixed nominal list.
#' @param inv_metric vector of inverse metric diagonal for the model. Default is NULL
#' @param threads_per_chain Number of threads per chain to split calculation of log-posterior
#' @param results_strata If TRUE, returns a list with the results for each stratum
#' @return A list with model fit (if return_fit=TRUE), a \code{tibble}
#' estimates including point estimates for each party (median)
#'   and limits of credible intervals, and a vector inv_metric for the model,
#' When results_strata=TRUE, there is a component strata_draws.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
hb_estimation <- function(data_tbl, stratum, id_station, sampling_frame, parties,
                          covariates,
                          prop_obs = 0.995, seed = NULL, return_fit = FALSE,
                          num_iter = 200, num_warmup = 200, adapt_delta = 0.80,
                          max_treedepth = 10,
                          chains = 3, model = "mlogit-corr", nominal_max = 1200,
                          threads_per_chain = 1, inv_metric = NULL, results_strata = FALSE){

  sampling_frame <- sampling_frame %>%
    rename(strata = {{ stratum }}) %>%
    rename(id_station = {{ id_station }})

  data_tbl <- data_tbl %>%
    ungroup() %>%
    rename(strata = {{ stratum }}) %>%
    rename(id_station = {{ id_station }})

  # Prepare data for stan model
  if(model == "mlogit"){
    json_path <- system.file("stan", "prior_data.json", package = "quickcountmx")
  } else {
    json_path <- system.file("stan", "prior_data_corr.json", package = "quickcountmx")
  }
  parameters <- jsonlite::read_json(json_path, simplifyVector = TRUE)
  parameters$nominal_max <- nominal_max
  data_list <- create_hb_data(data_tbl, sampling_frame,
                              parties = {{parties}}, covariates = {{covariates}},
                              prop_obs = prop_obs)
  stan_data <- c(parameters, data_list)
  parties_name <- stan_data$parties_name
  stan_data$parties_name <- NULL
  # Compile model
  if(model == "mlogit"){
    path <- system.file("stan", "model_parties_mlogit_corr.stan", package = "quickcountmx")
    adapt_delta <- adapt_delta
    max_treedepth <- max_treedepth
    iter_warmup <- num_warmup
  } else {
    if(model == "consulta"){
      path <- system.file("stan", "model_parties_mlogit_corr_consulta.stan", package = "quickcountmx")
      adapt_delta <- adapt_delta
      max_treedepth <- max_treedepth
      iter_warmup <- num_warmup
    }
    else {
      path <- system.file("stan", "model_parties_mlogit_corr.stan", package = "quickcountmx")
      adapt_delta <- adapt_delta
      max_treedepth <- max_treedepth
      iter_warmup <- num_warmup
    }
  }

  model <- cmdstanr::cmdstan_model(path, cpp_options = list(stan_threads = TRUE))
  ## fit

  fit <- model$sample(data = stan_data,
                      seed = seed,
                      init = 0.01,
                      iter_sampling = num_iter,
                      iter_warmup = num_warmup,
                      chains = chains,
                      refresh = 100,
                      parallel_chains = chains,
                      step_size = 0.01,
                      adapt_delta = adapt_delta,
                      max_treedepth = max_treedepth,
                      inv_metric = inv_metric,
                      threads_per_chain = threads_per_chain)
  output <- list()
  output$fit <- NULL
  if(return_fit == TRUE){
    output$fit <- fit
  }
  output$inv_metric <- fit$inv_metric()[[1]] |> diag()
  estimates_tbl <- NULL
  estimates_tbl <- fit$summary(variables = c("prop_votos", "participacion"),
                               ~ quantile(.x, probs = c(0.025, 0.5, 0.975)),
                               rhat = ~ posterior::rhat(.x),
                               ess = ~ posterior::ess_basic(.x))
  names(estimates_tbl) <- c("party", "inf", "median", "sup", "rhat", "ess")
  estimates_tbl$party <- c(parties_name, "part")
  print(estimates_tbl)
  output$estimates <- estimates_tbl

  if(results_strata){
    strata_draws <- fit$draws(c("prop_votos_strata", "participacion_strata"), format = "df") |>
      as_tibble() |>
      tidyr::pivot_longer(cols = c(contains("strata")), names_to = "variable", values_to = "prop") |>
      tidyr::separate("variable", into = c("tipo", "estrato", "partido"), sep="[\\[\\,\\]]", extra = "drop") |>
      select(.draw, tipo, estrato, partido, prop) |>
      mutate(estrato = as.integer(estrato), partido = as.integer(partido)) |>
      left_join(tibble(partido = 1: length(parties_name),
                       partido_nom = parties_name,
                       tipo = "prop_votos_strata"), by = c("partido", "tipo"))
    output$strata_draws <- strata_draws
  }

  return(output)
}

create_hb_data <- function(data_tbl, sampling_frame, parties,
                             covariates, prop_obs = 0.995){
  levels_strata_f <- unique(sampling_frame$strata)
  sampling_frame <- sampling_frame %>%
    mutate(strata_num_f = as.integer(factor(strata, levels = levels_strata_f)))
  data_tbl <- data_tbl %>%
    mutate(strata_num = as.integer(factor(strata, levels = levels_strata_f)))
  in_sample_na <- sampling_frame %>% select(id_station) %>%
    left_join(data_tbl %>% select(id_station, strata_num),
              by = "id_station") %>%
    pull(strata_num)
  in_sample <- ifelse(is.na(in_sample_na), 0, 1)
  votes <- data_tbl %>% select({{ parties }})
  votes_frame_tbl <- left_join(sampling_frame %>% select(id_station),
                           data_tbl) %>%
    select({{ parties }}) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))
  stan_data <- list()
  # frame data
  stan_data$parties_name <- colnames(votes)
  stan_data$N_f = nrow(sampling_frame)
  stan_data$n_strata_f = length(levels_strata_f)
  stan_data$p <- ncol(votes_frame_tbl)
  stan_data$y_f <- votes_frame_tbl %>% as.matrix()
  stan_data$in_sample <- in_sample
  stan_data$n_f <- sampling_frame$ln
  stan_data$stratum_f <- sampling_frame$strata_num_f
  stan_data$x_f <- sampling_frame %>% select({{ covariates }}) %>%
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
