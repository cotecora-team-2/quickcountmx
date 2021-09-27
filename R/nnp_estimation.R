#' Normal No-Pooling model
#'
#' This function implements the Normal No-Pooling model from \cite{AA}.
#' @param data \code{data.frame}
#' @param ln Unquoted variable indicating the nominal list (number of potential
#'   voters) at each polling station.
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param data_stratum Data frame with stratum variable (named exactly as in
#'   \code{data}) and number of polling stations per strata.
#' @param n_stratum Unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param ... Unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param std_errors Logical value indicating whether to compute standard errors
#'  (using bootstrap), defaults to TRUE.
#' @param B Number of bootstrap replicates used to compute standard errors,
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional). It will only be used when computing standard errors.
#' @return A list with two entries:
#' \enumerate{
#'   \item lambdas_summary \code{data.frame} including posterior mean, median,
#'   standard error, and quantiles (0.025 and 0.975) for each party.
#'   \item lamdas_sim \code{data.frame} with simulations for each party.
#' }
#' @examples
#' # count number of polling stations per stratum
#'
#' zac_stratum_sizes <- zac_2015 %>%
#'     dplyr::group_by(ID_DISTRITO_15) %>%
#'     dplyr::summarise(n_stratum = n())
#' zac_sample <- select_sample_prop(zac_2015, stratum = ID_DISTRITO_15, 0.06) %>%
#'   filter(LISTA_NOMINAL_15 > 0)
#' zac_nnp <- nnp_estimation(data = zac_sample, ln = LISTA_NOMINAL_15,
#'   stratum = ID_DISTRITO_15,
#'   data_stratum = zac_stratum_sizes, n_stratum = n_stratum,
#'   contains("CAND"), n_sims = 100)
#' zac_nnp$lambdas_summary
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
nnp_estimation <- function(data, ln, stratum, data_stratum, n_stratum,
                           ..., n_sims = 1000){
  stratum_enquo <- dplyr::enquo(stratum)
  n_stratum_enquo <- dplyr::enquo(n_stratum)
  ln_enquo <- dplyr::enquo(ln)
  parties_enquo <- dplyr::quos(...)
  data_long <- data %>%
    dplyr::mutate(casilla_id = 1:dplyr::n()) %>%
    dplyr::rename(strata = !!stratum_enquo, ln_total = !!ln_enquo) %>%
    dplyr::select(casilla_id, strata, ln_total, !!!parties_enquo) %>%
    tidyr::gather(party, votes, !!!parties_enquo)
  data_stratum <- data_stratum %>%
    dplyr::rename(strata = !!stratum_enquo, n_strata = !!n_stratum_enquo)
  theta_sims <- data_long %>%
    split(list(.$strata, .$party)) %>%
    purrr::map_df(~sims_posterior(x_j = .$votes, n = .$ln_total,
                                  n_sims = n_sims)$theta) %>%
    dplyr::mutate(n_sim = 1:dplyr::n()) %>%
    tidyr::gather(estrato_partido, theta, -n_sim) %>%
    tidyr::separate(estrato_partido, into = c("strata", "party"),
                    sep = "[.]") %>%
    dplyr::mutate(strata = as.numeric(strata)) %>%
    dplyr::left_join(data_stratum, by = "strata")
  lambdas <- theta_sims %>%
    dplyr::group_by(party, n_sim) %>%
    dplyr::summarise(theta_wgt = sum(n_strata * theta / sum(n_strata))) %>%
    dplyr::group_by(n_sim) %>%
    dplyr::mutate(lambda = 100 * theta_wgt / sum(theta_wgt)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-theta_wgt)

  lambdas_summary <- lambdas %>%
    dplyr::group_by(party) %>%
    dplyr::summarise(
      mean_post = mean(lambda),
      median_post = median(lambda),
      std_error = sd(lambda),
      q_low = quantile(lambda, 0.025),
      q_sup = quantile(lambda, 0.975)
    ) %>%
    dplyr::ungroup()
  return(list(lambdas_summary = lambdas_summary, lambdas_sim = lambdas))
}
sims_posterior <- function(x_j, n, n_sims = 200){
  if(length(n) < 2){
    theta_sims <- runif(n_sims)
    return(list(theta = theta_sims, tau = NA))
  }
  a_gamma <- (length(n) - 1) / 2
  b_gamma <- 1/ 2 * (sum(x_j ^ 2 / n) - sum(x_j) ^ 2 / sum(n))
  b_gamma <- ifelse(b_gamma == 0, 0.05, b_gamma)
  tau_sims <- rgamma(n_sims, shape = a_gamma, rate = b_gamma)
  mean_normal <- sum(x_j) / sum(n)
  desv_normal <- purrr::map_dbl(tau_sims, ~ sqrt(1 / (. * sum(n))))
  theta_sims <- purrr::map_dbl(desv_normal, ~truncnorm::rtruncnorm(1, mean = mean_normal, ., a = 0, b = 1))
  return(list(theta = theta_sims, tau = tau_sims))
}
