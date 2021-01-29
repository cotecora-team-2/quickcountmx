#' Simulation of arrival times according to model based on 2018 data
#'
#' Fit model and simulate arrival times (hours after polling stations closing).
#' This times correspond to different CST times depending on the timezone
#' of each polling station.
#' \code{fit_model} fits a survival regression model to the arrival times
#' @param arrivals_tbl \code{tibble} with the arrival data
#' @param states character vector with states for which model should
#' be fit
#' @importFrom dplyr %>%
#' @importFrom stats as.formula predict
#' @export
fit_model <- function(arrivals_tbl, states){
  arrivals_tbl_2 <- arrivals_tbl %>%
    filter(state_abbr %in% states) %>%
    ungroup %>%
    mutate(tiempo_huso = ifelse(time - huso > 0,
                                time - huso, 0.001)) %>%
    mutate(ln_log_c = lista_nominal_log - mean(lista_nominal_log))
  formula <- as.formula("survival::Surv(tiempo_huso, status) ~ 1 +
      state_abbr:ln_log_c +
      ln_log_c:tipo_casilla +
      state_abbr:I(tipo_seccion == 1) +
      tipo_casilla +
      state_abbr:.fittedPC1 +
      state_abbr:log((1+RAC)/(TOTAL_VOTOS_CALCULADOS + 1)) +
      state_abbr:log((1+AMLO)/(TOTAL_VOTOS_CALCULADOS + 1)) +
      state_abbr:log((1+JAMK)/(TOTAL_VOTOS_CALCULADOS +  1)) +
      survival::strata(state_abbr)")
  reg_arrivals <- survival::survreg(formula,
                   arrivals_tbl_2,
                   dist='loglogistic', x = TRUE,
                   control = survival::survreg.control(maxiter = 50000))
  out_list <- list()
  out_list[["model"]] <- reg_arrivals
  out_list[["formula"]] <- formula
  out_list[["data"]] <- arrivals_tbl_2
  out_list[["states"]] <- states
  return(out_list)
}

#' \code{simulate_arrivals} Simulate arrival times based on parametric bootstrap
#' of \code{model}
#' @param id id for the simulation run
#' @param model fit with function \code{fit_model}
#' @param new_data_tbl new data to simulate times (if not for the original dataset
#' for which the model was fit)
#' @param hour_censoring add censoring structure if desired certain hours after
#' polling station closing (use \code{Inf} for the exact simulated times )
#' @importFrom dplyr %>%
#' @export
simulate_arrivals <- function(id, model, new_data_tbl = NULL, hour_censoring = 5){
  data_tbl <- model[["data"]]
  if(is.null(new_data_tbl)){
    new_data_tbl <- data_tbl
  }
  reg <- model[["model"]]
  formula <- model[["formula"]]
  # simulate form original model
  mat_cuantiles <- predict(reg, newdata = data_tbl,
    type = "quantile", p = seq(0.001, 0.999, by = 0.001))
  rownames(mat_cuantiles) <- NULL
  sims_no_censoring <- apply(mat_cuantiles, 1, function(cuantiles){
    sample(cuantiles, 1)
  })
  sims_tbl <- as_tibble(data_tbl) %>%
    mutate(sim_time_nc = sims_no_censoring)
  ## parametric bootstrap
  obs_boot_1 <- sims_tbl %>%
    mutate(tiempo_huso = sim_time_nc, status = 1)
  reg_boot <- survival::survreg(formula = formula,
                      data = as.data.frame(obs_boot_1),
                      dist = 'loglogistic',
                      control = survival::survreg.control(maxiter = 5000),
                      model = TRUE, x = TRUE, y = TRUE)
  ## simulate from bootstrapped model
  mat_cuantiles <- predict(reg_boot, newdata = new_data_tbl,
                           type = "quantile", p = seq(0.001, 0.999, by = 0.001))
  rownames(mat_cuantiles) <- NULL
  sims_no_censoring <- apply(mat_cuantiles, 1, function(cuantiles){
    sample(cuantiles, 1)
  })
  # prep simulations
  sims_tbl <- as_tibble(new_data_tbl) %>%
    mutate(sim_time_nc = sims_no_censoring)  %>%
    mutate(max_time = hour_censoring) %>%
    mutate(status_sim = ifelse(sim_time_nc > max_time, 0, 1)) %>%
    ungroup %>%
    mutate(time_obs_sim = ifelse(status_sim == 0, max_time, sim_time_nc)) %>%
    select(CLAVE_CASILLA, time_obs_sim, status_sim, state_abbr) %>%
    rename(time = time_obs_sim, status = status_sim)
  sims_tbl <- sims_tbl %>% mutate(id = id)
  sims_tbl
}
