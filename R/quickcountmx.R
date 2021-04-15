#' quickcountmx
#'
#' @description
#' Functions to simulate and estimate Mexican election results based on a simple or stratified random sample, the functions were used, among other methodologies, to anticipate the final results of the 2021 Mexican elections.
#'
#' @import dplyr
#'
#' @docType package
#' @name quickcountmx
NULL


####Ratio estimation####
globalVariables(c("strata", "n_strata", "n_h", "n_votes", "party", "n_aux",
                  "prop", "party", "total_votes", "strata", "n_strata", "n_observed", "strata_num",
                  "value", "id_station"))

####Simulation of arrival times####
globalVariables(c("state_abbr", "time", "huso", "lista_nominal_log",
                  "sim_time_nc", "max_time", "status_sim", "CLAVE_CASILLA",
                  "time_obs_sim"))
