#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
## usethis namespace: end
NULL


####Ratio and hb estimation####
utils::globalVariables(c("strata", "n_strata", "n_h", "n_votes", "party", "n_aux",
                  "prop", "party", "total_votes", "strata", "n_strata", "n_observed", "strata_num",
                  "value", "id_station", "participacion"))

####Simulation of arrival times####
utils::globalVariables(c("state_abbr", "time", "huso", "lista_nominal_log",
                  "sim_time_nc", "max_time", "status_sim", "CLAVE_CASILLA",
                  "time_obs_sim"))
