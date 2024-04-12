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

####Ratio diputados####
utils::globalVariables(c("prop_vot_nal", "n_seats_max_raw", "n_seats_maj", "candidato",
                         "n_seats_total", "n_seats_maj", "prop_adj", "resto", "rank_resto",
                         "n_seats_prop_raw", "to_distribute", "coalition", "internal_id",
                         "LISTA_NOMINAL", "multipliter", "n_votes_weighted", "median",
                         "topped", "n_seats_prop", "prop_vot_nal", "n_seats_max", "n_assign",
                         "candidate", "prop_votes", "prop_votes_aux", "is_majority"))


####Simulation of arrival times####
utils::globalVariables(c("state_abbr", "time", "huso", "lista_nominal_log",
                  "sim_time_nc", "max_time", "status_sim", "CLAVE_CASILLA",
                  "time_obs_sim"))
####
