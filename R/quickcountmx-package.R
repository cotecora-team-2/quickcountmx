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

utils::globalVariables(c(".draw", "CANDIDATO", "CNR", "EQ", "EXT_CONTIGUA",
                         "ID_CASILLA", "ID_ESTADO",
                         "LISTA_NOMINAL", "LISTA_NOMINAL_CASILLA", "LMU",
                         "NULOS", "PART", "SECCION",
                         "TIPO_CASILLA", "TOTAL", "cand_id", "data",
                         "estrato", "head", "inf", "internal_id", "median",
                         "no_casilla", "part", "pivot_longer", "prop_votes",
                         "quantile", "rowname", "std_error",
                         "sup", "total", "total_ln_str", "total_nominal",
                         "total_votes_str", "votes", "votos"))
