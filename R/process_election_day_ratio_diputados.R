write_results_ratio_diputados <- function(df, file_name, team, path_out, path_mailbox){
  EN <- "40"
  R <- stringr::str_sub(file_name, 12, 17)

  tab_seats_candidatos <- df |>
    dplyr::arrange(desc(n_seats_total_median)) |>
    dplyr::select(party, contains("n_seats")) |>
    filter(party != "NULOS", party != "part", party != "CNR") |>
    mutate(inf = floor(n_seats_total_inf)) |>
    mutate(sup = ceiling(n_seats_total_sup)) |>
    mutate(median = round(n_seats_total_median)) |>
    select(-contains("n_seats")) |>
    tibble::column_to_rownames(var="party") |>
    tibble::rownames_to_column() |>
    tidyr::gather(LMU, value, -rowname) |>
    tidyr::spread(rowname, value) |>
    dplyr::mutate(LMU = dplyr::case_when(
      LMU == "inf" ~ 0,
      LMU == "median" ~ 1,
      LMU == "sup" ~ 2
    ),
    LMU = as.integer(LMU),
    EQ = team,
    EN = EN,
    R = R ) |>
    relocate(c(EQ,EN,R), .before = everything()) |>
    relocate(c(LMU), .after = last_col())

  # path_out must be set to opt/cotecora
  path_seats <- paste0(path_out, "/buzon_diputados_escano/equipo2dip")
  if(dir.exists(path_seats)) {
    readr::write_csv(tab_seats_candidatos, paste0(path_seats, "/", team, 'dip',
                                                  EN, R, ".csv"))
  }

  readr::write_csv(tab_seats_candidatos, paste0(path_mailbox, "/", team, 'dip',
                                                EN, R, ".csv"))

  tab_prop_candidatos <- df |>
    dplyr::arrange(desc(prop_median)) |>
    dplyr::select(party, contains("prop")) |>
    mutate(party = ifelse(party == "part", "PART", party)) %>%
    filter(party != "NULOS", party != "CNR") |>
    mutate(inf = round(100 * prop_inf, 2)) |>
    mutate(sup = round(100 * prop_sup, 2)) |>
    mutate(prop = round(100 * prop_median, 2)) |>
    select(-contains("prop_")) |>
    tibble::column_to_rownames(var="party") |>
    tibble::rownames_to_column() |>
    tidyr::gather(LMU, value, -rowname) |>
    tidyr::spread(rowname, value) |>
    dplyr::mutate(LMU = dplyr::case_when(
      LMU == "inf" ~ 0,
      LMU == "prop" ~ 1,
      LMU == "sup" ~ 2
    ),
    LMU = as.integer(LMU),
    EQ = team,
    EN = EN,
    R = R ) |>
    relocate(c(EQ,EN,R), .before = everything()) |>
    relocate(c(PART, LMU), .after = last_col())

  path_percent <- paste0(path_out, "/buzon_diputados/equipo2")
  if(dir.exists(path_percent)) {
    readr::write_csv(tab_prop_candidatos, paste0(path_percent, "/", team,
                                                 EN, R, ".csv"))
  }

  readr::write_csv(tab_prop_candidatos, paste0(path_mailbox, "/", team,
                                               EN, R, ".csv"))
}

#' Automatically process batch of new data, and write estimates in correct
#' form for INE systems
#'
#' @param path_name Path to a file that will be used for estimation. On election
#' day it will be a file with a subset of the sample.
#' @param file_name Name of the file with the data.
#' @param path_out Path to directory where partial results will be
#' saved.
#' @param path_mailbox Additional path to directory where partial results will be
#' saved.
#' @param team Name of team running the model, to be used in INE reports.
#' @inheritParams ratio_estimation

#'
#' @rdname process_batch_election_day
#' @export
ratio_diputados_process_batch <- function(path_name, file_name, path_out, path_mailbox, B,
                          team = "default"){
  print(team)
  tipo <- stringr::str_sub(file_name, 8, 9)

  assignment_tbl <- readr::read_csv("data-raw/assignment_tbl_2024.csv") |>
    mutate(strata = paste(ID_ENTIDAD, DISTRITO, sep = "-"))

  coalitions_tbl <- readr::read_csv("data-raw/coalitions_tbl_2024.csv")

  stratum_tbl <- readr::read_csv("data-raw/statum_size_nal_2024.csv")

  data_in <- readr::read_delim(path_name, "|", escape_double = FALSE,
                               trim_ws = TRUE, skip = 1) |>
    mutate(strata = paste(ID_ESTADO, ID_DISTRITO_FEDERAL, sep = "-")) |>
    filter(TOTAL > 0)

  print(paste0("datos: ", path_name))
  print(paste0("salidas: ", path_out))


  # run model ###################
  fit_time <- system.time(
    estimates <- ratio_estimation_diputados(data_in, stratum = strata,
                                         stratum_tbl = stratum_tbl, n_stratum = N_estrato,
                                         coalitions_tbl = coalitions_tbl,
                                         assignment_tbl = assignment_tbl,
                                         B = as.numeric(B))
  )
  print(fit_time)
  print(estimates)

  write_results_ratio_diputados(df = estimates, file_name = file_name,
                team = team, path_out = path_out, path_mailbox = path_mailbox)
}
