write_results_ratio <- function(df, file_name, team, n_muestra, #tot_estratos, n_estratos, tot_casillas, n_casillas,
                                path_out){
  EN <- stringr::str_sub(file_name, 10, 11)
  R <- stringr::str_sub(file_name, 12, 17)

  tab_candidatos <- df %>%
    dplyr::arrange(desc(prop)) %>% dplyr::select(party,prop,std_error) %>% filter(party != "OTROS") %>%
    mutate(inf = prop - stats::qt(0.025, n_muestra - 1) * std_error) %>%
    mutate(sup = prop + stats::qt(0.025, n_muestra - 1) * std_error) %>%
    select(-std_error) %>%
    dplyr::mutate(across(where(is.numeric), round, 1)) %>%
    tibble::column_to_rownames(var="party") %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(LMU, value, -rowname) %>%
    tidyr::spread(rowname, value) %>% dplyr::mutate(LMU = dplyr::case_when(
      LMU == "inf" ~ 0,
      LMU == "prop" ~ 1,
      LMU == "sup" ~ 2
    ),
    LMU = as.integer(LMU),
    EQ = team,
    EN = EN,
    R = R ) %>%
    relocate(c(EQ,EN,R), .before = everything()) %>%
    relocate(c(LMU), .after = last_col())


  #  tab_compulsados <- tab_candidatos %>%
  #    mutate(ESTRATOS = ifelse(LMU == 0,tot_estratos,""),
  #           EST_REC = ifelse(LMU == 0,n_estratos,""),
  #           TOT_CAS = ifelse(LMU == 0,tot_casillas,""),
  #           CAS_REC = ifelse(LMU == 0,n_casillas,""),
  #           PORCENTAJE = ifelse(LMU == 0,round(n_casillas/tot_casillas, digits = 2),""))



  readr::write_csv(tab_candidatos, paste0(path_out, "/", 'razon',
                                          EN, R, ".csv"))
  #  readr::write_csv(tab_compulsados, file = paste0(path_results, "/", "compulsado",
  #                                                 EN, R, ".csv"))
}

#' Automatically process batch of new data, and write estimates in correct
#' form for INE systems
#'
#' @param path_name Path to a file that will be used for estimation. On election
#' day it will be a file with a subset of the sample.
#' @param file_name Name of the file with the data.
#' @param path_out Path to directory where partial results will be
#' saved.
#' @param team Name of team running the model, to be used in INE reports.
#' @inheritParams ratio_estimation

#'
#' @rdname process_batch_election_day
#' @export
ratio_process_batch <- function(path_name, file_name, path_out, B,
                                team = "default"){
  print(team)
  tipo <- stringr::str_sub(file_name, 8, 9)
  estado_str <- stringr::str_sub(file_name, 10, 11)

  table_frame <- readr::read_rds("data-raw/marco_2023.rds")
  table_frame <- table_frame %>%
    ungroup() %>%
    mutate(ln = ifelse(LISTA_NOMINAL==0, 1000, LISTA_NOMINAL)) %>%
    filter(ID_ESTADO == as.numeric(estado_str)) %>%
    mutate(CLAVE_CASILLA = gsub("'","",CLAVE_CASILLA))

  candidatos <- readr::read_csv("data-raw/estados_candidatos_partidos_2023.csv") %>%
    filter(ID_ESTADO == as.numeric(estado_str))
  lista_candidatos <- candidatos$CANDIDATO %>% unique()

  data_in <- readr::read_delim(path_name, "|", escape_double = FALSE,
                               trim_ws = TRUE, skip = 1) %>%
    #    mutate(ID_ESTADO = iD_ESTADO) %>%
    mutate(OTROS = CNR + NULOS) %>%
    mutate(CLAVE_CASILLA = paste0(stringr::str_pad(ID_ESTADO, 2, pad = "0"),
                                  stringr::str_pad(SECCION, 4, pad = "0"),
                                  TIPO_CASILLA,
                                  stringr::str_pad(ID_CASILLA, 2, pad = "0"),
                                  stringr::str_pad(EXT_CONTIGUA,2,pad="0"))) %>%
    filter(TOTAL > 0) |>
    select(-c(ID_ESTADO, ID_DISTRITO_FEDERAL,
              SECCION, TIPO_CASILLA, EXT_CONTIGUA, TIPO_SECCION,
              LISTA_NOMINAL, ID_MUNICIPIO, ID_DIST_LOC, ID_ESTRATO))
  print(paste0("datos: ", path_name))
  print(paste0("salidas: ", path_out))
  # do processing ########
  muestra_m <- left_join(data_in, table_frame, by = "CLAVE_CASILLA") %>%
    mutate(estrato = as.character(estrato))
  data_stratum_tbl <- table_frame %>%
    filter(ID_ESTADO==as.numeric(estado_str)) %>%  count(estrato) %>%
    mutate(estrato = as.character(estrato))

  #tot_estratos <- nrow(data_stratum_tbl)
  #n_estratos <- muestra_m %>% select(estrato) %>% unique() %>% nrow()
  #tot_casillas <- table_frame %>% nrow()
  #n_casillas <- data_in %>% nrow()

  # run model ###################
  fit_time <- system.time(
    ratios <- ratio_estimation(muestra_m, stratum = estrato, n_stratum = n,
                               data_stratum = data_stratum_tbl,
                               parties = all_of(lista_candidatos), B = as.numeric(B))
  )
  print(fit_time)
  print(ratios)

  n_muestra_m <- nrow(muestra_m)

  write_results_ratio(df = ratios, file_name = file_name,
                      team = team, n_muestra = n_muestra_m, #tot_estratos = tot_estratos, n_estratos = n_estratos,
                      #tot_casillas, n_casillas,
                      path_out = path_out)
}
