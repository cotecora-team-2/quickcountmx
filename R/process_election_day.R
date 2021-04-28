write_results <- function(fit, file_name, team, #tot_estratos, n_estratos, tot_casillas, n_casillas,
                          path_out, path_mailbox){
  EN <- stringr::str_sub(file_name, 10, 11)
  R <- stringr::str_sub(file_name, 12, 17)

  tab_candidatos <- fit$estimates %>%
    dplyr::mutate(across(where(is.numeric), ~. * 100)) %>%
    dplyr::mutate(across(where(is.numeric), round, 1)) %>%
    dplyr::arrange(desc(median)) %>% dplyr::select(party,median,inf,sup) %>% filter(party != "OTROS") %>%
    tibble::column_to_rownames(var="party") %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(LMU, value, -rowname) %>%
    tidyr::spread(rowname, value) %>% dplyr::rename(PART = part) %>% dplyr::mutate(LMU = dplyr::case_when(
      LMU == "inf" ~ 0,
      LMU == "median" ~ 1,
      LMU == "sup" ~ 2
    ),
    LMU = as.integer(LMU),
    EQ = team,
    EN = EN,
    R = R ) %>%
    relocate(c(EQ,EN,R), .before = everything()) %>%
    relocate(c(PART,LMU), .after = last_col())

#  tab_compulsados <- tab_candidatos %>%
#    mutate(ESTRATOS = ifelse(LMU == 0,tot_estratos,""),
#           EST_REC = ifelse(LMU == 0,n_estratos,""),
#           TOT_CAS = ifelse(LMU == 0,tot_casillas,""),
#           CAS_REC = ifelse(LMU == 0,n_casillas,""),
#           PORCENTAJE = ifelse(LMU == 0,round(n_casillas/tot_casillas, digits = 2),""))



  readr::write_csv(tab_candidatos, paste0(path_out, "/", team,
                                               EN, R, ".csv"))
  readr::write_csv(tab_candidatos, paste0(path_mailbox, "/", team,
                                          EN, R, ".csv"))
  #  readr::write_csv(tab_compulsados, file = paste0(path_results, "/", "compulsado",
#                                                 EN, R, ".csv"))
}
#' @param path_name Path to a file that will be used for estimation. On election
#' day it will be a file with a subset of the sample.
#' @param file_name Name of the file with the data.
#' @param path_out Path to directory where partial results will be
#' saved.
#' @param team Name of team running the model, to be used in INE reports.
#' @inheritParams hb_estimation
#'
#' @rdname process_batch_election_day
#' @export
process_batch <- function(path_name, file_name, path_out, path_mailbox,
                          team = "default", n_iter = 300, n_chains = 4,
                          n_warmup = 200, adapt_delta = 0.80, max_treedepth = 10, seed=221285){
  print(team)
  tipo <- stringr::str_sub(file_name, 8, 9)
  estado_str <- stringr::str_sub(file_name, 10, 11)

  table_frame <- readr::read_rds("data-raw/marco_2021.rds")
  table_frame <- table_frame %>%
    ungroup() %>%
    mutate(ln = ifelse(LISTA_NOMINAL_CASILLA==0, 1200, LISTA_NOMINAL_CASILLA)) %>%
    filter(ID_ESTADO == as.numeric(estado_str)) %>%
    mutate(CLAVE_CASILLA = gsub("'","",CLAVE_CASILLA))

  candidatos <- readr::read_csv("data-raw/estados_candidatos_partidos_2021.csv") %>%
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
    filter(!is.na(TOTAL))
  print(paste0("datos: ", path_name))
  print(paste0("salidas: ", path_out))
  # do processing ########
  muestra_m <- left_join(data_in, table_frame, by=c("CLAVE_CASILLA")) %>%
    mutate(estrato = as.character(estrato))
  data_stratum_tbl <- table_frame %>%
    filter(ID_ESTADO==as.numeric(estado_str)) %>%  count(estrato) %>%
    mutate(estrato = as.character(estrato))

#  tot_estratos <- nrow(data_stratum_tbl)
#  n_estratos <- muestra_m %>% select(estrato) %>% unique() %>% nrow()
#  tot_casillas <- table_frame %>% nrow()
#  n_casillas <- data_in %>% nrow()

  # run model ###################
  fit_time <- system.time(
    fit <- hb_estimation(muestra_m, stratum = estrato, id_station = no_casilla,
                          sampling_frame = table_frame,
                          parties = all_of(lista_candidatos),
                          covariates = comp_marg_imp, num_iter = as.numeric(n_iter),
                         chains = as.numeric(n_chains), seed = as.numeric(seed), part = TRUE)
  )
  print(fit_time)

  write_results(fit = fit, file_name = file_name,
                team = team, #tot_estratos = tot_estratos, n_estratos = n_estratos,
                #tot_casillas, n_casillas,
                path_out = path_out, path_mailbox = path_mailbox)
}
