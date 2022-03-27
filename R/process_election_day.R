write_results <- function(fit, file_name, team,
                          path_out, path_mailbox, prop_obs){
  EN <- stringr::str_sub(file_name, 10, 11)
  R <- stringr::str_sub(file_name, 12, 17)

  tab_candidatos <- fit$estimates %>%
    dplyr::mutate(across(where(is.numeric), ~. * 100)) %>%
    dplyr::mutate(across(where(is.numeric), round, 2)) %>%
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

  prop_obs_str <- format(prop_obs,digits=3)
  tab_pctpropobs <- data.frame("EN"=c(EN), "R"=c(R), "pctpropobs"=c(as.numeric(prop_obs_str)*100))
  readr::write_csv(tab_candidatos, paste0(path_out, "/", team,
                                          EN, R, ".csv"))
  readr::write_csv(tab_candidatos, paste0(path_mailbox, "/", team,
                                          EN, R, ".csv"))
  p <- stringr::str_split(path_mailbox, "/", simplify = TRUE)
  l <- length(p)-2
  npath_mailbox <- paste(p[1:l],collapse='/')

  readr::write_csv(tab_pctpropobs, paste0(npath_mailbox, "/pctpropobs/", "pctpropobs",EN,".csv"),
                   append = TRUE, col_names = FALSE)

  row1 <- paste(stringr::str_pad(names(tab_candidatos),7,pad=" "),collapse = " ")
  row2 <- paste(stringr::str_pad(as.character(tab_candidatos[1,]),7,pad = " "), collapse = " ")
  row3 <- paste(stringr::str_pad(as.character(tab_candidatos[2,]),7,pad = " "), collapse = " ")
  row4 <- paste(stringr::str_pad(as.character(tab_candidatos[3,]),7,pad = " "), collapse = " ")
  logger::log_trace("prop_obs: {logger::colorize_by_log_level(prop_obs_str,logger::SUCCESS)}")
  logger::log_trace("{logger::grayscale_by_log_level(row1,logger::FATAL)}")
  logger::log_trace("{logger::grayscale_by_log_level(row2,logger::ERROR)}")
  logger::log_trace("{logger::grayscale_by_log_level(row3,logger::ERROR)}")
  logger::log_trace("{logger::grayscale_by_log_level(row4,logger::ERROR)}")

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
#' @param log_file Path to logfile of process
#' @param even skipping of batches
#' @param path_mailbox path to mailbox
#' @param n_iter number of stan sampling iterations
#' @param n_warmup numer of stan warmup iterations
#' @param n_chains number of stan chains
#' @inheritParams hb_estimation
#'
#' @rdname process_batch_election_day
#' @export
process_batch <- function(path_name, file_name, log_file, path_out, path_mailbox,
                          team = "default", even="0", n_iter = 100, n_chains = 8,
                          n_warmup = 180, adapt_delta = 0.80, max_treedepth = 10,
                          nominal_max = 3050, seed=221285){
  logger::log_appender(logger::appender_file(log_file))
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::TRACE)
  logger::log_info(team)
  tipo <- stringr::str_sub(file_name, 8, 9)
  estado_str <- stringr::str_sub(file_name, 10, 11)

  table_frame <- readr::read_csv("data-raw/marco_revocacion.csv") |>
    mutate(ln = ifelse(LISTA_NOMINAL==0, as.numeric(nominal_max), LISTA_NOMINAL))

  data_in <- readr::read_delim(path_name, "|", escape_double = FALSE,
                               trim_ws = TRUE, skip = 1) |>
    mutate(CLAVE_CASILLA = paste0(stringr::str_pad(ID_ESTADO, 2, pad = "0"),
                                  stringr::str_pad(SECCION, 4, pad = "0"),
                                  TIPO_CASILLA,
                                  stringr::str_pad(ID_CASILLA, 2, pad = "0"),
                                  stringr::str_pad(EXT_CONTIGUA,2,pad="0")))
  ################# fix id station #######################
  #table_frame <- table_frame  |>
  #  mutate(CLAVE_CASILLA = paste0(CLAVE_CASILLA, stringr::str_pad(NUMERO_ARE, 2, pad = "0")))
  #data_in <- data_in |>
  #  mutate(CLAVE_CASILLA = paste0(CLAVE_CASILLA, stringr::str_pad(NUMERO_ARE, 2, pad = "0")))
  ########################################################
  logger::log_info(paste0("numero de casillas leidas: ",data_in %>% nrow()))
  logger::log_info(paste0("datos: ", path_name))
  logger::log_info(paste0("salidas: ", path_out))

  lista_opciones <- c("REVOQUE", "SIGA", "NULOS")


  data_in <- data_in |> select(any_of(c("CLAVE_CASILLA", lista_opciones)))
  ## check out of frame
  casillas_fuera_marco_tbl <- anti_join(data_in |> select(CLAVE_CASILLA),
                                    table_frame |> select(CLAVE_CASILLA),
                                    by = "CLAVE_CASILLA")
  if(nrow(casillas_fuera_marco_tbl) > 0){
    logger::log_warn("Se encontraron {nrow(casillas_fuera_marco_tbl)} que no están en marco.")
    readr::write_csv(casillas_fuera_marco_tbl, "casillas_fuera_marco.csv")
    data_in <- data_in |> semi_join(table_frame |> select(CLAVE_CASILLA),
                                   by = "CLAVE_CASILLA")
  }
  # do processing ########
  muestra_m <- left_join(data_in, table_frame, by=c("CLAVE_CASILLA")) %>%
    mutate(estrato = as.character(estrato))
  data_stratum_tbl <- table_frame %>%
    count(estrato) %>%
    mutate(estrato = as.character(estrato))
  n_muestra_m <- muestra_m %>% nrow()
  logger::log_info(paste0("numero de casillas despues de union con marco: ", n_muestra_m))

  prop_obs <- n_muestra_m / 1820

  # run model ###################
  fit_time <- system.time(
    fit <- hb_estimation(muestra_m, stratum = estrato, id_station = id_casilla,
                         sampling_frame = table_frame,
                         parties = all_of(lista_opciones), prop_obs = 0.9 * prop_obs,
                         covariates = all_of(c("x")), num_iter = as.numeric(n_iter),
                         chains = as.numeric(n_chains), seed = as.numeric(seed),
                         adapt_delta = as.numeric(adapt_delta),
                         max_treedepth = as.numeric(max_treedepth),
                         num_warmup = as.numeric(n_warmup),
                         model = "consulta", nominal_max = as.numeric(nominal_max),
                         part = TRUE)
  )
  print(fit_time)
  if(even=="0") m <- 1
  else m <- 2
  if(fit_time[3] < 230 * m) {
    logger::log_info("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::SUCCESS)}")
  }
  else if(fit_time[3] < 290 * m) {
    logger::log_warn("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::WARN)}")
  }
  else{
    logger::log_fatal("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::FATAL)}")
  }

  write_results(fit = fit, file_name = file_name,
                team = team,
                path_out = path_out, path_mailbox = path_mailbox, prop_obs = prop_obs)
}
