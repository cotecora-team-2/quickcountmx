write_results <- function(fit, file_name, team, #tot_estratos, n_estratos, tot_casillas, n_casillas,
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
  #tab_pctpropobs <- data.frame("EN"=c(EN), "R"=c(R), "pctpropobs"=c(as.numeric(prop_obs_str)*100))
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
  p <- stringr::str_split(path_mailbox, "/", simplify = TRUE)
  l <- length(p)-2
  #npath_mailbox <- paste(p[1:l],collapse='/')

  #readr::write_csv(tab_pctpropobs, paste0(npath_mailbox, "/pctpropobs/", "pctpropobs",EN,".csv"),
  #           append = TRUE, col_names = FALSE)

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
#' @param nominal_max maximum number of votes in special stations
#' @param seed random seed 
#' @inheritParams hb_estimation
#'
#' @rdname process_batch_election_day
#' @export
process_batch <- function(path_name, file_name, log_file, path_out, path_mailbox,
                          team = "default", even="0", n_iter = 300, n_chains = 4,
                          n_warmup = 200, adapt_delta = 0.80, max_treedepth = 10, nominal_max = 1200, seed=221285){
  logger::log_appender(logger::appender_file(log_file))
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::TRACE)
  logger::log_info(team)
  tipo <- stringr::str_sub(file_name, 8, 9)
  estado_str <- stringr::str_sub(file_name, 10, 11)

  table_frame <- readr::read_rds("data-raw/marco_2022.rds")
  table_frame <- table_frame |>
    ungroup() |>
    mutate(ln = ifelse(LISTA_NOMINAL==0, as.numeric(nominal_max), LISTA_NOMINAL)) |>
    filter(ID_ESTADO == as.numeric(estado_str)) |>
    mutate(CLAVE_CASILLA = gsub("'","",CLAVE_CASILLA))

  candidatos <- readr::read_csv("data-raw/estados_candidatos_partidos_2022.csv") |>
    filter(ID_ESTADO == as.numeric(estado_str)) #%>%
#    filter(!grepl("IC",CANDIDATO)) #quita candidatos independientes
  lista_candidatos <- candidatos$CANDIDATO %>% unique()

  data_in <- readr::read_delim(path_name, "|", escape_double = FALSE,
                               trim_ws = TRUE, skip = 1) %>%
#    rename(ID_ESTADO = iD_ESTADO) %>% #cambia nombre de columna iD_ESTADO a mayusculas
    mutate(OTROS = CNR + NULOS) %>%
    mutate(CLAVE_CASILLA = paste0(stringr::str_pad(ID_ESTADO, 2, pad = "0"),
                                  stringr::str_pad(SECCION, 4, pad = "0"),
                                  TIPO_CASILLA,
                                  stringr::str_pad(ID_CASILLA, 2, pad = "0"),
                                  stringr::str_pad(EXT_CONTIGUA,2, pad = "0"))) 
  logger::log_info(paste0("numero de casillas con TOTAL mayor que cero: ",data_in %>% nrow()))
  logger::log_info(paste0("datos: ", path_name))
  logger::log_info(paste0("salidas: ", path_out))

  #check if candidates votes correspond to their alliances sums
  lista_coaliciones <- candidatos$PARTIDO %>% unique()
  votacion_larga <- data_in %>%
    tidyr::pivot_longer(cols = all_of(lista_coaliciones), names_to = "PARTIDO",
                 values_to = "votos")
  votacion <- votacion_larga %>%
    left_join(candidatos) %>%
    group_by(CLAVE_CASILLA, ID_ESTADO, CANDIDATO) %>%
    summarise(votos = sum(votos)) %>%
    tidyr::pivot_wider(names_from = CANDIDATO, values_from = votos)

  rm(votacion_larga)
  rm(lista_coaliciones)
  tmp_data_in <- data_in %>%
    select(CLAVE_CASILLA, ID_ESTADO,all_of(lista_candidatos))
  tmp_data_in <- tmp_data_in[order(data_in$CLAVE_CASILLA),]

  tmp_votacion <- votacion %>%
    select(CLAVE_CASILLA, ID_ESTADO,all_of(lista_candidatos))
  tmp_votacion <- tmp_votacion[order(votacion$CLAVE_CASILLA),]
  print(all(tmp_data_in == tmp_votacion))
  if(!all(tmp_data_in == tmp_votacion)){
      erroneos <- data.frame(cand_id = which(tmp_data_in != tmp_votacion, arr.ind=TRUE)[,2] %>% as.numeric())
      erroneos <- erroneos %>% group_by(cand_id) %>% summarise(c = n())
      erroneos <- erroneos %>%
                  mutate_at(vars(cand_id), ~ names(tmp_data_in)[.x])
      print(erroneos)
      for(ro in 1:nrow(erroneos)) {print(paste0("candidato erroneo: ",erroneos[ro,1]))}
      for(ro in 1:nrow(erroneos)) {logger::log_error('La suma del candidato {logger::colorize_by_log_level(erroneos[ro,1], logger::ERROR)} esta incorrecta en {logger::colorize_by_log_level(erroneos[ro,2], logger::ERROR)} casillas!')}
  }
  rm(votacion)
  rm(tmp_votacion)
  rm(tmp_data_in)

  # do processing ########
  muestra_m <- left_join(data_in, table_frame, by=c("CLAVE_CASILLA")) %>%
    mutate(estrato = as.character(estrato))
  data_stratum_tbl <- table_frame %>%
    filter(ID_ESTADO==as.numeric(estado_str)) %>%  count(estrato) %>%
    mutate(estrato = as.character(estrato))
  n_muestra_m <- muestra_m %>% nrow()
  logger::log_info(paste0("numero de casillas despues de union con marco: ", n_muestra_m))

#  tot_estratos <- nrow(data_stratum_tbl)
#  n_estratos <- muestra_m %>% select(estrato) %>% unique() %>% nrow()
#  tot_casillas <- table_frame %>% nrow()
#  n_casillas <- data_in %>% nrow()

  n_t_muestra <- readr::read_csv("data-raw/estados_n_muestra.csv") %>%
    filter(ID_ESTADO == as.numeric(estado_str))
  prop_obs <- ifelse(n_muestra_m/n_t_muestra$n >= 1.0, 0.999, n_muestra_m/n_t_muestra$n)

  # run model ###################
  fit_time <- system.time(
    fit <- hb_estimation(muestra_m, stratum = estrato, id_station = no_casilla,
                          sampling_frame = table_frame,
                          parties = all_of(lista_candidatos), prop_obs = prop_obs,
                          model = "mlogit-corr",
                          covariates = all_of(c("seccion_no_urbana")), num_iter = as.numeric(n_iter),
                          nominal_max = as.numeric(nominal_max),
                         chains = as.numeric(n_chains), seed = as.numeric(seed))
  )
  print(fit_time)
  if(even == "0") m <- 1
  else m <- 2
  if(fit_time[3] < 230 * m){
      logger::log_info("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::SUCCESS)}")
  }
  else if(fit_time[3] < 290*m){
    logger::log_warn("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::WARN)}")
  }
  else{
    logger::log_fatal("elapsed time: {logger::colorize_by_log_level(fit_time[3],logger::FATAL)}")
  }

  write_results(fit = fit, file_name = file_name,
                team = team, #tot_estratos = tot_estratos, n_estratos = n_estratos,
                #tot_casillas, n_casillas,
                path_out = path_out, path_mailbox = path_mailbox, prop_obs = prop_obs)
}
