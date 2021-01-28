
estimate_arrival_model <- function(){

}

preprocess_arrival <- function(){
  datos_muestra <- muestra_selec %>%
    left_join(conteo %>%
                select(CLAVE_CASILLA, LISTA_NOMINAL_CASILLA, AMLO_1:JAMK_1,
                       TOTAL_VOTOS_CALCULADOS, lista_nominal_log, ln_log_c,
                       huso, tipo_casilla,
                       estrato, tipo_seccion, TVIVHAB:.fittedPC5) %>%
                rename(LISTA_NOMINAL = LISTA_NOMINAL_CASILLA),
              by = c("CLAVE_CASILLA", "LISTA_NOMINAL"))

  # casillas de la muestra con faltantes:
  datos_muestra %>% group_by(is.na(TOTAL_VOTOS_CALCULADOS)) %>%
    count() ## 33 casillas
  datos_muestra <- datos_muestra %>%
    filter(!is.na(TOTAL_VOTOS_CALCULADOS)) %>%
    left_join(estados_tbl %>% mutate(iD_ESTADO = region))
  ## asignación muestra_selec
  estratos_nal <- conteo %>%
    group_by(state_abbr, estrato) %>% count() %>%
    ungroup %>% select(-state_abbr)


  # muestra obtenida por hora de llegada, calcular huso
  # Clave casilla: ID_ESTADO-SECCION-TIPO_CASILLA-ID_CASILLA-EXT_CONTIGUA
  remesas <- read_delim("../datos/remesas_nal/remesas_nal/REMESAS0100020000.txt",
                        delim = "|", skip = 1) %>%
    mutate(timestamp = ymd_hms(paste(ANIO, MES, DIA, HORA, MINUTOS, SEGUNDOS, sep = "-"),
                               tz = "America/Mexico_City")) %>%
    mutate(ID_ESTADO = str_pad(iD_ESTADO, 2, pad = "0"),
           SECCION = str_pad(SECCION,4, pad = "0"),
           TIPO_CASILLA = ifelse(TIPO_CASILLA == "MEC", "M", TIPO_CASILLA),
           ID_CASILLA = str_pad(ID_CASILLA, 2, pad = "0"),
           EXT_CONTIGUA = str_pad(EXT_CONTIGUA, 2, pad = "0")) %>%
    mutate(CLAVE_CASILLA = paste0(ID_ESTADO, SECCION, TIPO_CASILLA, ID_CASILLA, EXT_CONTIGUA))

  muestra_tot <-
    left_join(
      datos_muestra %>% select(CLAVE_CASILLA, LISTA_NOMINAL, TIPO_SECCION,
                               ID_ESTRATO_F, ID_AREA_RESPONSABILIDAD, state_abbr,
                               TOTAL_VOTOS_CALCULADOS, tipo_casilla, lista_nominal_log,
                               ln_log_c,
                               tipo_seccion, LISTA_NOMINAL, huso, AMLO_1:JAMK_1, TVIVHAB:.fittedPC5),
      remesas %>% select(-TIPO_SECCION, - TIPO_CASILLA),
      by = c("CLAVE_CASILLA", "LISTA_NOMINAL")) %>%
    mutate(llegada = ifelse(is.na(TOTAL), 0, 1)) # llegó antes de las 12:00 ?


  # Construir datos de llegadas
  llegadas_tbl <- muestra_tot %>%
    select(timestamp, huso, llegada, state_abbr, tipo_casilla,
           tipo_seccion,TVIVPARHAB, VPH_INTER, VPH_PISOTI, VPH_LAVAD, VPH_REFRI,
           VPH_CEL, .fittedPC1:.fittedPC5,
           RAC_1, JAMK_1, AMLO_1,
           lista_nominal_log, ln_log_c,
           TOTAL_VOTOS_CALCULADOS,
           LISTA_NOMINAL, ID_ESTRATO_F.x, ID_AREA_RESPONSABILIDAD.x,
           TOTAL, ID_ESTADO) %>%
    mutate(timestamp = if_else(is.na(timestamp),
                               ymd_hms("2018-07-01 23:59:59", tz = "America/Mexico_City"), timestamp)) %>%
    mutate(timestamp = with_tz(timestamp, "America/Mexico_City")) %>%
    mutate(tiempo = difftime(timestamp,
                             ymd_hms("2018-07-01 18:30:00", tz ="America/Mexico_City"),
                             units = "hours")) %>%
    mutate(cae = paste0(ID_ESTRATO_F.x, ID_AREA_RESPONSABILIDAD.x)) %>%
    group_by(cae) %>%
    mutate(n_reporte = rank(timestamp)) %>%
    ungroup %>%
    group_by(state_abbr) %>%
    mutate(status = llegada)

  # Tabla para ajustar modelos
  llegadas_tbl_2 <- llegadas_tbl %>% filter(state_abbr %in% estados) %>%
    ungroup %>%
    mutate(tiempo_huso = ifelse(tiempo - huso > 0, tiempo - huso, 0.001))


}
