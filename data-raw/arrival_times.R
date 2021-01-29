library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(usethis)
data("conteo_2018")

estados_tbl <- read_csv("./data-raw/df_mxstate.csv")
muestra_selec <- read_csv("./data-raw/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv") %>%
  mutate(CLAVE_CASILLA = paste0(str_sub(ID, 2, 3), str_sub(ID, 6, -1)))

# muestra obtenida por hora de llegada, calcular huso
# Clave casilla: ID_ESTADO-SECCION-TIPO_CASILLA-ID_CASILLA-EXT_CONTIGUA
remesas <- read_delim("./data-raw/REMESAS0100020000.txt",
                      delim = "|", skip = 1) %>%
  mutate(timestamp = lubridate::ymd_hms(paste(ANIO, MES, DIA, HORA, MINUTOS, SEGUNDOS, sep = "-"),
                             tz = "America/Mexico_City")) %>%
  mutate(ID_ESTADO = str_pad(iD_ESTADO, 2, pad = "0"),
         SECCION = str_pad(SECCION,4, pad = "0"),
         TIPO_CASILLA = ifelse(TIPO_CASILLA == "MEC", "M", TIPO_CASILLA),
         ID_CASILLA = str_pad(ID_CASILLA, 2, pad = "0"),
         EXT_CONTIGUA = str_pad(EXT_CONTIGUA, 2, pad = "0")) %>%
  mutate(CLAVE_CASILLA = paste0(ID_ESTADO, SECCION, TIPO_CASILLA, ID_CASILLA, EXT_CONTIGUA))

## procesar
datos_muestra <- muestra_selec %>%
  left_join(conteo_2018 %>%
              select(CLAVE_CASILLA, LISTA_NOMINAL_CASILLA, AMLO:JAMK,
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
estratos_nal <- conteo_2018 %>%
  group_by(state_abbr, estrato) %>% count() %>%
  ungroup %>% select(-state_abbr)

muestra_tot <-
  left_join(
    datos_muestra %>% select(CLAVE_CASILLA, LISTA_NOMINAL, TIPO_SECCION,
                             ID_ESTRATO_F, ID_AREA_RESPONSABILIDAD, state_abbr,
                             TOTAL_VOTOS_CALCULADOS, tipo_casilla, lista_nominal_log,
                             ln_log_c,
                             tipo_seccion, LISTA_NOMINAL, huso, AMLO:JAMK, TVIVHAB:.fittedPC5),
    remesas %>% select(!c(TIPO_SECCION, TIPO_CASILLA, RAC:JHRC)),
    by = c("CLAVE_CASILLA", "LISTA_NOMINAL")) %>%
  mutate(llegada = ifelse(is.na(TOTAL), 0, 1)) # llegó antes de las 12:00 ?


# Construir datos de llegadas
arrivals_tbl <- muestra_tot %>%
  select(CLAVE_CASILLA, timestamp, huso, llegada, state_abbr, tipo_casilla,
         tipo_seccion, TVIVPARHAB, VPH_INTER, VPH_PISOTI, VPH_LAVAD, VPH_REFRI,
         VPH_CEL, .fittedPC1:.fittedPC5,
         RAC, JAMK, AMLO,
         lista_nominal_log, ln_log_c,
         TOTAL_VOTOS_CALCULADOS,
         LISTA_NOMINAL,
         TOTAL, ID_ESTADO) %>%
  mutate(timestamp = if_else(is.na(timestamp),
                             lubridate::ymd_hms("2018-07-01 23:59:59", tz = "America/Mexico_City"), timestamp)) %>%
  mutate(timestamp = lubridate::with_tz(timestamp, "America/Mexico_City")) %>%
  mutate(time = difftime(timestamp,
                           lubridate::ymd_hms("2018-07-01 18:30:00", tz ="America/Mexico_City"),
                           units = "hours")) %>%
  mutate(status = llegada)


usethis::use_data(arrivals_tbl, overwrite = TRUE)
