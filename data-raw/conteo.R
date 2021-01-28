library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(usethis)

estados_tbl <- read_csv("./data-raw/df_mxstate.csv")
marco_ext <- read_csv(file = "./data-raw/marco_ext.csv")
encabezado <- read_lines("./data-raw/presidencia.csv", skip = 6, n_max = 1) %>%
  str_replace("\\|\\|", "") %>%
  str_split_fixed("\\|", n = 42)

conteo_2018 <- read_delim("./data-raw/presidencia.csv", delim = "|",
                     col_names = encabezado,
                     skip = 7,
                     quote = "'", na = c("", "NA", "-")) %>%
  mutate(AMLO = MORENA + PT + `ENCUENTRO SOCIAL` + PT_MORENA +
           MORENA_PES + PT_PES + PT_MORENA_PES,
         RAC = PAN + PRD + `MOVIMIENTO CIUDADANO` + PAN_PRD + PAN_MC +
           PRD_MC + PAN_PRD_MC,
         JAMK = PRI + PVEM + `NUEVA ALIANZA` + PRI_PVEM + PRI_NA +
           PVEM_NA + PRI_PVEM_NA) %>%
  left_join(estados_tbl %>%
              rename(ID_ESTADO = region) %>%
              mutate(ID_ESTADO = as.numeric(ID_ESTADO)),
            by = "ID_ESTADO") %>%
  #filter(TIPO_CASILLA != "M") %>%
  mutate(tipo_casilla = factor(TIPO_CASILLA, levels= c("B", "C", "E", "S"))) %>%
  mutate(lista_nominal_log = log(1 + LISTA_NOMINAL_CASILLA)) %>%
  ungroup %>%
  mutate(media_ln_log = mean(lista_nominal_log, na.rm = TRUE)) %>%
  mutate(ln_log_c = lista_nominal_log - media_ln_log) %>%
  mutate(huso = case_when(state_abbr %in% c("BC", "SON") ~ 2,
                          state_abbr %in% c("CHIH", "BCS", "NAY", "SIN") ~ 1,
                          TRUE ~ 0)) %>%
  left_join(marco_ext %>%
              select(CLAVE_CASILLA, estrato, tipo_seccion, TIPO_CASILLA, TVIVHAB:VPH_SNBIEN),
            by = c("CLAVE_CASILLA"))
  #filter(TOTAL_VOTOS_CALCULADOS != 0) %>% # alrededor de 200 casillas no entregadas
  #filter(!is.na(tipo_seccion)) # casillas


write_csv(conteo_2018, "data-raw/conteo_2018.csv")
usethis::use_data(conteo_2018, overwrite = TRUE)
