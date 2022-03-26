library(tidyverse)


marco_rev_tmp <- read_csv("../pruebas-conteo/reporte-votacion-sim/votacion_simulada_simulacro_2.csv")

marco_revocacion <- marco_rev_tmp |>
  select(CLAVE_CASILLA:ID_ESTRATO) |>
  mutate(ln = LISTA_NOMINAL) |>
  mutate(ln = ifelse(ln == 0, 3050, ln)) |>
  mutate(x = as.numeric(TIPO_CASILLA == "S")) |>
  mutate(id_casilla = row_number()) |>
  mutate(estrato = ID_ESTRATO)

write_csv(marco_revocacion, file = "data-raw/marco_revocacion.csv")
