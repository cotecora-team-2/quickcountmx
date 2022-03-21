library(tidyverse)


votacion_simulada_simulacro <- read_csv("../pruebas-conteo/reporte-votacion-sim/votacion_simulada_simulacro.csv")

marco_simulacro_0322 <- votacion_simulada_simulacro |>
  select(CLAVE_CASILLA:ESTRATO) |>
  mutate(ln = LISTA_NOMINAL) |>
  mutate(ln = ifelse(ln ==0, 3100, ln)) |>
  mutate(x = as.numeric(TIPO_CASILLA=="S")) |>
  mutate(id_casilla = row_number()) |>
  mutate(estrato = ID_ESTRATO)

write_csv(marco_simulacro_0322, file = "data-raw/marco_simulacro_0322.csv")
