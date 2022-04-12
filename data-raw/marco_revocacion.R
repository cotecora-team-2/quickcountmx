library(tidyverse)


marco_rev_tmp <- read_csv("./data-raw/votacion_simulada_simulacro_2.csv")

marco_revocacion <- marco_rev_tmp |>
  select(CLAVE_CASILLA:ID_ESTRATO) |>
  mutate(ln = LISTA_NOMINAL) |>
  mutate(ln = ifelse(ln == 0, 3050, ln)) |>
  mutate(x = as.numeric(TIPO_CASILLA == "S")) |>
  mutate(id_casilla = row_number()) |>
  mutate(estrato = ID_ESTRATO)

modificaciones_ultimas <- read_csv("./data-raw/ubicacion_casillas_final_08042022.csv") |>
  select(CLAVE_CASILLA)

marco_revocacion_ajustado <- semi_join(marco_revocacion, modificaciones_ultimas)

write_csv(marco_revocacion_ajustado, file = "data-raw/marco_revocacion.csv")
