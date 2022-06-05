library(tidyverse)
marco_previo <- readr::read_rds("data-raw/marco_2022_previo.rds")
eliminadas_1 <- readr::read_csv("data-raw/Casillas_Baja.csv")
eliminadas_2 <- readr::read_csv("data-raw/Oaxaca_Casillas_Baja_Complemento_4junio2022 (1).csv")

prueba_1 <- semi_join(marco_previo, eliminadas_1)
nrow(prueba_1)
nrow(eliminadas_1)

prueba_2 <- semi_join(marco_previo, eliminadas_2)
nrow(prueba_2)
nrow(eliminadas_2)

eliminadas <- bind_rows(eliminadas_1, eliminadas_2)
marco_nuevo <- anti_join(marco_previo, eliminadas)
nrow(marco_previo) - nrow(marco_nuevo)
View(marco_nuevo)

marco_nuevo <- marco_nuevo |> group_by(ID_ESTADO) |>
  mutate(no_casilla = row_number()) |> ungroup()

write_rds(marco_nuevo, "data-raw/marco_2022.rds")

## Checar Durango
durango <- filter(marco_nuevo, ID_ESTADO==10) #ok


