library(tidyverse)
ruta <- "~/Documents/pruebas-script/"
ruta_salida <- "~/Documents/pruebas-script/"
remesa <- "2800001900"
nombre_razon <- stringr::str_c(ruta, "razon", remesa, ".csv")
nombre_hb <- stringr::str_c(ruta, "hb", remesa, ".csv")

## Leer archivos
razon_tbl <- read_csv(nombre_razon)
hb_tbl <- read_csv(nombre_hb)

if(nrow(razon_tbl) == 0 | nrow(hb_tbl) == 0){
  stop("Alguna de la salidas no tiene datos.")
}

## Compulsar
compulsado_tbl <-
  left_join(razon_tbl |> select(EN, R, part_r = PART, LMU),
            hb_tbl |> select(EN, R, part_hb = PART, LMU)) |>
  filter(LMU != 1) |>
  mutate(PART = ifelse(LMU==0, pmin(part_r, part_hb), pmax(part_r, part_hb))) |>
  mutate(EQ = "compulsado") |>
  select(EQ, EN, R, PART, LMU)

##
nombre_salida <- str_c(ruta_salida, "compulsado", remesa, ".csv")
write_csv(compulsado_tbl, nombre_salida)





## Escribir archivo
