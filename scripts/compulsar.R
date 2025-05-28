library(tidyverse)
#ruta <- "/Volumes/estimaciones/"
ruta <- "~/Documents/pruebas-sim1/"
ruta_salida <- "~/Documents/pruebas-sim1/"
remesa <- "05271700"
nombre_razon <- stringr::str_c(ruta, "razon", remesa, ".csv")
nombre_hb <- stringr::str_c(ruta, "hb", remesa, ".csv")

## Leer archivos
razon_tbl <- read_csv(nombre_razon)
hb_tbl <- read_csv(nombre_hb)

if(nrow(razon_tbl) == 0 | nrow(hb_tbl) == 0){
  stop("Alguna de la salidas no tiene datos.")
}
tab_1 <- razon_tbl |> select(muestra, p_muestra, num_estratos)
tab_2 <- hb_tbl |> select(muestra, p_muestra, num_estratos)

if(!all(tab_1==tab_2)){
  stop("Las salidas no cuadran en muestra")
}


## Compulsar
compulsado_tbl <-
  left_join(razon_tbl |> select(EN, R, part_r = PART, LMU),
            hb_tbl |> select(EN, R, part_hb = PART, LMU)) |>
  filter(LMU != 1) |>
  mutate(PART = ifelse(LMU==0, pmin(part_r, part_hb), pmax(part_r, part_hb))) |>
  mutate(EQ = "compulsado") |>
  select(EQ, EN, R, PART, LMU) |>
  mutate(R = str_c(EN, R)) |>
  mutate(EN="00") |>
  left_join(razon_tbl |> select(LMU, muestra, p_muestra, num_estratos))


#compulsado_tbl <- compulsado_tbl |>
#  mutate(muestra = 354, p_muestra = round(100*muestra/1644,2), num_estratos = 59)

##
nombre_salida <- str_c(ruta_salida, "compulsado", remesa, ".csv")
write_csv(compulsado_tbl, nombre_salida)





