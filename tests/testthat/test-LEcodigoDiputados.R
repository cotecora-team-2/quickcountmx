#### Primer intento de reescribir codigo de LE en funciones
### Este test replica el codigo de LE y checa que las funciones den los resultados esperados

library(tidyverse)
source("./R/fns_codigo_diputados.R")

## crear un archivo con Keys para partidos y coaliciones
## ahorita hardcodeado... hay que ver de donde se saca esta info.
keys_part_coa <- tibble(id_partido = 1:11,
                        nombre_partido = c("PAN","PRI","PRD","PT","PVEM","MC","MORENA","PES","RSP","FPM","CI1"),
                        candidato_independiente = c(rep(0,10),1),
                        id_coalicion = c(rep(1,3),rep(2,2),0,2,rep(0,4))) |>
  mutate(coalicion_partido = case_when(
    id_coalicion == 1 ~ "Va por Mexico",
    id_coalicion == 2 ~ "Juntos Hacemos Historia",
    TRUE ~ "sin_coalicion"))

### READ inputs ####

## remesas y sus thetas
dir <- "./data-raw/diputados_LE/"
id_remesa<-"0400020027"

c1<-scan(paste(dir,"REMESAS",id_remesa,".txt",sep=""),n=1)
remesa<-read.table(paste(dir,"REMESAS",id_remesa,".txt",sep=""),header=TRUE,skip=1,sep="|")

## reads first 10 simulations for theta for id_remesa
theta_long<-read_rds(paste0(dir,"theta_melt_10_",id_remesa,".rds"))


## info = Info con numero de estrato para cada uno de los 300 DF y sus listas nominales...
info<-read.csv("./data-raw/diputados_LE/Info_distritos_2021.txt")
#Info coaliciones
coa<-read.csv("./data-raw/diputados_LE/2021_Diputados_coaliciones.csv")
## reformatear coa
coa_long <- coa |>
  left_join(info|> select(ID_ENTIDAD = ID_ESTADO, DISTRITO = ID_DISTRITO,id_estrato =ID_ESTRATO)) |>
  select(id_estrato,coaliciones_que_aplican=Coaliciones.que.aplican,any_of(keys_part_coa$nombre_partido)) |>
  pivot_longer(cols = any_of(keys_part_coa$nombre_partido),names_to = "nombre_partido",values_to = "reciben_votos_coa") |>
  ## formatear nombres a que cuadren con el keys_part_coa
  ## NB. FORMATEO MANUAL
  mutate(coaliciones_que_aplican = ifelse(coaliciones_que_aplican =="Va por M\xe9xico","Va por Mexico",coaliciones_que_aplican))

nd1_long<-calcular_nd1(theta_long,coa_long,keys_part_coa)
#write_rds(nd1_long, file= paste0(dir,"nd1_long_10_0400020027.rds"))

# para pruebas acorde a Felipe voy a fijar 1 simulaciones
# se podria hacer igual para todas sin seleccionar una sola...
#sim_check = 1:10
sim_check = 10
theta_long_test<- theta_long |>
  filter(sim %in% sim_check)

nd1_long_test<-calcular_nd1(theta_long_test,coa_long,keys_part_coa)

nd1_long_test_against<-read_rds(paste0(dir,"nd1_long_10_",id_remesa,".rds")) |>
  filter(sim %in% sim_check)

testthat::test_that("nd1 checks", {
  check <- nd1_long_test |>
    group_by(sim) |>
    summarise(total = sum(nd1))  |>
    filter(total !=300) ## cada simulacion reparte los 300 distritos

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nd1_long_test, nd1_long_test_against)
})

### Para Representacion Proporcional ####
lista_nominal_estrato <- info |> select(id_estrato = ID_ESTRATO, LISTA_NOMINAL)

## Regresamos a theta para calcular la lambda de LE

### Primera vuelta nd2
nd2_1_calc<-calcular_nd2_1(theta_long,lista_nominal_estrato,keys_part_coa)
nd2_1_long <- nd2_1_calc$nd2_1_long
#write_rds(nd2_1_long, file= paste0(dir,"nd2_1_long_10_0400020027.rds"))

# para pruebas acorde a Felipe voy a fijar 1 simulaciones
# se podria hacer igual para todas sin seleccionar una sola...
#sim_check = 1:10
sim_check = 10
theta_long_test<- theta_long |>
  filter(sim %in% sim_check)

nd2_1_long_test<-calcular_nd2_1(theta_long_test,lista_nominal_estrato,keys_part_coa)$nd2_1_long

nd2_1_long_test_against<-read_rds(paste0(dir,"nd2_1_long_10_",id_remesa,".rds")) |>
  filter(sim %in% sim_check)

testthat::test_that("nd2_1 checks", {
  check <- nd2_1_long_test |>
    group_by(sim) |>
    summarise(total = sum(nd2_1))  |>
    filter(total !=200) ## cada simulacion reparte los 200 distritos de representacion proporcional

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nd2_1_long_test, nd2_1_long_test_against)
})

# check

### Maximo numero de diputaciones y sobrerrepresentacion
# Artículo 17, numerales 2 y 3, de la LGIPE; y, 54, fracciones IV y V de la CPEUM:
#   Un PPN no podrá contar con un número de diputaciones por ambos principios que representen un porcentaje del total de la Cámara que exceda en ocho puntos a su porcentaje de votación nacional emitida.
# A partir de lo anterior, se suma el número de diputaciones que le corresponden a cada PPN por ambos principios y se verifica si existe sobrerrepresentación:
# pm<-floor(500*(eta+0.08)) # PPT slide 7. Num maximo de diputados

eta_long <- nd2_1_calc$eta_long

nd2_corregida<-corregir_nd2_sobrerep(eta_long,nd1_long,nd2_1_long)

testthat::test_that("nd2_corregida checks", {
  check <- nd2_corregida |>
    group_by(sim) |>
    summarise(total = sum(nd2))  |>
    filter(total !=200) ## cada simulacion reparte los 200 distritos de representacion proporcional

  testthat::expect_equal(nrow(check), 0)

})


nd <- nd1_long |> full_join(nd2_corregida) |>
  arrange(sim,id_partido) |>
  mutate(across(c(nd1,nd2), ~replace_na(.,0))) |>
  mutate(nd = nd1 + nd2)

testthat::test_that("nd checks", {
  check <- nd |>
    group_by(sim) |>
    summarise(total = sum(nd))  |>
    filter(total !=500) ## cada simulacion reparte los 500 distritos total

  testthat::expect_equal(nrow(check), 0)

})



