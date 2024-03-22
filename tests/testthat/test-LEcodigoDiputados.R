## This tests replicates LE's code and checks that functions give expected results.

#library(tidyverse)
#source("./R/fns_codigo_diputados.R")

## create an object with Keys for parties and coalitions
## at the moment hardcoded...
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
dir <- "testdata_diputados"
id_remesa<-"0400020027"

c1<-scan(testthat::test_path(dir,paste("REMESAS",id_remesa,".txt",sep="")),n=1)
remesa<-read.table(testthat::test_path(dir,paste("REMESAS",id_remesa,".txt",sep="")),
                   header=TRUE,skip=1,sep="|")

## reads first 10 simulations for theta for id_remesa
theta_long <- readr::read_rds(testthat::test_path(dir,paste0("theta_melt_10_",id_remesa,".rds")))

## info = Info with ID_ESTRATO for each of the 300 DF and their listas nominales...
info<-read.csv(testthat::test_path(dir,"Info_distritos_2021.txt"))

#Info coaliciones
coa<-read.csv(testthat::test_path(dir,"2021_Diputados_Coaliciones.csv"))

## reformat coa to long object
coa_long <- coa |>
  left_join(info|> select(ID_ENTIDAD = ID_ESTADO, DISTRITO = ID_DISTRITO,id_estrato =ID_ESTRATO)) |>
  select(id_estrato,coaliciones_que_aplican=Coaliciones.que.aplican,any_of(keys_part_coa$nombre_partido)) |>
  tidyr::pivot_longer(cols = any_of(keys_part_coa$nombre_partido),names_to = "nombre_partido",values_to = "reciben_votos_coa") |>
  ## format names so they match keys_part_coa
  ## NB. FORMATEO MANUAL
  mutate(coaliciones_que_aplican = ifelse(coaliciones_que_aplican =="Va por M\xe9xico","Va por Mexico",coaliciones_que_aplican))

### First Step: 300 seats Maximo Relativo  ####

nd1_long<-calcular_nd1(theta_long,coa_long,keys_part_coa)
#write_rds(nd1_long, file= paste0(dir,"nd1_long_10_0400020027.rds"))

# to test faster only one simulation. Could be done for all.  #sim_check = 1:10
sim_check = 10
theta_long_test<- theta_long |>
  filter(sim %in% sim_check)

nd1_long_test<-calcular_nd1(theta_long_test,coa_long,keys_part_coa)

nd1_long_test_against <- readr::read_rds(testthat::test_path(dir,paste0("nd1_long_10_",id_remesa,".rds"))) |>
  filter(sim %in% sim_check)

testthat::test_that("nd1 checks", {
  check <- nd1_long_test |>
    group_by(sim) |>
    summarise(total = sum(nd1))  |>
    filter(total !=300) ## each simulation assigns 300 DFs

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nd1_long_test, nd1_long_test_against)
})

### Second Step: 200 seats Rep Prop  ####
lista_nominal_estrato <- info |> select(id_estrato = ID_ESTRATO, LISTA_NOMINAL)

nd2_1_calc<-calcular_nd2_1(theta_long,lista_nominal_estrato,keys_part_coa)

nd2_1_long <- nd2_1_calc$nd2_1_long
#write_rds(nd2_1_long, file= paste0(dir,"nd2_1_long_10_0400020027.rds"))

# to test faster only one simulation. Could be done for all.  #sim_check = 1:10
sim_check = 10
theta_long_test<- theta_long |>
  filter(sim %in% sim_check)

nd2_1_long_test<-calcular_nd2_1(theta_long_test,lista_nominal_estrato,keys_part_coa)$nd2_1_long

nd2_1_long_test_against <- readr::read_rds(testthat::test_path(dir,paste0("nd2_1_long_10_",id_remesa,".rds"))) |>
  filter(sim %in% sim_check)

testthat::test_that("nd2_1 checks", {
  check <- nd2_1_long_test |>
    group_by(sim) |>
    summarise(total = sum(nd2_1))  |>
    filter(total !=200) ## each simulation assigns 200 DFs

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nd2_1_long_test, nd2_1_long_test_against)
})

### Third Step: Corrections for 200 seats Rep Prop  ####

eta_long <- nd2_1_calc$eta_long

nd2_corregida<-corregir_nd2_sobrerep(eta_long,nd1_long,nd2_1_long)

testthat::test_that("nd2_corregida checks", {
  check <- nd2_corregida |>
    group_by(sim) |>
    summarise(total = sum(nd2))  |>
    filter(total !=200)  ## each simulation assigns 200 DFs

  testthat::expect_equal(nrow(check), 0)

})

### Total 500 seats summing up nd1 and nd2  ####

nd <- nd1_long |> full_join(nd2_corregida) |>
  arrange(sim,id_partido) |>
  mutate(across(c(nd1,nd2), ~tidyr::replace_na(.,0))) |>
  mutate(nd = nd1 + nd2)

testthat::test_that("nd checks", {
  check <- nd |>
    group_by(sim) |>
    summarise(total = sum(nd))  |>
    filter(total !=500)  ## each simulation assigns 500 DFs

  testthat::expect_equal(nrow(check), 0)

})



