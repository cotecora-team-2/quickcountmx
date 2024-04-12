### Test functions deputies quickcount(qc)
library(tidyverse)

assignment_tbl <- readr::read_csv(testthat::test_path("testdata_diputados/asignacion-coaliciones.csv")) |>
  rename(party = nombre_partido) |> mutate(strata = paste(ID_ENTIDAD, DISTRITO, sep = "."))

nombres_partido <- tibble(id_partido = 1:13,
                          party=c("PAN","PRI","PRD","PT","PVEM","MC","MORENA","PES","RSP","FPM","CI1","NULOSNR","PART"))

## reads first 10 simulations for theta for id_remesa
theta_long<-readr::read_rds(testthat::test_path("testdata_diputados/theta_melt_10_0400020027.rds"))
## info = Info with strata for each of thr 300 DF and listas nominales...
info<-read.csv(testthat::test_path("testdata_diputados/Info_distritos_2021.txt"))

#### Test assign_majority ####
## re-create format qc output
strata_tbl <- theta_long |>
  left_join(info |> select(id_estrato =ID_ESTRATO,ID_ESTADO,ID_DISTRITO))|>
  mutate(strata = paste(ID_ESTADO,ID_DISTRITO,sep = "."))|>
  left_join(nombres_partido) |>
  filter(id_partido <=11)|>
  select(strata, party, prop_votes = value, rep = sim)

nd1_maj_qc <- assign_majority(strata_tbl, assignment_tbl ,
                               party_name = party, candidate_name = candidato)

nd1_long_qc <- nd1_maj_qc |>
  left_join(nombres_partido |> select(id_partido,candidate =party))|>
  select(sim =rep,id_partido, nombre_partido =candidate, nd1 =n_seats_maj)|>
  arrange(sim,id_partido)

nd1_long_test_against<-readr::read_rds(testthat::test_path("testdata_diputados/nd1_long_10_0400020027.rds")) |>
  ungroup()


testthat::test_that("nd1 checks", {
  check <- nd1_long_qc |>
    group_by(sim) |>
    summarise(total = sum(nd1))  |>
    filter(total !=300) ## each simulation assigns 300 districts

  nd1_long_qc_flt <- nd1_long_qc |>
  filter(nd1 !=0)
  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nd1_long_qc_flt, nd1_long_test_against)
})

#### Test add_max_seats with first assignment ####

# Prepare LE output to match qc output.
total_tbl_LE <- theta_long |>
  left_join( info |> select(id_estrato = ID_ESTRATO, LISTA_NOMINAL)) |>
  group_by(sim,id_partido) |>
  summarise(lam = sum(value*LISTA_NOMINAL/sum(LISTA_NOMINAL)))|>
  # paste code for lambda_partidos
  filter(id_partido <=11) |> ## leave parties up to CI1
  group_by(sim) |>
  mutate(lambda = lam/sum(lam)) |>
  # adequate output to fns_qc
  left_join(nombres_partido) |>
  select(party, prop = lambda, rep = sim)|>
  dplyr::filter(party != "part") |>
  dplyr::mutate(n_assign = 200, topped = FALSE)

total_tbl_LE_maj <- total_tbl_LE |>
  mutate(
    prop_vot_nal = ifelse(prop < 0.03 |
                            stringr::str_detect(party, "CI"),
                          0, prop),
    prop_vot_nal = prop_vot_nal / sum(prop_vot_nal),
    n_seats_max = floor(500 * (prop_vot_nal + 0.08))
  ) |>
  left_join(nd1_long_qc, by = c("party" = "nombre_partido", "rep" = "sim")) |>
  rename(n_seats_maj = nd1) |>
  relocate(n_seats_max,.after = n_seats_maj)

total_tbl_add_max_qc <- add_max_seats(total_tbl_LE,nd1_maj_qc) |>
  select(-n_seats_max_raw)

testthat::test_that("add_max_seats checks", {

  total_tbl_LE_maj_test <- total_tbl_LE_maj |> select(-id_partido)

  testthat::expect_equal(total_tbl_add_max_qc, total_tbl_LE_maj_test )
})


#### Test assign_prop with first assignment ####

# run assign_prop in test data
nd2_1_long_qc <- assign_prop(total_tbl_LE_maj) |>
  ungroup() |>
  left_join(nombres_partido) |>
  select(sim =rep,id_partido, nombre_partido =party, nd2_1 =n_seats_prop) |>
  filter(id_partido <=10) |>
  arrange(sim,id_partido)

### check vs eta
### prop_adj of qc corresponds to LE's etas.
eta_long <- readr::read_rds(testthat::test_path("testdata_diputados/eta_long_10_0400020027.rds"))

nd2_1_long_test_against <- readr::read_rds(testthat::test_path("testdata_diputados/nd2_1_long_10_0400020027.rds")) |>
  ungroup()

testthat::test_that("nd2_1 checks", {
  ## check sum to 200
  check <- nd2_1_long_qc |>
    group_by(sim) |>
    summarise(total = sum(nd2_1))  |>
    filter(total !=200)
  ## check eta and prop_adj are the same
  check_props <- eta_long |> left_join(
    assign_prop(total_tbl_LE_maj) |>
      ungroup() |>
      left_join(nombres_partido)|>
      select(sim=rep,id_partido,party,prop_adj)) |>
    mutate(check = eta-prop_adj < 0.000000001) |>
    filter(!check)

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(nrow(check_props), 0)
  testthat::expect_equal(nd2_1_long_qc, nd2_1_long_test_against)
})

#### Test assign_all_seats no over-representation ####

estimates <- list(total_tbl = total_tbl_LE, strata_tbl = strata_tbl)

assign_seats_qc <- assign_all_seats(estimates, assignment_tbl) |>
  ungroup()|>
  left_join(nombres_partido) |>
  select(sim =rep,id_partido, nombre_partido =party, nd1 = n_seats_maj, nd2 =n_seats_prop,nd =n_seats_total) |>
  arrange(sim,id_partido)

nd2_long_test_against <- nd2_1_long_test_against |>
  rename(nd2=nd2_1)

nd <- nombres_partido |> filter(id_partido<=11) |>
  select(id_partido,nombre_partido=party) |>
  mutate(sim=1)|>
  full_join(nd1_long_test_against) |> full_join(nd2_long_test_against) |>
  complete(nesting(id_partido,nombre_partido),sim) |>
  arrange(sim,id_partido) |>
  mutate(across(c(nd1,nd2), ~replace_na(.,0))) |>
  mutate(nd = nd1 + nd2)|>
  select(sim,id_partido,nombre_partido,nd1,nd2,nd)

testthat::test_that("nd checks", {
  check <- assign_seats_qc |>
    group_by(sim) |>
    summarise(total = sum(nd))  |>
    filter(total !=500) ## each simulation assigns 500 total DF

  testthat::expect_equal(nrow(check), 0)
  testthat::expect_equal(assign_seats_qc, nd)

})


#### Test assign_all_seats with over-representation ####
### check with an altered data set to force over-representation
reps_list <-estimates
reps_list$total_tbl <- reps_list$total_tbl |>
  mutate(prop= if_else(rep==1 & party == "MORENA",.15,prop))

nd2_corrected <- readr::read_rds(testthat::test_path("testdata_diputados/nd2_corrected_10_0400020027.rds"))

assign_seats_qc_sobrerep <- assign_all_seats(reps_list, assignment_tbl) |>
  ungroup()|>
  left_join(nombres_partido) |>
  select(sim =rep,id_partido, nombre_partido =party, nd1 = n_seats_maj, nd2 =n_seats_prop,nd =n_seats_total) |>
  arrange(sim,id_partido)

nd_sobrerep <- nombres_partido |> filter(id_partido<=11) |>
  select(id_partido,nombre_partido=party) |>
  mutate(sim=1)|>
  full_join(nd1_long_test_against) |> full_join(nd2_corrected) |>
  complete(nesting(id_partido,nombre_partido),sim) |>
  arrange(sim,id_partido) |>
  mutate(across(c(nd1,nd2), ~replace_na(.,0))) |>
  mutate(nd = nd1 + nd2)|>
  select(sim,id_partido,nombre_partido,nd1,nd2,nd)

testthat::test_that("nd checks over-rep", {
  check1 <- assign_seats_qc_sobrerep |>
    group_by(sim) |>
    summarise(total = sum(nd))  |>
    filter(total !=500) ## each simulation assigns 500 total DF
  check2 <- assign_seats_qc_sobrerep |>
    group_by(sim) |>
    mutate(nd_diff = nd -nd1 -nd2) |>
    summarise(diff = sum(nd_diff != 0))  |>
    filter(diff !=0) ## totals match

  testthat::expect_equal(nrow(check1), 0)
  testthat::expect_equal(nrow(check2), 0)
  testthat::expect_equal(assign_seats_qc_sobrerep$nd, nd_sobrerep$nd)
  testthat::expect_equal(assign_seats_qc_sobrerep, nd_sobrerep)

})

#### Test assign_all_seats with  over-representation and majority greater than max. ####
### check with an altered data set to force over-representation with majority (nd1) which would give a negative nd2
reps_list <-estimates
reps_list$total_tbl <- reps_list$total_tbl |>
  mutate(prop= if_else(rep==1 & party == "MORENA",.1,prop))  |>
  mutate(prop= if_else(rep==1 & party == "PAN",.05,prop))|>
  mutate(prop= if_else(rep==1 & party == "PRI",.5,prop))


nd2_corrected_2_neg <- readr::read_rds(testthat::test_path("testdata_diputados/nd2_corrected_neg_10_0400020027.rds"))

assign_seats_qc_sobrerep <- assign_all_seats(reps_list, assignment_tbl) |>
  ungroup()|>
  left_join(nombres_partido) |>
  select(sim =rep,id_partido, nombre_partido =party, nd1 = n_seats_maj, nd2 =n_seats_prop,nd =n_seats_total) |>
  arrange(sim,id_partido)

nd_sobrerep <- nombres_partido |> filter(id_partido<=11) |>
  select(id_partido,nombre_partido=party) |>
  mutate(sim=1)|>
  full_join(nd1_long_test_against) |> full_join(nd2_corrected_2_neg) |>
  complete(nesting(id_partido,nombre_partido),sim) |>
  arrange(sim,id_partido) |>
  mutate(across(c(nd1,nd2), ~replace_na(.,0))) |>
  mutate(nd = nd1 + nd2)|>
  select(sim,id_partido,nombre_partido,nd1,nd2,nd)


testthat::test_that("nd checks over-rep-neg", {
  check1 <-assign_seats_qc_sobrerep |>
    group_by(sim) |>
    summarise(total = sum(nd1))  |>
    filter(total !=300) ## cada simulacion reparte los 300 distritos total
  check2 <-assign_seats_qc_sobrerep |>
    group_by(sim) |>
    summarise(total = sum(nd2))  |>
    filter(total !=200) ## cada simulacion reparte los 200 distritos total
  check3 <- assign_seats_qc_sobrerep |>
    group_by(sim) |>
    summarise(total = sum(nd))  |>
    filter(total !=500) ## cada simulacion reparte los 500 distritos total
  check4 <- assign_seats_qc_sobrerep |>
    group_by(sim) |>
    mutate(nd_diff = nd -nd1 -nd2) |>
    summarise(diff = sum(nd_diff != 0))  |>
    filter(diff !=0) ## cada simulacion no hay differencias
  check5 <- assign_seats_qc_sobrerep |>
    group_by(sim) |>
    mutate(check3 = nd2 >=0) |>
    filter(!check3) ## cada simulacion no hay differencias

  testthat::expect_equal(nrow(check1), 0)
  testthat::expect_equal(nrow(check2), 0)
  testthat::expect_equal(nrow(check3), 0)
  testthat::expect_equal(nrow(check4), 0)
  testthat::expect_equal(nrow(check5), 0)
  testthat::expect_equal(assign_seats_qc_sobrerep$nd, nd_sobrerep$nd)
  testthat::expect_equal(assign_seats_qc_sobrerep, nd_sobrerep)

})

