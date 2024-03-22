#### First attempt to rewrite LE code in functions


#' @name asignar_diputados
#' @aliases calcular_nd1
#' @aliases calcular_nd2
#' @aliases corregir_nd2_sobrerep
#'
#' @title  Functions that help with the  assignment of the Chamber of Deputies.
#'
#' @description There are 500 seats to be assigned in the Chamber of Deputies in Mexico.
#' The process involves different steps.
#' `calcular_nd1`: Calculates the assignment of the first 300 seats by "maximo relativo".
#' `calcular_nd2`: Calculates the assignment of the second 200 seats by "representacion proporcional".
#' `corregir_nd2_sobrerep`: Corrects the second assignment of the second 200 seats for over representation,
#'  taking into account the maximum number allowed per party.
#'
#' @param theta_long a data frame in long format with columns:
#' `id_estrato` (integer for each of the 300 federal districts);
#' `id_partido` (integer for each of the parties);
#' `sim` (integer id for each of the simulations);
#' `value` (numeric with the value of the estimated theta)
#' @param coa_long tibble in long format with the data for coalition assignment in each Federal District
#' `id_estrato` (integer for each of the 300 federal districts);
#' `coaliciones_que_aplican` (character, with coalitions that apply in the DF,
#' either name of coalition to match `coalicion_partido` in `keys_part_coa`),
#' "Ambas" if both/all or "Ninguna" if no coalitions apply for that DF;
#' `nombre_partido` (character with party name);
#' `reciben_votos_coa` (integer, indicator: 1 if the party of the coalition receives the votes for that DF, 0 if not);
#' @param keys_part_coa a data frame with the key variables for assignment.
#' `id_partido` (integer for each of the parties);
#' `nombre_partido` (character with party name);
#' `candidato_independiente` (numeric, 1 if the party is from an independent candidate, 0 if not);
#' `id_coalicion` (numeric, id for coalicion);
#' `coalicion_partido` (character, name of the coalicion);
#' @param lista_nominal_estrato a data frame with the nominal list for each DF. Columns:
#' `id_estrato` (integer id for each of the 300 federal districts);
#' `LISTA_NOMINAL` (integer, nominal list for each of the 300 federal districts);
#' @param eta_long tibble in long format with with `sim`, `id_partido`, `nombre_partido` and
#' `eta` which is created in `calcular_nd2` and used in `corregir_nd2_sobrerep` as an input.
#' @param nd1_long tibble in long format with grouped by simulation with columns:
#' `sim`, `id_partido`, `nombre_partido`, and `nd1`.
#'  `nd1_long` is created in `calcular_nd1` and used in `corregir_nd2_sobrerep` as an input
#' @param nd2_1_long tibble in long format with grouped by simulation with columns:
#' `sim`, `id_partido`, `nombre_partido`, and `nd2_1`.
#'  `nd2_long` is created in `calcular_nd2` and used in `corregir_nd2_sobrerep` as an input
#'
#' @rdname asignar_diputados
#' @return  `calcular_nd1` returns a tibble (`nd1_long`) grouped by simulation with columns:
#' `sim`, `id_partido`, `nombre_partido`, and
#' `nd1` (integer, number of seats out of the first 300 assigned for each of the parties);
#'
#' @export
calcular_nd1 <- function(theta_long,coa_long,keys_part_coa){

  ## Assign votes depending on coalition (each DF decides which party is assigned the coalition votes)
  votos_asignacion_coa <- theta_long |>
    right_join(keys_part_coa) |>
    arrange(sim,id_estrato,id_partido) |>
    left_join(coa_long, by=c("id_estrato","nombre_partido")) |>
    group_by(sim,id_estrato,coalicion_partido) |>
    mutate(suma_coalicion = sum(value)) |> # calculate sums by coalition: auxiliar variable
    group_by(sim,id_estrato) |>
    mutate(valor_ajustado = case_when(
      coalicion_partido == coaliciones_que_aplican ~ suma_coalicion*reciben_votos_coa,
      (coaliciones_que_aplican == "Ambas") ~ suma_coalicion*reciben_votos_coa,
      TRUE ~ value)) |>  #view()
    select(id_estrato,id_partido,sim,nombre_partido,valor_ajustado)

  ## calculate for each simulation and DF which party got the maximum
  partido_maximo_DF_sim <- votos_asignacion_coa |>
    group_by(sim,id_estrato) |>
    summarise(partido_con_maximo = id_partido[which.max(valor_ajustado)])

  ## nd1: number of deputies after first step (maximo relativo)
  ## for each simulation number of times that each party was maximum in the 300 DF
  nd1_long <- partido_maximo_DF_sim |>
    group_by(sim,partido_con_maximo) |>
    summarise(nd1 = n()) |>
    select(sim,id_partido = partido_con_maximo,nd1) |>
    left_join(keys_part_coa |> select(id_partido,nombre_partido)) |>
    select(sim,id_partido,nombre_partido,nd1)|>
    arrange(sim,id_partido)

  return(nd1_long)
} # end function
#'
#' @rdname asignar_diputados
#' @return  `calcular_nd2` returns a list with 2 elements:
#' `nd2_1_long` is a tibble grouped by simulation with columns:
#' `sim`, `id_partido`, `nombre_partido` and
#' `nd2_1` (numeric, number of seats out of the first assignment of the 200 seats assigned for each of the parties);
#' and `eta_long` another tibble with `sim`, `id_partido`, `nombre_partido` and `eta` which is used in the next function for corrections.
#'
#' @export
calcular_nd2_1 <- function(theta_long,lista_nominal_estrato,keys_part_coa){

  # lambda partidos
  lambda_partidos <- theta_long |>
    left_join(lista_nominal_estrato) |>
    group_by(sim,id_partido) |>
    summarise(lam = sum(value*LISTA_NOMINAL/sum(LISTA_NOMINAL))) |>
    filter(id_partido %in% keys_part_coa$id_partido) |>
    group_by(sim) |>
    mutate(lambda = lam/sum(lam)) |>
    select(-lam)

  # calculate theta3
  theta3_long <- theta_long |>
    # take out independent candidates
    filter(id_partido %in% keys_part_coa$id_partido[keys_part_coa$candidato_independiente ==0]) |>
    left_join(lambda_partidos) |>
    mutate(value = ifelse(lambda > 0.03, value, 0)) |> # take out parties with votes less than 3%
    select(-lambda)

  # calculate eta.
  eta_long <- theta3_long |>
    left_join(lista_nominal_estrato) |>
    group_by(sim,id_partido) |>
    summarise(eta = sum(value*LISTA_NOMINAL/sum(LISTA_NOMINAL))) |>
    group_by(sim) |>
    mutate(eta = eta/sum(eta)) |>
    left_join(keys_part_coa |> select(id_partido,nombre_partido))

  # Asignacion de diputados por rep. proporcional. Primer calculo de nd2
  nd2_1_long <- eta_long |>
    group_by(sim) |>
    mutate(nd2_1a = floor(200*eta)) |>
    mutate(rm2 = 200*eta-nd2_1a) |>
    mutate(rtot = 200-sum(nd2_1a)) |>
    arrange(sim,desc(rm2))|>
    mutate(aux = row_number()<=rtot)|>
    mutate(nd2_1 = if_else(aux,nd2_1a+1,nd2_1a)) |>
    select(sim,id_partido,nombre_partido,nd2_1) |>
    arrange(sim,id_partido)

  return(list(nd2_1_long=nd2_1_long,eta_long=eta_long))
} # end funcion calcular nd2_1
#'
#' @rdname asignar_diputados
#' @return  `corregir_nd2_sobrerep` uses outputs from the other two functions and returns a tibble with columns:
#' `sim`, `id_partido`, `nombre_partido` and
#' `nd2` (numeric, number of seats out of the corrections for over representation of the 200 seats assigned for each of the parties);
#'
#' @export
corregir_nd2_sobrerep <- function(eta_long,nd1_long,nd2_1_long){

  max_nd_partido <- eta_long |>
    mutate(max_nd = floor(500*(eta+0.08))) |>
    select(-eta)

  nd2_sobrep_long <- nd2_1_long
  # # In case it is needed to create a case where there is sobrerrep in LE example
  # nd2_sobrep_long <- nd2_1_long |>
  #   mutate(nd2_1 = if_else(sim==1&id_partido==7,nd2_1+28,nd2_1)) |>
  #   mutate(nd2_1 = if_else(sim==1&id_partido==1,nd2_1-28,nd2_1))
  eta2_long <- eta_long
  stop = FALSE
  while(!stop){
    #print(stop)
    sobrerrep <-  nd2_sobrep_long|>
      left_join(nd1_long) |>
      select(sim, id_partido, nombre_partido,nd1,nd2_1) |>
      mutate(across(starts_with("nd"), ~ if_else(is.na(.x),0,.x))) |>
      left_join(max_nd_partido) |>
      mutate(nd = nd1 + nd2_1) |>
      mutate(id_sobrerrep= nd > max_nd) |>
      mutate(cuantas_sobran = if_else(id_sobrerrep,nd-max_nd,0))

    if(sum(sobrerrep$id_sobrerrep) >0) {
      ## this follows LE logic. Might be done better.
      rep_prop_sr1 <- sobrerrep |>
        mutate(nd2_aj1 = if_else(id_sobrerrep,max_nd-nd1,nd2_1))|>
        mutate(pr = if_else(id_sobrerrep,0,max_nd-nd))|>
        mutate(piid = if_else(pr>0,1,0))|>
        #Correction for negative rep.prop.
        mutate(nd2_aj2 = if_else(nd2_aj1<0,0,nd2_aj1))|>
        mutate(pr_aj1 = if_else(pr<0,0,pr))|>
        # second step to assign proportions
        group_by(sim) |>
        mutate(rtot_aj1 = 200-sum(nd2_aj2*(1-piid)))|>
        left_join(eta2_long)|>
        mutate(eta2 = piid*eta/sum(piid*eta)) |>
        mutate(nd2_aj3 = nd2_aj2*(1-piid)+floor(rtot_aj1*eta2)) |>
        mutate(rm2_aj1 = piid*(rtot_aj1*eta2-floor(rtot_aj1*eta2))) |>
        mutate(rtot_aj2 = 200-sum(nd2_aj3)) |>
        arrange(sim,desc(rm2_aj1))|>
        mutate(aux = row_number()<=rtot_aj2)|>
        mutate(nd2_2 = if_else(aux,nd2_aj3+1,nd2_aj3))|>
        arrange(sim,id_partido)

      nd2_sobrep_long <- rep_prop_sr1 |>
        select(sim,id_partido,nombre_partido,nd2_1 = nd2_2)
      eta2_long <- rep_prop_sr1 |>
        select(sim,id_partido,nombre_partido,eta = eta2)
    } else{
      stop = TRUE
    } # else
  } # while

  nd2_sobrep_long <- nd2_sobrep_long |> rename(nd2 = nd2_1)

  return(nd2_sobrep_long)
} # end funcion corregir
