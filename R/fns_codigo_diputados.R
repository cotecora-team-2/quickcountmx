#### Primer intento de reescribir codigo de LE en funciones

## calcular_nd1: Calcula la reparticion de los primeros 300 diputados por maximo relativo.
calcular_nd1 <- function(theta_long,coa_long,keys_part_coa){
  
  ## Asignar votos acorde a coa (cada DF decide a que partido se le asignan los votos a la coalicion)
  votos_asignacion_coa <- theta_long |>
    #filter(id_partido %in% keys_part_coa$id_partido) |> ## dejar solo los partidos hasta CI1, el right_join se hace cargo de eso
    right_join(keys_part_coa) |>
    arrange(sim,id_estrato,id_partido) |>
    left_join(coa_long, by=c("id_estrato","nombre_partido")) |>
    # calcular sumas por coalicion: variable auxiliar
    group_by(sim,id_estrato,coalicion_partido) |>
    mutate(suma_coalicion = sum(value)) |> 
    ## repartir por coalicion
    group_by(sim,id_estrato) |>
    mutate(valor_ajustado = case_when(
      coalicion_partido == coaliciones_que_aplican ~ suma_coalicion*reciben_votos_coa,
      (coaliciones_que_aplican == "Ambas") ~ suma_coalicion*reciben_votos_coa, 
      TRUE ~ value)) |> # view()
    select(id_estrato,id_partido,sim,nombre_partido,valor_ajustado)
  
  ## sacar pid = partido con el maximo por estrato
  partido_maximo_DF_sim <- votos_asignacion_coa |>
    ## para cada simulacion y estrato cual partido obtuvo el maximo
    group_by(sim,id_estrato) |>
    summarise(partido_con_maximo = id_partido[which.max(valor_ajustado)])
  
  ## nd1 numero de diputaciones en el primer paso (maximo relativo)
  nd1_long <- partido_maximo_DF_sim |>
    ## para cada simulacion numero de veces que el partido fue maximo en las 300 DF
    group_by(sim,partido_con_maximo) |>
    summarise(nd1 = n()) |>
    select(sim,id_partido = partido_con_maximo,nd1) |>
    left_join(keys_part_coa |> select(id_partido,nombre_partido)) |>
    select(sim,id_partido,nombre_partido,nd1)|>
    arrange(sim,id_partido)
  
  return(nd1_long)
} # end function

### Primera vuelta nd2 
calcular_nd2_1 <- function(theta_long,lista_nominal_estrato,keys_part_coa){
  #Creacion de lambda 
  ## Los nulos se quitan .. primero 11 partidos... y luego participacion... 
  ## En el codigo de LE theta tiene info para participacion. 
  ## Nosotros lo hacemos separado.
  
  lam_long <- theta_long |>
    left_join(lista_nominal_estrato) |>
    group_by(sim,id_partido) |>
    summarise(lam = sum(value*LISTA_NOMINAL/sum(LISTA_NOMINAL)))
  
  # lambda partidos
  lambda_partidos <- lam_long |>
    filter(id_partido %in% keys_part_coa$id_partido) |> ## dejar solo los partidos hasta CI1
    group_by(sim) |>
    mutate(lambda = lam/sum(lam)) |> 
    select(-lam)
  
  # calcular theta3
  theta3_long <- theta_long |>
    # quitar a candidato independiente y votos nulos
    filter(id_partido %in% keys_part_coa$id_partido[keys_part_coa$candidato_independiente ==0]) |> 
    left_join(lambda_partidos) |>
    mutate(value = ifelse(lambda > 0.03, value, 0)) |> # quitar partidos con votacion menor al 3%
    ## pregunta: la lambda se calcula con el partido independiente, pero aqui se quita el independiente, es correcto?
    select(-lambda)
  
  #check ... See PES con remesa "0400020027"
  #lambda_partidos |> filter(lambda <= 0.03)
  #theta3_long %>% filter(value == 0)
  
  # calcular eta.
  # similar a lam pero con theta3
  eta_long <- theta3_long |>
    left_join(lista_nominal_estrato) |>
    group_by(sim,id_partido) |>
    summarise(eta = sum(value*LISTA_NOMINAL/sum(LISTA_NOMINAL))) |>
    group_by(sim) |>
    mutate(eta = eta/sum(eta)) |>
    left_join(keys_part_coa |> select(id_partido,nombre_partido))
  
  #Asignacion de diputados por rep. proporcional
  # Primer calculo de nd2
  #nd2<-floor(200*eta) 
  # rtot<-200-apply(nd2,1,sum)
  # mm<-t(apply(rm2,1,order,decreasing=TRUE)) ## orden de mayor a menor
  
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

### Maximo numero de diputaciones y sobrerrepresentacion
corregir_nd2_sobrerep <- function(eta_long,nd1_long,nd2_1_long){
  
  max_nd_partido <- eta_long |> ### possible issue... eta es para 10 partidos. Que onda con el independiente? 
    mutate(max_nd = floor(500*(eta+0.08))) |>
    select(-eta)
  
  nd2_sobrep_long <- nd2_1_long
  # # crear un caso donde si haya sobrerrep
  # nd2_sobrep_long <- nd2_1_long |>
  #   mutate(nd2_1 = if_else(sim==1&id_partido==7,nd2_1+28,nd2_1)) |>
  #   mutate(nd2_1 = if_else(sim==1&id_partido==1,nd2_1-28,nd2_1)) 
  eta2_long <- eta_long
  # solo sobre 10 primeros partidos
  ##  LE hace los siguientes dos pasos 3 veces arbitrariamente... adivino pensando que depues de 3 vueltas se ajustan las nd2
  # meter un while
  stop = FALSE
  while(!stop){
    print(stop)
    sobrerrep <-  nd2_sobrep_long|>
      left_join(nd1_long) |>
      select(sim, id_partido, nombre_partido,nd1,nd2_1) |>
      mutate(across(starts_with("nd"), ~ if_else(is.na(.x),0,.x))) |>
      left_join(max_nd_partido) |>
      mutate(nd = nd1 + nd2_1) |>
      mutate(id_sobrerrep= nd > max_nd) |>
      mutate(cuantas_sobran = if_else(id_sobrerrep,nd-max_nd,0))
    
    ## parar de hacerlo si no hay ningun caso de sobre-representacion? 
    # algo asi como:
    if(sum(sobrerrep$id_sobrerrep) >0) {
      ## copiar logica de LE para entender
      ## volver a repartir proporcionalmente los que sobran... 
      rep_prop_sr1 <- sobrerrep |>
        mutate(nd2_aj1 = if_else(id_sobrerrep,max_nd-nd1,nd2_1))|>
        mutate(pr = if_else(id_sobrerrep,0,max_nd-nd))|>
        mutate(pi = if_else(pr>0,1,0))|>
        #Correccion por rep.prop. negativa
        mutate(nd2_aj2 = if_else(nd2_aj1<0,0,nd2_aj1))|>
        mutate(pr_aj1 = if_else(pr<0,0,pr))|>
        # segundo paso para repartir proporcionalmente
        group_by(sim) |>
        mutate(rtot_aj1 = 200-sum(nd2_aj2*(1-pi)))|>
        left_join(eta2_long)|>
        mutate(eta2 = pi*eta/sum(pi*eta)) |>
        mutate(nd2_aj3 = nd2_aj2*(1-pi)+floor(rtot_aj1*eta2)) |>
        mutate(rm2_aj1 = pi*(rtot_aj1*eta2-floor(rtot_aj1*eta2))) |>
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
