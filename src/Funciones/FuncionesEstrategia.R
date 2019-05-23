vencomDis <- function(x){
  long  <- length(x)
  orden   <- rep(9, long)
  ab <- 0
  
  for(ii in 2:long){
    if( !is.na(x[ii]) & !is.na(x[ii-1]) ) {
      # Para comprar  
      if(ab == 0){
        if(x[ii] == 0 & x[ii-1] == 1){
          orden[ii] <- 1
          ab <- 1
        }
      }
      # Para vender
      if(ab == 1){
        if(x[ii] == 0 & x[ii-1] == -1){
          orden[ii]   <- 0
          ab = 0
        }
      }
    }
  }
  # return(list(orden = orden, tpOrden = rep(NA,length(orden))))
  return(orden)
}

# # -- Función con señales continuas
vencomCon <- function(x){
  long <- length(x)
  orden   <- rep(9, long)
  ab <- 0
  
  for(ii in 2:long){
    if(!is.na(x[ii]) & !is.na(x[ii-1])){ 
      # Para comprar  
      if(ab == 0){
        if(x[ii] == 1 & x[ii-1] == -1){
          orden[ii] <- 1
          ab <- 1
        }
      }
      # Para vender 
      if(ab == 1){
        if(x[ii] == -1 & x[ii-1] ==  1){
          orden[ii] <- 0
          ab <- 0
        }
      }
    }
  }
  # return(list(orden = orden, tpOrden = rep(NA,length(orden))))
  return(orden)
}

# # -- Corre ordenes 
OrdenesSinSL <- function(vec.signal, price){
  signal <- pull(vec.signal)
  
  isCont <- sum(!is.na(unique(signal))) == 2
  
  if(!isCont){# Discontinuas
    infOrdenI <- vencomDis(signal)
  }else{# Continuas
    infOrdenI <- vencomCon(signal)
  }
  return(infOrdenI)
}

################################################################################
# # Funciones de ordenes: Compra y Venta CV
################################################################################
# # -- Función con señales discontinuas
vencomDis <- function(x){
  long  <- length(x)
  orden   <- rep(9, long)
  tpOrden <- rep("NN", long)
  ab <- 0
  
  for(ii in 2:long){
    if( !is.na(x[ii]) & !is.na(x[ii-1]) ) {
      # Para comprar  
      if(ab == 0){
        if(x[ii] == 0 & x[ii-1] == 1){
          orden[ii] <- 1
          ab <- 1
        }
      }
      # Para vender
      if(ab == 1){
        if(x[ii] == 0 & x[ii-1] == -1){
          orden[ii]   <- 0
          tpOrden[ii] <- "IN" 
          ab = 0
        }
      }
    }
  }
  return(list(orden = orden, tpOrden = tpOrden))
}

# # -- Función con señales continuas
vencomCon <- function(x){
  long <- length(x)
  orden   <- rep(9, long)
  tpOrden <- rep("NN", long)
  ab <- 0
  
  for(ii in 2:long){
    if(!is.na(x[ii]) & !is.na(x[ii-1])){ 
      # Para comprar  
      if(ab == 0){
        if(x[ii] == 1 & x[ii-1] == -1){
          orden[ii] <- 1
          ab <- 1
        }
      }
      # Para vender 
      if(ab == 1){
        if(x[ii] == -1 & x[ii-1] ==  1){
          orden[ii] <- 0
          tpOrden[ii] <- "IN" 
          ab <- 0
        }
      }
    }
  }
  return(list(orden = orden, tpOrden = tpOrden))
}

################################################################################
# # Ordenes de compra y venta - Stop loss Take profit
################################################################################
# # -- Función con señales discontinuas
vencomDisSLTP <- function(signals, price, porLoss, porProf, fee){
  long    <- length(signals)
  orden   <- rep(9, long)
  tpOrden <- rep("NN", long)
  
  ab <- 0
  
  for(ii in 2:long){
    if(!is.na(signals[ii]) & !is.na(signals[ii-1])) {
      # Para comprar  
      if(ab == 0){
        if(signals[ii] == 0 & signals[ii-1] == 1){
          orden[ii] <- 1
          valBuy <- price[ii]
          valSellTP <- valBuy * (1 + porProf + fee)
          valSellSL <- valBuy * (1 - porLoss)
          ab <- 1
        }
      }      
      # Para vender
      if(ab == 1){
        condInd <- signals[ii] == 0 & signals[ii-1] == -1
        condSL <- price[ii] <= valSellSL
        condTP <- price[ii] >= valSellTP
        if(condInd | condSL | condTP){
          orden[ii] <- 0
          if(condInd == TRUE) tpOrden[ii] <- "IN"          
          if(condSL  == TRUE) tpOrden[ii] <- "SL"
          if(condTP  == TRUE) tpOrden[ii] <- "TP"
          ab = 0
        }
      }
    }
  }
  
  return(list(orden = orden, tpOrden = tpOrden))
}

# # -- Función con señales continuas
vencomConSLTP <- function(signals, price, porLoss, porProf, fee){
  
  long  <- length(signals)
  orden <- rep(9, long)
  tpOrden <- rep("NN", long)
  
  ab <- 0
  
  for(ii in 2:long){
    if(!is.na(signals[ii]) & !is.na(signals[ii-1])){ 
      # Para comprar  
      if(ab == 0){
        if(signals[ii] == 1 & signals[ii-1] == -1){
          orden[ii] <- 1
          ab <- 1
          valBuy <- price[ii]
          valSellTP <- valBuy * (1 + porProf + fee)
          valSellSL <- valBuy * (1 - porLoss)
        }
      }
      # Para vender 
      if(ab == 1){
        condInd <- signals[ii] == -1 & signals[ii-1] ==  1
        condSL  <- price[ii] <= valSellSL
        condTP  <- price[ii] >= valSellTP
        if(condInd | condSL | condTP) {
          orden[ii] <- 0
          if(condInd == TRUE) tpOrden[ii] <- "IN"          
          if(condSL  == TRUE) tpOrden[ii] <- "SL"
          if(condTP  == TRUE) tpOrden[ii] <- "TP"
          ab <- 0
        }
      }
    }
  }
  return(list(orden = orden, tpOrden = tpOrden))
}

# funcion corre ordenes ---------------------------------------------------
Ordenes <- function(vec.signal, vloss, price, vfee = fee, vprofit = profit,
                    vloss_gen = 0.999){
  signal <- pull(vec.signal)
  
  isCont <- sum(!is.na(unique(signal))) == 2
  if(is.na(vloss)) vloss <- vloss_gen
  
  if(!isCont){# Discontinuas
    infOrdenI <- vencomDis(signal)
    infOrdenISLTP <- vencomDisSLTP(signal, price, vloss, vprofit, vfee)
  }else{# Continuas
    infOrdenI <- vencomCon(signal)
    infOrdenISLTP <- vencomConSLTP(signal, price, vloss, vprofit, vfee)
  }
  
  list.return <- list(infOrdenI, SLTP = infOrdenISLTP)
  return(list.return)
}


# funcion transacciones ---------------------------------------------------
# Esta funcion saca una tabla de las transacciones en base a un vector 
# de ordenes 

fun.tran <- function(ordenes, tbl.dateClose){
  ordenes %>% 
    tibble(orden = .) %>% 
    bind_cols(tbl.dateClose, .) %>% 
    filter(orden %in% c(1,0)) %>% 
    mutate(tran.num = cumsum(orden),
           orden = ifelse(orden == 0, 1, -1)) %>% 
    filter(tran.num != 0) %>% 
    # quita la ultima si es una transaccion abierta
    filter(ifelse((seq_along(close)==nrow(.))&(orden==-1), F, T)) %>% 
    mutate(close = close*(1-orden*fee),
           retorno = tsibble::tile_dbl(close, ~(.x[2]/.x[1]-1), .size = 2) %>% 
             rep(each = 2)) %>% 
    select(-close) %>%
    mutate(orden = ifelse(orden == -1, 'date.entrada','date.salida')) %>% 
    spread(orden, date) %>% 
    mutate(cum.retorno = vInver*cumprod(retorno+1))
}


# funcion corre estrategia  -----------------------------------------------
# Esta funcion corre las estrategias
runStrategy <- function(tblDatos, arg.tbls = c('TpOrd','Trans')){
  # Argumentos de arg.tbls = c('Indic','Segna','Orden','TpOrd','Trans')
  
  tbl.datf <- tbl.specs %>% # Datos para aplicar los indicadores
    distinct(dat.f) %>% 
    mutate(datos = map(dat.f, ~eval(parse(text=paste0(.x,'(tblDatos)')))) )
  
  apply.specs <- tbl.specs %>% # Calcula indicadores y señales
    left_join(tbl.datf) %>%
    mutate(
      parms = map2(parms,datos, ~append( list(.y),.x) ),
      indicadores = invoke_map(f, parms),
      sig.parms = map2(sig.parms, indicadores, ~append( list(.y),.x)),
      signals = invoke_map(sig.f, sig.parms)
    )
  
  datos.indicadores <- apply.specs %>% # Tabla con los indicadores
    mutate(indicadores = map(indicadores, as.tibble),
           colNames = map2(indicadores, nameInd, ~ colnames(.x) %>%
                             paste(.y,.,sep = '.')),
           indicadores = map2(indicadores, colNames, magrittr::set_colnames)) %>%
    .$indicadores %>%
    bind_cols()
  
  tbl.signals <- apply.specs %>% # Tabla con las señales
    mutate(signals = map(signals, as.tibble),
           signals = map2(signals, nameInd, magrittr::set_colnames)) %>% 
    .$signals %>% 
    bind_cols()
  
  tbl.condicsIndic <- tblDatos %>% # Calcula indicadores de las condiciones
    indicadoresCond()
  
  tbl.condics <- tbl.condicsIndic %>% # Tabla de señales de las condiciones
    condiciones() %>% 
    select_(.dots = paste0('-',colnames(tbl.condicsIndic))) 
  
  datos.signal <- tbl.signals %>% # Combina las señales y las condiciones
    lapply(., function(x) 
      lapply(tbl.condics, function(y) 
        combSig(x,y))) %>% 
    tibble(first = names(.), dat = .) %>%
    mutate(second = map(dat, names)) %>% 
    unnest() %>% 
    unite('nombre',first, second, sep = '.') %$% 
    map2_dfc(dat, nombre, ~magrittr::set_colnames(as.tibble(.x),.y)) %>% 
    bind_cols(tbl.signals, .)
  
  datos.signal <- verificaTipo(datos.signal) # Realiza una verificación del tipo de señal en datos.signal
  
  tbl.loss <- stopLoss %>% # Es la tabla con los porcentajes Loss
    filter(period == periodo) %>% 
    select(-period) %>%  
    left_join(apply.specs %>% 
                select(nameInd, f), by = c('indicador'='f') ) %>% 
    filter(!is.na(nameInd)) %>% 
    select(-indicador) %>% 
    mutate(porLoss = abs(porLoss)/100)
  
  price <- cl(tblDatos) %>% pull() # Los precios 
  
  tbl.orden0 <- datos.signal %>% # Calcula ordenes apartir de señales
    gather(nameInd, signal) %>% 
    nest(-nameInd) %>% 
    separate(nameInd, c('nameInd2','condic'), '\\.',
             remove = F, fill = 'right') %>% 
    left_join(tbl.loss, by = c('nameInd2' = 'nameInd')) %>% 
    select(-nameInd2, -condic) %>% 
    mutate(
      order = map2(data, porLoss, Ordenes, price)
    )
  
  tbl.orden <- tbl.orden0 %>% # Organiza la tabla tbl.orden0
    select(-data) %>%
    mutate(SL = map(order, names)) %>% 
    unnest() %>% 
    mutate(tp = map(order, names),
           nameInd = paste0(nameInd, SL)) %>% 
    unnest()
  
  datos.ordenes <- tbl.orden %>% # Tabla de las ordenes en lista-fila 
    filter(tp == 'orden') %>% 
    mutate(order = map2(order, nameInd, ~tibble(.x) %>%
                          set_colnames(.y))) %>%
    .$order %>%
    bind_cols()
  
  datos.ordenesTP <- tbl.orden %>% # Tabla de tipo de las ordenes
    filter(tp == 'tpOrden') %>%
    mutate(order = map2(order, nameInd, ~tibble(.x) %>%
                          set_colnames(.y))) %>%
    .$order %>%
    bind_cols()
  
  tbl.dateClose <- tblDatos %>%
    select(date, close)
  
  tbl.transac <- tbl.orden %>% # datos de transacciones
    filter(tp == 'orden') %>% 
    mutate(trans = map(order, fun.tran, tbl.dateClose)) %>% 
    select(nameInd, trans)
  
  # Esto es para retornar un tibble con las tablas necesitadas
  vec.tablas <- c('Indic','Segna','Orden','TpOrd','Trans')
  nombre.tbls <- c(
    'datos.indicadores',
    'datos.signal',
    'datos.ordenes',
    'datos.ordenesTP',
    'tbl.transac' )
  
  text.output <- nombre.tbls[vec.tablas %in% arg.tbls] %>% 
    paste(arg.tbls,'=','list(',.,')') %>% 
    paste(collapse = ', ') %>% 
    paste0('tibble(',.,')')
  
  return(eval(parse(text=text.output)))
}


# funcion auxiliar --------------------------------------------------------

verificaTipo <- function(datos.signal){
  # Esta es una funcion que permite verificar si existe alguna señal en la base
  # datos.signal que no sea continua o discontinua, y revisa que las señales 
  # generadas de un indicador y una condicion deba ser del tipo discontinua
  # si resulta que es continua la saca de las señales (esto debido a que
  # no esta haciendo nada al aplicarle la función vencomDis).
  # No es tan fundamental llamarla despues de crear la tabla datos.signal
  
  who.aux <- datos.signal %>% # cuenta cuantos datos unicos distinto != NA
    map_dbl( ~  sum(!is.na(unique( . ))) ) %>%
    tibble(name_ind = names(.), cant = .)
  
  who.cont <- who.aux %>%
    filter(cant == 2) %>% 
    pull(name_ind)
  
  message('Los indicadores identificados como continuos son:','\n',
          '   ', paste(who.cont, collapse = ', '))
  who.logi <- str_detect(who.cont, '\\.')
  if(any(who.logi)){
    who.NOcont <- who.cont[who.logi]
    message('WARNING: Se descartaron los siguientes indicadores que no son continuas:',
            '\n', paste(who.NOcont, collapse = ', ') )
    if( any(who.NOcont %in% colnames(datos.signal)) ){
      datos.signal %<>%
        select_(.dots = paste0('-',who.NOcont ))
    }
  }
  
  if(any(!(unique(who.aux$cant) %in% c(2,3)))){
    who.NOis <- who.aux$name_ind[ !(who.aux$cant %in% c(2,3)) ]
    cont.NOis <- who.aux$cant[ !(who.aux$cant %in% c(2,3)) ]
    
    message('WARNING: Se descartaron los siguientes indicadores que no son ni continuos
          o discontinuos',
            '\n', paste(paste0(who.NOis,'-',cont.NOis), collapse = ', ') )
    if(who.NOis %in% colnames(datos.signal)){
      datos.signal %<>%
        select_(.dots = paste0('-',who.NOis ))
    }
  }
  # Termina mensaje
  return(datos.signal)
}

# ,
# close = tsibble::tile_dbl(close, ~.x[1], .size = 2) %>% 
#   rep(each=2),
# retBruto = retorno*close
