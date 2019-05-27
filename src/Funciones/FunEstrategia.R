# Script.info ---- 
# file: FunEstrategia.R
# date: 24-03-2019
# description: Funciones necesarias para correr el backtesting
#---

# Funciones esenciales ----------------------------------------------------

indSeg <- function(parm = NULL, sigParm = NULL, datos, indName){
  # Calcula señal apartir de un indicador. Se pueden varias tanto los
  # parametros del indicador como los que pueda usar la señal.
  # Argumentos:
  #   parm: lista de parametros del indicador, si esta en NULL toma
  #         los argumentos en la tabla de especificaciones.
  #   sigParm: lista de parametros para la señal, si esta en NULL toma
  #            los argumentos en la tabla de especificaciones.
  #   datos: tabla de datos.
  #   indName: Nombre del indicador en la tabla de especificaciones.
  
  if(!exists("tbl.specs")) # Verifica si existe tbl.specs
    stop('Falta crear la tabla de especificaciones tbl.specs')
  if(!(indName %in% tbl.specs$nameInd)) # Verifica que este el indicador
    stop('No se encuentra el nombre del indicador (indName) en la tabla tbl.specs')
  
  # Corre el indicador y la señal.
  specs <- filter(tbl.specs, nameInd == indName)
  
  if(is.null(parm)) parm <- specs$parms[[1]]
  if(is.null(sigParm)) sigParm <- specs$sig.parms[[1]]
  
  .datf <- do.call(specs$dat.f, list(datos)) # matriz
  .indic <- do.call(specs$f, append(list(.datf), parm)) # matriz
  .signal <- do.call(specs$sig.f, append(list(.indic), sigParm)) # vector
  
  return(.signal)
}


fun.tran <- function(ordenes, datos){
  # Calcula el backtesting apartir de un vector de ordenes.
  # Argumentos:
  #   ordenes: Vector de ordenes o tibble(data_frame) que tenga una columna
  #            llamada 'orden'.
  #   datos: tabla de datos.
  
  if(!is.vector(ordenes)) ordenes <- ordenes$orden
  tbl.dateClose <- datos %>% 
    select(close, date)
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

# Funciones de gráficas ---------------------------------------------------
plotBackTest <- function(backtest, datos, semanas = 2 ){
  # Esta función permite ver mas de cerca las transacciones 
  # del backtesting en la grafíca de los close. 
  # Parametros:
  #   backtest: Salida de la fun.tran, el bactesting.
  #   datos: La tabla de datos.
  #   semanas: Vector o entero de las semanas que se quieren
  #            graficar. El vector se recomienda que sean semanas
  #            consecutivas.
  nsemana <- sprintf("%02d", semanas)
  
  baseFil <- datos %>% 
    filter(format(date, "%V") %in% nsemana) 
  
  tblaux <- backtest %>% 
    mutate(signo = ifelse(sign(retorno)==1,'up','down') %>% factor()) %>% 
    select(-retorno, -cum.retorno) %>% 
    rename(IN=date.entrada, OUT = date.salida) %>% 
    left_join(datos %>% 
                rename(close_in = close) %>% 
                select(date, close_in), by = c('IN' = 'date')) %>% 
    left_join(datos %>% 
                rename(close_out = close) %>% 
                select(date, close_out), by = c('OUT' = 'date'))
  
  tblaux %>% 
    ggplot() +
    geom_line(aes(date, close), datos) +
    geom_point(aes(IN, close_in), colour = 'green', fill = 'green', shape =24) +
    geom_point(aes(OUT, close_out), colour = 'red', fill = 'red', shape =25) +
    geom_segment(aes(x=IN, xend=OUT, y=close_in, yend=close_out,  
                     color = signo), size = 1.3) +
    tidyquant::coord_x_datetime(xlim = c(min(baseFil$date), max(baseFil$date)),
                                ylim = c(min(baseFil$close), max(baseFil$close) ))
}

plotTest <- as_mapper(
  # Crea un plot del retorno acumulado obtenidos del backtesting
  ~.x %>% 
    ggplot(aes(date.entrada, cum.retorno)) +
    geom_line() +
    geom_hline(yintercept = 100))


# Funciones utilidades ----------------------------------------------------

get_indicators <- function(datos){
  # Esta función calcula los indicadores y retorna una tabla con estos
  # puestos en columnas
  # Argumentos: 
  #   datos: la tabla de datos.
  # Nota: Es necesario que se encuentre creada la tabla de especificaciones.
  
  tbl.datf <- tbl.specs %>% # Extrea los datos para los indicadores
    distinct(dat.f) %>% 
    mutate(
      data = map( dat.f, ~eval(parse(text=paste0(.x,'(datos)'))) ) 
    )
  
  apply.indi <- tbl.specs %>% # Corre indicadores
    left_join(tbl.datf, by = "dat.f") %>% 
    mutate(
      parms = map2(parms, data, ~append( list(.y),.x) ),
      indicadores =  invoke_map(f, parms)
    ) %>% 
    mutate(
      indicadores = map(indicadores, as.tibble),
      colNames = map2(indicadores, nameInd, ~ colnames(.x) %>%
                        paste(.y,.,sep = '.')),
      indicadores = map2(indicadores, colNames, magrittr::set_colnames)
    ) %>%
    .$indicadores %>%
    bind_cols()
  
  return(apply.indi)
  
}

runStrategy <- function(parm, sigParm = NULL, datos, 
                        indName, ord.f = Ordenes, ordParm = NULL){
  
  # Esta función corre la estrategia
  # indicador + señal + ordenes
  # y devuelve las transacciones
  
  if(is.numeric(ordParm)) ordParm <- as.list(ordParm) 
  
  signal <- indSeg(parm, sigParm, datos, indName)
  orden <- do.call(ord.f, append(list(signal), ordParm))
  transac <- fun.tran(orden, datos)
  return(transac)
}


# Esta es la antigua función que corre estrategias.
# runStrategy <- function(tblDatos, arg.tbls = c('TpOrd','Trans')){
#   # Argumentos de arg.tbls = c('Indic','Segna','Orden','TpOrd','Trans')
#   
#   tbl.datf <- tbl.specs %>% # Datos para aplicar los indicadores
#     distinct(dat.f) %>% 
#     mutate(datos = map(dat.f, ~eval(parse(text=paste0(.x,'(tblDatos)')))) )
#   
#   apply.specs <- tbl.specs %>% # Calcula indicadores y señales
#     left_join(tbl.datf) %>%
#     mutate(
#       parms = map2(parms,datos, ~append( list(.y),.x) ),
#       indicadores = invoke_map(f, parms),
#       sig.parms = map2(sig.parms, indicadores, ~append( list(.y),.x)),
#       signals = invoke_map(sig.f, sig.parms)
#     )
#   
#   datos.indicadores <- apply.specs %>% # Tabla con los indicadores
#     mutate(indicadores = map(indicadores, as.tibble),
#            colNames = map2(indicadores, nameInd, ~ colnames(.x) %>%
#                              paste(.y,.,sep = '.')),
#            indicadores = map2(indicadores, colNames, magrittr::set_colnames)) %>%
#     .$indicadores %>%
#     bind_cols()
#   
#   tbl.signals <- apply.specs %>% # Tabla con las señales
#     mutate(signals = map(signals, as.tibble),
#            signals = map2(signals, nameInd, magrittr::set_colnames)) %>% 
#     .$signals %>% 
#     bind_cols()
#   
#   tbl.condicsIndic <- tblDatos %>% # Calcula indicadores de las condiciones
#     indicadoresCond()
#   
#   tbl.condics <- tbl.condicsIndic %>% # Tabla de señales de las condiciones
#     condiciones() %>% 
#     select_(.dots = paste0('-',colnames(tbl.condicsIndic))) 
#   
#   datos.signal <- tbl.signals %>% # Combina las señales y las condiciones
#     lapply(., function(x) 
#       lapply(tbl.condics, function(y) 
#         combSig(x,y))) %>% 
#     tibble(first = names(.), dat = .) %>%
#     mutate(second = map(dat, names)) %>% 
#     unnest() %>% 
#     unite('nombre',first, second, sep = '.') %$% 
#     map2_dfc(dat, nombre, ~magrittr::set_colnames(as.tibble(.x),.y)) %>% 
#     bind_cols(tbl.signals, .)
#   
#   datos.signal <- verificaTipo(datos.signal) # Realiza una verificación del tipo de señal en datos.signal
#   
#   tbl.loss <- stopLoss %>% # Es la tabla con los porcentajes Loss
#     filter(period == periodo) %>% 
#     select(-period) %>%  
#     left_join(apply.specs %>% 
#                 select(nameInd, f), by = c('indicador'='f') ) %>% 
#     filter(!is.na(nameInd)) %>% 
#     select(-indicador) %>% 
#     mutate(porLoss = abs(porLoss)/100)
#   
#   price <- cl(tblDatos) %>% pull() # Los precios 
#   
#   tbl.orden0 <- datos.signal %>% # Calcula ordenes apartir de señales
#     gather(nameInd, signal) %>% 
#     nest(-nameInd) %>% 
#     separate(nameInd, c('nameInd2','condic'), '\\.',
#              remove = F, fill = 'right') %>% 
#     left_join(tbl.loss, by = c('nameInd2' = 'nameInd')) %>% 
#     select(-nameInd2, -condic) %>% 
#     mutate(
#       order = map2(data, porLoss, Ordenes, price)
#     )
#   
#   tbl.orden <- tbl.orden0 %>% # Organiza la tabla tbl.orden0
#     select(-data) %>%
#     mutate(SL = map(order, names)) %>% 
#     unnest() %>% 
#     mutate(tp = map(order, names),
#            nameInd = paste0(nameInd, SL)) %>% 
#     unnest()
#   
#   datos.ordenes <- tbl.orden %>% # Tabla de las ordenes en lista-fila 
#     filter(tp == 'orden') %>% 
#     mutate(order = map2(order, nameInd, ~tibble(.x) %>%
#                           set_colnames(.y))) %>%
#     .$order %>%
#     bind_cols()
#   
#   datos.ordenesTP <- tbl.orden %>% # Tabla de tipo de las ordenes
#     filter(tp == 'tpOrden') %>%
#     mutate(order = map2(order, nameInd, ~tibble(.x) %>%
#                           set_colnames(.y))) %>%
#     .$order %>%
#     bind_cols()
#   
#   tbl.dateClose <- tblDatos %>%
#     select(date, close)
#   
#   tbl.transac <- tbl.orden %>% # datos de transacciones
#     filter(tp == 'orden') %>% 
#     mutate(trans = map(order, fun.tran, tbl.dateClose)) %>% 
#     select(nameInd, trans)
#   
#   # Esto es para retornar un tibble con las tablas necesitadas
#   vec.tablas <- c('Indic','Segna','Orden','TpOrd','Trans')
#   nombre.tbls <- c(
#     'datos.indicadores',
#     'datos.signal',
#     'datos.ordenes',
#     'datos.ordenesTP',
#     'tbl.transac' )
#   
#   text.output <- nombre.tbls[vec.tablas %in% arg.tbls] %>% 
#     paste(arg.tbls,'=','list(',.,')') %>% 
#     paste(collapse = ', ') %>% 
#     paste0('tibble(',.,')')
#   
#   return(eval(parse(text=text.output)))
# }


# funcion auxiliar --------------------------------------------------------

# verificaTipo <- function(datos.signal){
#   # Esta es una funcion que permite verificar si existe alguna señal en la base
#   # datos.signal que no sea continua o discontinua, y revisa que las señales 
#   # generadas de un indicador y una condicion deba ser del tipo discontinua
#   # si resulta que es continua la saca de las señales (esto debido a que
#   # no esta haciendo nada al aplicarle la función vencomDis).
#   # No es tan fundamental llamarla despues de crear la tabla datos.signal
#   
#   who.aux <- datos.signal %>% # cuenta cuantos datos unicos distinto != NA
#     map_dbl( ~  sum(!is.na(unique( . ))) ) %>%
#     tibble(name_ind = names(.), cant = .)
#   
#   who.cont <- who.aux %>%
#     filter(cant == 2) %>% 
#     pull(name_ind)
#   
#   message('Los indicadores identificados como continuos son:','\n',
#           '   ', paste(who.cont, collapse = ', '))
#   who.logi <- str_detect(who.cont, '\\.')
#   if(any(who.logi)){
#     who.NOcont <- who.cont[who.logi]
#     message('WARNING: Se descartaron los siguientes indicadores que no son continuas:',
#             '\n', paste(who.NOcont, collapse = ', ') )
#     if( any(who.NOcont %in% colnames(datos.signal)) ){
#       datos.signal %<>%
#         select_(.dots = paste0('-',who.NOcont ))
#     }
#   }
#   
#   if(any(!(unique(who.aux$cant) %in% c(2,3)))){
#     who.NOis <- who.aux$name_ind[ !(who.aux$cant %in% c(2,3)) ]
#     cont.NOis <- who.aux$cant[ !(who.aux$cant %in% c(2,3)) ]
#     
#     message('WARNING: Se descartaron los siguientes indicadores que no son ni continuos
#           o discontinuos',
#             '\n', paste(paste0(who.NOis,'-',cont.NOis), collapse = ', ') )
#     if(who.NOis %in% colnames(datos.signal)){
#       datos.signal %<>%
#         select_(.dots = paste0('-',who.NOis ))
#     }
#   }
#   # Termina mensaje
#   return(datos.signal)
# }

# ,
# close = tsibble::tile_dbl(close, ~.x[1], .size = 2) %>% 
#   rep(each=2),
# retBruto = retorno*close


