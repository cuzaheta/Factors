# Script.info ---- 
# file: TunningEstrategias.R
# date: 29-01-2019
# input: Datos de las monedas
# output: Resultados del tunning
# description: 
#---
rm(list = ls()) # ls(all.names = TRUE)

# packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(TTR)
# library(rPref)
library(lubridate)
library(tsibble)

# path --------------------------------------------------------------------
inPath <- file.path('input')
outPath <- file.path('output')
srcPath <- file.path('src')
inFunct <- file.path(srcPath,'Funciones')
inStrat <- file.path(outPath, 'Estrategias')

# functions ---------------------------------------------------------------
c('Reglas.R','FuncionesBacktest.R', 'Ordenes.R') %>% 
  file.path(inFunct, .) %>% 
  map(source) %>% 
  invisible()

# Funciones para sacar resumen estrategia
# winsorF <- function (x, fraction=.05, func = mean){
#   if(length(fraction) != 1 || fraction < 0 || fraction > 0.5) { 
#     stop("bad value for 'fraction'")
#   }
#   lim <- quantile(x, probs=c(fraction, 1-fraction))
#   x[ x < lim[1] ] <- lim[1]
#   x[ x > lim[2] ] <- lim[2]
#   func(x)
# }
resumen <- function(tblTransaction, p.extre = 0.1){
  retors <- tblTransaction$retorno
  
  N.trans <- max(tblTransaction$tran.num)
  profit <- tblTransaction$cum.retorno[N.trans]
  N.gan <- sum(retors %>% sign() > 0)
  
  x.pos <- retors[retors %>% sign() > 0]
  x.neg <- retors[retors %>% sign() < 0]
  
  tbl.general <- tibble(retors) %>% 
    summarise(
      median.gan = median(x.pos),
      meantrim.per = mean(x.neg[x.neg>quantile(x.neg, c(p.extre))]),
      meantrim.ret = mean(retors, p.extre),
      iqr.ret = IQR(retors)
    ) %>% 
    add_column(N.trans, profit, N.gan)
  
  tbl.time <- tblTransaction %>% 
    mutate(tiempo = date.salida - date.entrada) %>% 
    summarize(time.median = median(tiempo),
              time.iqr = IQR(tiempo))
  
  tbl.mes <- tblTransaction %>% 
    mutate(mes = format(date.entrada, "%Y-%m")) %>% 
    group_by(mes) %>% 
    summarise(retorno = prod(retorno+1), N = n()) %>% 
    summarise(
      retmes.quant90 = quantile(retorno, 0.9),
      mes.median = median(retorno),
      mes.sd = sd(retorno),
      Nmes.mean = mean(N)
    )
  
  return(bind_cols(tbl.general,tbl.mes,tbl.time))
}

# Funciones de plots
plotBackTest <- function(backtest, datos, nsemana = c('02')){
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

plotTest <- as_mapper(~.x %>%
                        ggplot(aes(date.entrada, cum.retorno)) +
                        geom_line() +
                        geom_hline(yintercept = 100))



runIndicador <- function(Params, Sig.parms = NULL, tblDatos, indicName){
  tbl.specsJun <- tbl.specsDefault %>%
    filter(f == indicName) %>%
    mutate(parms = list(Params),
           sig.parms = ifelse(is.null(Sig.parms),
                              sig.parms,
                              list(Sig.parms)))
  
  tbl.datf <- tbl.specsJun %>% # Datos para aplicar los indicadores
    distinct(dat.f) %>%
    mutate(datos = map(dat.f, ~eval(parse(text=paste0(.x,'(tblDatos)')))) )
  
  apply.specs <- tbl.specsJun %>% # Calcula indicadores y señales
    left_join(tbl.datf, by = 'dat.f') %>%
    mutate(
      parms = map2(parms, datos, ~append( list(.y),.x) ),
      indicadores = invoke_map(f, parms),
      sig.parms = map2(sig.parms, indicadores, ~append( list(.y),.x)),
      signals = invoke_map(sig.f, sig.parms)
    )
  
  vectorSig <- apply.specs$signals[[1]] %>% tibble()
  
  price <- tblDatos %>% select(close) %>% pull()
  
  vec.orden <- OrdenesSinSL(vectorSig, price)
  
  tbl.dateClose <- tblDatos %>%
    select(date, close)
  
  return(fun.tran(vec.orden, tbl.dateClose))
  
}
# Funciones SL movil
runSignal <- function(Params, Sig.parms = NULL, tblDatos, indicName){
  tbl.specsJun <- tbl.specsDefault %>% # tabla para correr indicadores y señales
    filter(f == indicName) %>% 
    mutate(parms = list(Params),
           sig.parms = ifelse(is.null(Sig.parms),
                              sig.parms,
                              list(Sig.parms)))
  
  tbl.datf <- tbl.specsJun %>% # Datos para aplicar los indicadores
    distinct(dat.f) %>% 
    mutate(datos = map(dat.f, ~eval(parse(text=paste0(.x,'(tblDatos)')))) )
  
  apply.specs <- tbl.specsJun %>% # Calcula indicadores y señales
    left_join(tbl.datf, by = 'dat.f') %>%
    mutate(
      parms = map2(parms, datos, ~append( list(.y),.x) ),
      indicadores = invoke_map(f, parms),
      sig.parms = map2(sig.parms, indicadores, ~append( list(.y),.x)),
      signals = invoke_map(sig.f, sig.parms)
    )
  
  vectorSig <- apply.specs$signals[[1]] %>% tibble(vecSig = .)
  
  return(tblDatos %>% 
           select(date, close, ma, maRet) %>% 
           bind_cols(vectorSig))
}
SLprofit <- function(tblSig, vloss = NA){
  vec.orden <- tblSig %>% 
    select(close, ma, maRet) %>% 
    OrdenesSLMovil(tblSig %>% select(vecSig), ., vloss) 
  
  tbl.dateClose <- tblSig %>%
    select(date, close)
  # return(last(fun.tran(vec.orden, tbl.dateClose)$cum.retorno))
  return(fun.tran(vec.orden, tbl.dateClose))
}


# load --------------------------------------------------------------------
# entra <- file.path(inPath, "StopLoss.txt")
# stopLoss <- read.delim2(entra, sep = "\t", header = TRUE, 
#                         stringsAsFactors = FALSE)

# stopLoss <- tribble(
#   ~period,	~indicador,	~porLoss,
#   'D',	'ADX',	-4.5
# )

# specs -------------------------------------------------------------------
# En esta parte se realizan las especificaciones de los indicadores y
# y parametros por default
tbl.specsDefault <- tribble( 
  ~f, ~dat.f, ~sig.f, ~sig.parms, 
  'BBands', 'hlc',  'sig.BBands', list(inf=0, sup=1),
  'RSI', 'cl',  'sigThresh', list(inf=30, sup=70), 
  'SARM', 'hlc',  'sig.SAR3', list(), # sig.SAR \ sig.SAR3 
  'ADX', 'hlc',  'sig.ADXL', list(), # sig.ADX \ sig.ADXL
  'MACD', 'cl', 'sig.MACD', list(),
  'SMIM', 'hlc',  'sig.SMI', list(), 
  'SMAtwo', 'cl', 'sig.SMAtwo', list()
) # '', '', '', list(), '', list()




# Incluir SAR - ADX, SAR dice la direccion y ADX confirma
# MACD-histogram con limites despues de cierto numero entrar a comprar,
#   no solo con el cero y salir solo con stop-loss movil


# variables ---------------------------------------------------------------
fee <- 0.002
# profit <- 10
vInver <- 100

minTrans <- 100


# lectura datos -----------------------------------------------------------
periodo1 <- '30M'

entra <- paste0(inPath, "/dataPoloniexSel", periodo1, "NTop.RData")
load(entra) # dataTop

# ponerlo en otro script ---------
tbl.datos <- tibble(name=names(dataSel) %>%
                      str_replace(paste0('T',periodo1,'\\|'), ''),
                    data=dataSel) %>%
  mutate(data = map(data, select, -period, -pair),
         data = map(data, ~as_tibble(.x) %>%
                      mutate(date = lubridate::ymd_hms(date))))

rm(dataSel)
# ponerlo en otro script ---------

# periodo2 <- '4H'
# entra <- paste0(inPath, "/dataPoloniexSel", periodo2, "NTop.RData")
# load(entra) # dataTop
# 
# # ponerlo en otro script ---------
# tbl.datos2 <- tibble(name=names(dataSel) %>%
#                       str_replace(paste0('T',periodo2,'\\|'), ''),
#                     data=dataSel) %>%
#   mutate(data = map(data, select, -period, -pair),
#          data = map(data, ~as_tibble(.x) %>%
#                       mutate(date = lubridate::ymd_hms(date))))
# # ponerlo en otro script ---------

rm(dataSel)

# mirando suavisados 4h con 6 datos, 30min con 48 datos

# base5.1 <- tbl.datos$data[[5]] %>%
#   left_join(tbl.datos2$data[[5]] %>% 
#               mutate(flag = T) %>% 
#               select(date, flag) 
#   ) %>% 
#   mutate(close4h = ifelse(flag, close, NA)) 
# 
# base5.2 <- base5.1 %>% 
#   filter(flag) %>% 
#   mutate(close4 = SMA(close4h, 6)) %>% 
#   select(date, close4)
#   
# base5 <- base5.1 %>% 
#   left_join(base5.2) %>% 
#   mutate(close4 = zoo::na.locf0(close4))
# 
# base5 %>% 
#   mutate(close = SMA(close, 53)) %>% 
#   filter(date(date) == ymd('2018-01-31')) %>% 
#   ggplot() +
#   geom_line(aes(date, close)) +
#   geom_line(aes(date, close4), colour = 'purple') +
#   geom_vline(xintercept = base5 %>% 
#                filter(flag, date(date) == ymd('2018-01-31')) %>% 
#                pull(date))


# Data --------------------------------------------------------------------
# "2018-01-01 UTC"
# "2019-01-31 UTC"

# Para graficar las estrategias 

test01 <- runIndicador(list(n = 10, sd = 2.4), NULL, base5, 'BBands')
test02 <- runIndicador(list(n = 43, sd = 1.8), NULL, base5, 'BBands')

plotBackTest(test01, base5, sprintf("%02d", 15:18))

# Cantidad de semanas
base5 %$% 
  unique(format(date, "%V"))

# Observando la serie
tbl.datos$data[[5]] %>% 
  select(date, close) %>% 
  mutate(retorno = ROC(close)) %>% 
  group_by(format(date, "%V")) %>% 
  summarise(retMes = sum(retorno, na.rm = T), date = min(date)) %>% 
  mutate(Acum = cumsum(retMes)) %>% 
  ggplot(aes(date, Acum)) +
  geom_line()

tbl.datos$data[[2]] %>% 
  ggplot(aes(date, close)) +
  geom_line()


# SLMovil
# 
# tblSL1 <- runSignal(list(accel = c(0.01, 0.1)), list(dots = 6),
#                     base5, 'SARM')
# 
# SLprofit(tblSL1, 0.99) 
# 
# tictoc::tic() # Cambiar salida de tblSL1, si no, no corre
# ejemplo1 <- tibble(ite = seq(0, 0.15, 0.01)) %>% 
#   mutate( profit = furrr::future_map_dbl(ite, ~SLprofit(tblSL1, .x)))
# tictoc::toc()
# 
# ejemplo1 %>% 
#   ggplot(aes(ite, profit)) +
#   geom_line() 


# pasaStrategy -----------------------------------------------------------

# Para pasarle al stop loss movil - necesita unas variables fijas en la base
# en este caso necesita un promedio movil (columna llamada ma) y los 
# retorno de estos promedio moviles (maRet)
base5 <- tbl.datos$data[[5]] %>% 
  mutate(ma = EMA(close, 5),
         maRet = ROC(ma))

tbl.datos$data[[3]] %>% 
  ggplot(aes(date, close))+
  geom_line()
mutate(date, close)

# plots -------------------------------------------------------------------
# # # # Esto es BBands 
base5Bb <- base5 %>%
  bind_cols(BBands(hlc(.)) %>% as_tibble()) %>% 
  bind_cols(MACD(cl(.)) %>% as_tibble()) %>% 
  mutate(
    macdHist = (macd - signal)
  ) %>% 
  bind_cols(tibble(rsi = RSI(cl(.))))

plot2 <- as_mapper(
  ~base5Bb %>% 
    plot_bandas(dn, mavg, up, .x) /
   base5Bb %>% 
    plot_oscilador(pctB, .x, c(0,1)) /
   base5Bb %>% 
    plot_oscilador(rsi, .x, c(30,50,70))
)

plot2(10:11) *
  labs(x='') *
  theme_light() +
  plot_layout(heights = c(2,1,1))


lag(1:10, 2)
lead(1:10, 2) # for the Slow SMA

# Leading a slow mean 
# base5 %>%
#   mutate(meanFree = EMA(close, 100),
#     meanFast = lead(SMA(close, 250),50) ) %>% 
#   filter(format(date, "%V") %in% sprintf("%02d", 34:46)) %>%
#   ggplot() +
#   geom_line(aes(date, close)) +
#   geom_line(aes(date, meanFree), colour = 'blue') +
#   geom_line(aes(date, meanFast), colour = 'red')

# # # # two SMA plots
tbl2 <- base5 %>%
  mutate(
    meanFree = EMA(close, 3),
    meanSlow = SMA(close, 15),
    meanFast = SMA(close, 64),
    meanDif= meanSlow - meanFast,
    adx = ADX(hlc(.), 40)[,'ADX'],
    rsi = RSI(close)
  )

plot1 <- as_mapper(
  ~tbl2 %>% 
    plot_3means(meanSlow, meanFast, meanFree, .x) /
    tbl2 %>% 
    plot_oscilador(adx, .x, 30) /
    trans %>% 
    plotBackTest(base5, .x) +
    theme(legend.position="none")
)

# Semanas "rango" 10,11
# No entro en la semana 33
plot1(10:11) * 
  labs(x='') * 
  theme_light() +
  plot_layout(heights = c(2,2,1))

# TWO-SMA -----------------------------------------------------------------

# Old
fast <- seq(2, 70, by = 2)
slow <- seq(50, 250, by = 2)

tablaSMA <- expand.grid(fast,
                        slow) %>%
  set_colnames(c('fast','slow')) %>% 
  as_tibble() %>% 
  filter(slow >= (fast+4)) %>% 
  mutate(listArg = pmap(list(fast, slow), 
                        ~list(nfast = ..1, nslow = ..2)))

tictoc::tic()
tablaSMA20 <-tablaSMA %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runStrategy,
                                          NULL,
                                          base5,
                                          'smaTwo'))

tablaSMA2 <- tablaSMA20 %>% 
  mutate(estadisticas = furrr::future_map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 3.2 min

tbl01 <- tablaSMA2 %>% 
  select(-listArg) %>% 
  mutate_all(as.numeric) %>% 
  mutate(ta.trans = N.gan/(N.trans-N.gan))





# tbl01 %>%
#   mutate_all(as.numeric) %>%
#   mutate(tasa.Ngan = N.gan/(N.trans-N.gan),
#          name = paste0(fast,slow,collapse = '!')) %>%
#   write_csv('CoordinatePlot/tbl01.csv')

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.mean = as.numeric(time.mean),
         time.final = (time.mean - median(time.mean))^2 ) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(median.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(nfast = 46, nslow = 50), NULL, base5, 'SMAtwo')
plotTest(test0)

# maximo profit
test0 <- runIndicador(list(nfast = 32, nslow = 64), NULL, base5, 'SMAtwo')
plotTest(test0)


# Bbands ------------------------------------------------------------------
# No sirve con stop loss, empieza a entrar en mas transacciones en perdidas 
Nn <- seq(4,50,1)
Ssd <- seq(1,3.5,0.1)

tablaBandas <- expand.grid(Nn,
            Ssd) %>%
  set_colnames(c('n','sd')) %>% 
  as_tibble() %>% 
  mutate(listArg = pmap(list(n, sd), 
                        ~list(n = ..1, sd = ..2)))

tictoc::tic()
tablaBandas20 <-tablaBandas %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runIndicador,
                                          tblDatos=base5,
                                          indicName='BBands'))
tablaBandas2 <- tablaBandas20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 5.55 minutos

tbl01 <- tablaBandas2 

tbl02 <- tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(n = 43, sd = 1.8), NULL, base5, 'BBands')
# maximo profit
test0 <- runIndicador(list(n = 10, sd = 2.4), NULL, base5, 'BBands')


# MACD --------------------------------------------------------------------
# Traders use the MACD’s histogram to identify when bullish or 
# bearish momentum is high.
# Es parecido a las diferencia de SMA. 

fast <- seq(2, 50, by = 2)
slow <- seq(4, 110, by = 4)
sing <- seq(6, 20, by = 2)

tablaMACD <- expand.grid(fast,
                         slow,
                         sing) %>%
  set_colnames(c('fast','slow','sig')) %>% 
  as_tibble() %>% 
  filter(slow >= (fast+4)) %>% 
  mutate(listArg = pmap(list(fast, slow, sig), 
                        ~list(nFast = ..1, nSlow = ..2, nSig = ..3)))

tictoc::tic()
tablaMACD20 <-tablaMACD %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runIndicador,
                                          tblDatos=base5,
                                          indicName='MACD'))
tablaMACD2 <- tablaMACD20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 16 minutos / 13 minutos

tbl01 <- tablaMACD2 

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)


tbl01 %>% 
  select(time.mean) %>% 
  mutate(time.final = as.numeric(time.mean - 15*30)^2) %>% 
  ggplot(aes(time.mean, time.final)) +
  geom_line()

tbl01 %>% 
  mutate(ta.trans = N.gan/(N.trans-N.gan)) %>% 
  ggplot(aes(profit, ta.trans)) +
  geom_point()


# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(nFast = 14, nSlow = 56, nSig = 6), NULL, base5, 'MACD')

# maximo profit
test0 <- runIndicador(list(nFast = 50, nSlow = 108, nSig = 20), NULL, base5, 'MACD')



# ADX ---------------------------------------------------------------------
# ADX is used to quantify trend strength
# # the two directional movement indicator (DMI) lines
# When the +DMI is above the -DMI, prices are moving up, and ADX measures 
# the strength of the uptrend. When the -DMI is above the +DMI, prices are 
# moving down, and ADX measures the strength of the downtrend.

# 'ADX', 'hlc',  'sig.ADX', list(),

nADX  <- 4:100

tablaADX <- tibble(nADX) %>% 
  mutate(listArg = map(nADX, ~list(n = .x))) 

tictoc::tic()
tablaADX20 <-tablaADX %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runStrategy,
                                          datos=base5,
                                          indName='adx'))
tablaADX2 <- tablaADX20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 17.197 sec elapsed

tbl01 <- tablaADX2 

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.mean = as.numeric(time.mean),
         time.final = (time.mean - quantile(time.mean, c(.3)))^2 ) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(n = 36), NULL, base5, 'ADX') # 7%
plotTest(test0)

# maximo profit
test0 <- runIndicador(list(n = 91), NULL, base5, 'ADX') # 22.8%
plotTest(test0)



# RSI ---------------------------------------------------------------------
# 
# 'RSI', 'cl',  'sigThresh', list(inf=30, sup=70),

nRSI <- 4:50
constLim <- seq(6, 40, by = 2)

tablaRSI <- expand.grid(nADX, constLim) %>% 
  set_colnames(c('n','cons')) %>% 
  mutate(linf = cons, lsup = 100-cons) %>% 
  mutate(listArg = map(n, ~list(n=.x)),
         SigArg = map2(linf, lsup,  ~list(inf = .x, sup= .y))) 

# Sig.parms = NULL

tictoc::tic()
tablaRSI20 <- tablaRSI %>% 
  mutate(estadisticas = furrr::future_map2(listArg,
                                           SigArg,
                                           runStrategy,
                                           base5,
                                           'RSI'))
tablaRSI2 <- tablaRSI20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 95.797 sec elapsed

tbl01 <- tablaRSI2 

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)



# SAR ---------------------------------------------------------------------
# 'SARM', 'hlc',  'sig.SAR', list(), 

maxStep <- seq(0.05, 0.30, by = 0.01)
seqStep <- seq(0.004, 0.03, by = 0.002)

tablaSAR <- expand.grid(seqStep, maxStep) %>% 
  set_colnames(c('accelF','accelMax')) %>% 
  mutate(listArg = map2(accelF, accelMax, ~list(accel = c(.x, .y))))

tictoc::tic()
tablaSAR20 <- tablaSAR %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runIndicador,
                                          tblDatos=base5,
                                          indicName='SARM'))
tablaSAR2 <- tablaSAR20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 56.826 sec elapsed / (3dots) 118.913 sec elapsed

tbl01 <- tablaSAR2 

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(accel = c(0.014, 0.05)), NULL, base5, 'SARM') # 7%
plotTest(test0)

# maximo profit
test0 <- runIndicador(list(accel = c(0.004, 0.05)), NULL, base5, 'SARM') # 22.8%
plotTest(test0)

# SAR dots ----------------------------------------------------------------
maxStep <- seq(0.05, 0.30, by = 0.01)
seqStep <- seq(0.004, 0.03, by = 0.002)
seqDots <- 1:6

tablaSAR3 <- expand.grid(seqStep, maxStep, seqDots) %>%
  as_tibble() %>%
  set_colnames(c('accelF','accelMax', 'sdot')) %>%
  mutate(listArg = map2(accelF, accelMax, ~list(accel = c(.x, .y))),
         SigArg = map(sdot, ~list(dots = .x)) )

tictoc::tic()
tablaSAR30 <- tablaSAR3 %>%
  mutate(estadisticas = furrr::future_map2(listArg,
                                           SigArg,
                                           runIndicador,
                                           tblDatos=base5,
                                           indicName='SARM'))
tablaSAR33 <- tablaSAR30 %>%
  filter(map_dbl(estadisticas, nrow) > minTrans) %>%
  mutate(estadisticas = map(estadisticas, resumen)) %>%
  unnest(estadisticas)
tictoc::toc() # 9.6 minutos

tbl01 <- tablaSAR33

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>%
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>%
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# # En 30 min
# encontrado con paretos
test0 <- runIndicador(list(accel = c(0.01, 0.1)), list(dots = 6),
                      base5, 'SARM') # 7% / 8.4%(0.0risk)

# maximo profit
test0 <- runIndicador(list(accel = c(0.004, 0.05)), list(dots = 6),
                      base5, 'SARM') # 22.8%


# SAR dots SL -------------------------------------------------------------
risk <- seq(0, 0.10, 0.02)
maxStep <- seq(0.05, 0.30, by = 0.01)
seqStep <- seq(0.004, 0.03, by = 0.002)
seqDots <- 1:6

tablaSAR3 <- expand.grid(seqStep, maxStep, seqDots, risk) %>%
  as_tibble() %>%
  set_colnames(c('accelF','accelMax', 'sdot', 'risk')) %>% 
  nest(risk) %>% 
  mutate(listArg = map2(accelF, accelMax, ~list(accel = c(.x, .y))),
         SigArg = map(sdot, ~list(dots = .x)))

options(future.globals.maxSize= 2097152000)

tictoc::tic()
tablaSARuno <- tablaSAR3 %>% 
  mutate(estas = furrr::future_map2(listArg,
                                  SigArg,
                                  runSignal,
                                  tblDatos=base5,
                                  indicName='SARM'),
       supEst = furrr::future_map2(estas, data,
                                   ~.y %>% 
                                     mutate(estadisticas = map(risk, SLprofit, tblSig = .x)))
       ) %>% 
  select(-data, -estas) %>% 
  unnest(supEst)


tablaSARuno2 <- tablaSARuno %>%
  filter(map_dbl(estadisticas, nrow) > minTrans) %>%
  mutate(estadisticas = map(estadisticas, resumen)) %>%
  unnest(estadisticas)
tictoc::toc() # 21 minutos (13104/6risk)

tbl01 <- tablaSARuno2

tbl02 <- tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>%
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>%
  psel(low(time.final), # *low(time.final)
       top_level = 1)

View(tbl01)
View(tbl02)

# # En 30 min
# encontrado con paretos
test0 <- runSignal(list(accel = c(0.022, 0.05)), list(dots = 4),
                   base5, 'SARM') %>% 
  SLprofit(0.02)

plotTest(test0)

# maximo profit 40.49 %
test0 <- runSignal(list(accel = c(0.004, 0.05)), list(dots = 6),
                   base5, 'SARM') %>% 
  SLprofit(0.02)

plotTest(test0)



# SMI ---------------------------------------------------------------------
# 'SMIM', 'hlc',  'sig.SMI', list()
nSMI <- c(seq(2, 20, by = 3),13)
fast <- seq(2, 50, by = 4)
slow <- seq(4, 60, by = 4)
sing <- seq(6, 20, by = 3)

tablaSMI <- expand.grid(nSMI,
                        fast,
                         slow,
                         sing) %>%
  set_colnames(c('nS','fast','slow','sig')) %>% 
  as_tibble() %>% 
  # filter(slow >= (fast+4)) %>% 
  mutate(listArg = pmap(list(fast, slow, sig, nS), 
                        ~list(n = ..4, nFast = ..1, nSlow = ..2, nSig = ..3)))

tictoc::tic()
tablaSMI20 <-tablaSMI %>% 
  mutate(estadisticas = furrr::future_map(listArg,
                                          runIndicador,
                                          tblDatos=base5,
                                          indicName='SMIM'))
tablaSMI2 <- tablaSMI20 %>% 
  filter(map_dbl(estadisticas, nrow) > minTrans) %>% 
  mutate(estadisticas = map(estadisticas, resumen)) %>% 
  unnest(estadisticas) 
tictoc::toc() # 10 minutos(7,800)

tbl01 <- tablaSMI2 

tbl01 %>%
  filter(!is.na(profit)) %>%
  mutate(ta.trans = N.gan/(N.trans-N.gan),
         time.final = abs(as.numeric(time.mean - 10*30))) %>% 
  psel(high(ta.trans)*high(profit)*high(mes.mean)*
         low(meantrim.per)*low(mes.sd), # *low(time.final)
       top_level = 1) %>% 
  psel(low(time.final), # *low(time.final)
       top_level = 1)

# Best 30 min
test01 <- runIndicador(list(n = 8, nFast = 38, nSlow = 40, nSig = 18), 
                       NULL, base5, 'SMIM')

plotBackTest(test01, base5, sprintf("%02d", 42))


# final -------------------------------------------------------------------




