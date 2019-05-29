# Script.info ---- 
# file: Optimindicator.R
# date: 17-04-2019
# input: Datos de las monedas
# output: Estrategia con optimización de parametros
# description: La idea de este script es la busqueda de diferentes estrategias.
#   en su mayor medida combinación de indicadores basicos y formas de obtener
#   ordenes.
#---
rm(list = ls()) # ls(all.names = TRUE)
# options(future.globals.maxSize= 2097152000)

# packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(TTR)
# library(rPref)
library(lubridate)
library(tsibble)
library(patchwork) # combine separate ggplots into the same graphic, by `+`

# path --------------------------------------------------------------------
inPath <- file.path('input')
outPath <- file.path('output')
srcPath <- file.path('src')
inFunct <- file.path(srcPath,'Funciones')
inStrat <- file.path(outPath, 'Estrategias')

# functions ---------------------------------------------------------------
c('Reglas.R','FunEstrategia.R', 'Ordenes.R', 'GraphUtilities.R') %>% 
  file.path(inFunct, .) %>% 
  map(source, encoding = 'UTF8') %>% 
  invisible()
# Funciones para sacar resumen estrategia
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

# specs -------------------------------------------------------------------

tbl.specs <- tribble(
  ~nameInd, ~f, ~dat.f, ~sig.f, ~sig.parms,
  'adx', 'ADX', 'hlc', 'sig.ADXL', list(),
  'smaTwo','SMAtwo', 'cl', 'sig.SMAtwo', list()
) %>% 
  mutate(parms = map(f, ~list()))


# variables ---------------------------------------------------------------
fee <- 0.002
vInver <- 100
# profit <- 10
# minTrans <- 100

# lectura datos -----------------------------------------------------------

timeData <- c("4H", "2H", "30M", "15M")

datosAll <- tibble(timeData) %>% 
  mutate(datos = map(timeData, ~list()))

for (i in 1:nrow(datosAll)) {
  entra <- paste0(inPath, "/dataPoloniexSel", 
                  datosAll$timeData[[i]], "NTop.RData")
  load(entra) # dataSel
  
  cat('Inicia temporalidad ', datosAll$timeData[[i]], '\n')
  
  # ponerlo en otro script ---------
  tbl.datos <- tibble(name=names(dataSel) %>% 
                        str_replace(paste0('T',datosAll$timeData[[i]],
                                           '\\|'), ''), 
                      data=dataSel) %>% 
    mutate(data = map(data, select, -period, -pair),
           data = map(data, ~as_tibble(.x) %>% 
                        mutate(date = lubridate::ymd_hms(date))))
  # ponerlo en otro script ---------
  
  datosAll$datos[[i]] <- tbl.datos
}

datosAll %>% 
  unnest()

# base --------------------------------------------------------------------
base5 <- datosAll$datos[[3]]$data[[5]] %>% 
  mutate(ma = EMA(close, 3),
         maRet = ROC(ma))

base5.4 <- datosAll$datos[[2]]$data[[5]]

# Comparando temporalidades para ver si coincide el close
base5 %>% # ----------------------  NO COINCIDE !!!!!!!!!!!!!!!!!!!!!!!
  filter(date >= dmy('01-02-2018'), 
         date <= dmy('02-02-2018')) %>% 
  ggplot(aes(date, close)) +
  geom_line() +
  geom_line(aes(date, close, colour = 'red'), 
            data = base5.4 %>%
              filter(date >= dmy('01-02-2018'), 
                     date <= dmy('02-02-2018')))

# twoSMA-ADX --------------------------------------------------------------

# Con 30min parametros 25:80

twoSmaSig <- indSeg(list(nfast = 25, nslow = 80), NULL,
                    datos = base5, indName = 'smaTwo')
adxSig <- indSeg(list(n=16), list(limit= 30),
                 datos = base5, indName = 'adx')

sigmix <- mix.ordenConfirm(twoSmaSig, adxSig)

# Con ordenes normales
orden <- Ordenes(sigmix)

trans <- fun.tran(orden, base5)

summary(trans$retorno)

trans %>% 
  ggplot(aes(retorno)) +
  geom_histogram()

trans %>% 
  resumen() %>% 
  t()

trans
trans %>% 
  plotTest()


# Con SL movil
ordenSLM <- OrdenesSLMovil(sigmix, base5, 0.028)

transSL <- fun.tran(ordenSLM, base5)

summary(transSL$retorno)

transSL %>% 
  ggplot(aes(retorno)) +
  geom_histogram()

transSL %>% 
  resumen() %>% 
  t()

transSL
transSL %>% 
  plotTest()

df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)

p1 <- ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

p2 <- ggplot(df) +
  geom_point(aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3)



# lobstr::ast(1/3*5 + 2 * 3)

# plots -------------------------------------------------------------------

tbl2 <- base5 %>%
  mutate(
    meanFast = SMA(close, 25),
    meanSlow = SMA(close, 82),
    meanFree = EMA(close, 5),
    adx = ADX(hlc(.), 16)[,'ADX']
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

# not rut -----------------------------------------------------------------
# # # Tabla signos SMAtwo
# fast <- seq(2, 70, by = 2)
# slow <- seq(50, 250, by = 4)
# 
# tablaSMA <- expand.grid(fast,
#                         slow) %>%
#   set_colnames(c('fast','slow')) %>% 
#   as_tibble() %>% 
#   filter(slow >= (fast+4)) %>% 
#   mutate(listArg = pmap(list(fast, slow), 
#                         ~list(nfast = ..1, nslow = ..2))) %>% 
#   mutate(smaSg =  furrr::future_map(listArg, indSeg,
#                                     datos = base5,
#                                     indName = 'smaTwo'))
# 
# # # Tabla signos ADX
# nADX  <- seq(2,100,3)
# limADX <- c(seq(5,40,5),seq(7,37,5))
# 
# 
# tablaADX <- expand.grid(nADX,
#                         limADX) %>%
#   as_tibble() %>% 
#   set_colnames(c('n','lim')) %>% 
#   mutate(listArg = map(n, ~list(n = .x)),
#          SigArg = map(lim, ~list(limit = .x))) %>% 
#   mutate(adxSg = furrr::future_map2(listArg,
#                                     SigArg,
#                                     indSeg,
#                                     base5,
#                                     'adx'))
# 
# 
# # # Estrategia twoSMA - ADX
# 
# tabla.SMADX <- expand.grid(fast,
#                            slow,
#                            nADX,
#                            limADX) %>%
#   set_colnames(c('fast','slow','n','lim')) %>% 
#   as_tibble() %>% 
#   left_join(tablaSMA %>% 
#               select(fast, slow, smaSg)) %>% 
#   left_join(tablaADX %>% 
#               select(n, lim, adxSg))