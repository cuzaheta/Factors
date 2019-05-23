# Script ---- 
# file: Reglas.R
# date: 22-01-2019
# input: Ninguno
# output: funciones
# description: Se crean las funciones para calcular indicadores y señales
#---

# reglas decisión ---------------------------------------------------------
sigThresh <- function(x, inf, sup){
  case_when(
    is.na(x) ~ NA_real_,
    x < inf ~ 1,
    x >= sup ~ -1,
    TRUE ~ 0
  )
}  


# combinar dos señales ----------------------------------------------------
combSig <- function(x, y){
  # Combina dos señales 
  # Retorna una señal discontinua
  case_when(
    is.na(x) | is.na(y) ~ NA_real_,
    x == 1 & y == 1 ~ 1,
    x == -1 ~ -1,
    TRUE ~ 0
  )
}

mix.ordenConfirm <- function(sig1, sig2){
  # Combina dos señales para confirmar una entrada
  # El indicador que abre la puerta para generar una compara es sig1
  # Y el que confirma la transacción es sig2
  # Por ejemplo: sig1 <- twoSMA, sig2 <- ADX
  # Retorna una señal continua
  
  # Siempre esta en -1
  sigmix <- rep(-1,length(sig1))
  
  puerta1 <- F
  entro <- F
  
  # Quitar los NA's que estan en las dos señales
  indices <- seq_along(sig1)
  indicesNA <- map2_lgl(sig1, sig2, anyNA)
  real_indices <- indices[!indicesNA]
  
  for (j in real_indices[2:length(real_indices)] ) {
    if(!entro & sig1[j] == 1 & sig1[j-1] == -1){
      puerta1 <- T
    }else if(puerta1 & sig1[j] == -1 & sig1[j-1] == 1){
      puerta1 <- F
    }else if(puerta1 & sig2[j] == 1 & sig2[j-1] == -1){
      puerta1 <- F
      entro <- T
      sigmix[j] <- 1
    }else if(entro & sig1[j] == 1){
      sigmix[j] <- 1
    }else if(entro & sig1[j] != 1){
      entro <- F
    }
  }
  
  return(sigmix)
}













# seleccionar datos -------------------------------------------------------
# OHLC
hlc <- . %>% select(high, low, close)
hl <- . %>% select(high, low)
cl <- . %>% select(close)
x1 <- . %>% select(x1)
x12 <- . %>% select(x1,x2)







# Nuevos indicadores ------------------------------------------------------
# Nombre de funciones sin numeros

SARM <- function(HLC, accel = c(0.02, 0.2)){
  # SAR Modificada
  SAR(hl(HLC), accel) %>% 
    as.tibble() %>% 
    bind_cols(cl(HLC))
}

SMIM <- function(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9){
  # SMI Modificada
  as.matrix(HLC) %>% 
    SMI(n, nFast, nSlow, nSig)
}

SMAtwo <- function(price, nfast = 15, nslow = 80){
  # Estrategía dos medias
  price <- pull(price)
  tibble(difMA = SMA(price, nfast) - SMA(price, nslow))
}








# Funciones señales -------------------------------------------------------
sig.BBands <- function(X, inf = 0, sup = 1){
  # Salida de BBands con columnas("dn","mavg","up","pctB")
  sigThresh(X[,'pctB'], inf, sup)
}
sig.SMAtwo <- function(X){
  # Salida de SMAtwo con columnas('difMA')
  -sigThresh(X[,'difMA'], 0, 0)
}
sig.SAR <- function(X){
  # Salida de SARM con columnas("sar","close")
  ifelse(X[,'sar'] > X[,'close'], -1, 1) %>% 
    as.vector()
}
sig.SAR3 <- function(X, dots = 3){
  # Salida de SARM con columnas("sar","close")
  indi <- ifelse(X[,'sar'] > X[,'close'], -1, 1)
  tsibble::slide_chr(indi, ~ifelse(sum(.x)==dots,1,-1) ,
                     .size = dots, .fill = -1) %>%
    as.integer()
}
sig.ADX <- function(X){ # Nombre directional movement indicator sig.DMI 
  # Salida de ADX con columnas("DIp","DIn","DX","ADX")
  ADXh <- X[,'DIp'] - X[,'DIn'] 
  sigThresh(ADXh, 0, 0)
}
sig.ADXL <- function(X, limit = 25){
  # Salida de ADX con columnas("DIp","DIn","DX","ADX")
  ifelse(X[,"ADX"] > limit, 1, -1)
}
# sig.SMADX <- function(X,Y, limit = 30){
#   # x: Salida de ADX con columnas("DIp","DIn","DX","ADX")
#   # y: Salida de SMAtwo con columnas('difMA')
#   sig.sma <- -sigThresh(X[,'difMA'], 0, 0)
#   sig.adx <- ifelse(Y[,"ADX"] > limit, 1, -1)
#   case_when(
#     is.na(sig.sma) | is.na(sig.adx) ~ NA_real_,
#     sig.sma == 1 & sig.adx == 1 ~ 1,
#     TRUE ~ -1
#   )
# }
sig.MACD <- function(X){
  # Salida de MACD con columnas("macd","signal")
  ifelse(X[,'macd'] >= X[,'signal'], 1, -1)
}
sig.SMI <- function(X){
  # Salida de SMI con columnas("SMI","signal")
  hSMI <- X[, "SMI"] - X[, "signal"]
  -sigThresh(hSMI,0,0)
  #ifelse(hSMI < 0, -1, ifelse(hSMI > 0, 1, 0))
}
sig.pred1 <- function(X){
  -sigThresh(X[,'x1'],0,0)
}
sig.pred12 <- function(X){
  s1 <- -sigThresh(X[,'x1'],0,0)
  s2 <- -sigThresh(X[,'x2'],0,0)
  ifelse((s1 == 1)&(s2==1),1,-1)
}

