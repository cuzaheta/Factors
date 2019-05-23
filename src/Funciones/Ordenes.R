# Script ---- 
# file: Ordenes.R
# date: 09-04-2019
# input: Ninguno
# output: funciones
# description: Se crean las funciones para calcular ordenes apartir de señales
#---


# Funciones de ordenes ----------------------------------------------------
# # -- Función con señales discontinuas
vencomDis <- function(signal){
  long <- length(signal)
  orden   <- rep(9, long)
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( signal[c(ii-1,ii)] ) ){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 0 & signal[ii-1] == 1){
        orden[ii] <- 1
        ab <- 1
      }
    }else if(ab == 1){ # Para vender
      if(signal[ii] == 0 & signal[ii-1] == -1){
        orden[ii] <- 0
        ab  <- 0
      }
    }
  }
  return(orden)
}

# # -- Función con señales continuas
vencomCon <- function(signal){
  long <- length(signal)
  orden   <- rep(9, long)
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( signal[c(ii-1,ii)] ) ){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 1 & signal[ii-1] == -1){
        orden[ii] <- 1
        ab <- 1
      }
    }else if(ab == 1){ # Para vender
      if(signal[ii] == -1 & signal[ii-1] == 1){
        orden[ii] <- 0
        ab  <- 0
      }
    }
  }
  return(orden)
}

# # -- Corre ordenes 
Ordenes <- function(signal){
  isCont <- sum(!is.na(unique(signal))) == 2
  
  if(!isCont){# Discontinuas
    infOrdenI <- vencomDis(signal)
  }else{# Continuas
    infOrdenI <- vencomCon(signal)
  }
  return(infOrdenI)
}

# Funciones de ordenes - stopLoss y takeProfit ----------------------------
# # -- Función con señales discontinuas
vencomDisSLTP <- function(signal, datos, porLoss, porProf, fee){
  price <- datos$close
  
  long    <- length(signal)
  orden   <- rep(9, long)
  tpOrden <- rep("NN", long)
  
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( signal[c(ii-1,ii)] ) ){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 0 & signal[ii-1] == 1){
        orden[ii] <- 1
        valBuy <- price[ii]
        valSellTP <- valBuy * (1 + porProf + fee)
        valSellSL <- valBuy * (1 - porLoss)
        ab <- 1
        ab <- 1
      }
    }else if(ab == 1){ # Para vender
      condInd <- signal[ii] == 0 & signal[ii-1] == -1
      condSL <- price[ii] <= valSellSL
      condTP <- price[ii] >= valSellTP
      if(condInd | condSL){
        orden[ii] <- 0
        if(condInd == TRUE) tpOrden[ii] <- "IN"          
        if(condSL  == TRUE) tpOrden[ii] <- "SL"
        if(condTP  == TRUE) tpOrden[ii] <- "TP"
        ab <- 0
      }
    }
  }
  return(tibble(orden = orden, tpOrden = tpOrden))
}

# # -- Función con señales continuas
vencomConSLTP <- function(signal, datos, porLoss, porProf, fee){
  price <- datos$close
  
  long    <- length(signal)
  orden   <- rep(9, long)
  tpOrden <- rep("NN", long)
  
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( signal[c(ii-1,ii)])){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 1 & signal[ii-1] == -1){
        orden[ii] <- 1
        ab <- 1
        valBuy <- price[ii]
        valSellTP <- valBuy * (1 + porProf + fee)
        valSellSL <- valBuy * (1 - porLoss)
      }
    }else if(ab == 1){ # Para vender
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
  return(tibble(orden = orden, tpOrden = tpOrden))
}

# # -- Corre ordenes - stop loss and take profit
OrdenesSLTP <- function(signal, datos, vloss, vfee = fee, vprofit = profit){
  isCont <- sum(!is.na(unique(signal))) == 2
  if(is.na(vloss)) vloss <- 0.999
  
  if(!isCont){# Discontinuas
    infOrdenISLTP <- vencomDisSLTP(signal, datos, vloss, vprofit, vfee)
  }else{# Continuas
    infOrdenISLTP <- vencomConSLTP(signal, datos, vloss, vprofit, vfee)
  }
  return(infOrdenISLTP)
}

# Funciones de ordenes - movil stop loss ----------------------------------

# Cambiar tblPrice por la tabla de los datos de la moneda, dentro seleccionar precio
# Al igual que con las otras funciones de ordenes, sin paso intermedio seleccionando
# las variables que necesita.

# # -- Función con señales discontinuas
vencomDisSLM <- function(signal, datos, porLoss){
  price <- datos$close
  ma <- datos$ma
  maRet <- datos$maRet
  
  long <- length(signal)
  orden   <- rep(9, long)
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( c(signal[c(ii-1,ii)], maRet[ii]) ) ){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 0 & signal[ii-1] == 1){
        orden[ii] <- 1
        valSL <- ma[ii] * (1 - porLoss) # SL inicial
        ab <- 1
      }
    }else if(ab == 1){ # Para vender
      condInd <- signal[ii] == 0 & signal[ii-1] == -1
      condSL <- price[ii] <= valSL
      if(condInd | condSL){
        orden[ii] <- 0
        ab  <- 0
      }else if(maRet[ii] > 0){ # Si no sale de la transacción, cambia el SL
        newSL <- ma[ii] * (1 - porLoss) # nuevo SL
        if(newSL > valSL) valSL <- newSL
      }
    }
  }
  return(orden)
}

# # -- Función con señales continuas
vencomConSLM <- function(signal, datos, porLoss){
  price <- datos$close
  ma <- datos$ma
  maRet <- datos$maRet
  
  long <- length(signal)
  orden   <- rep(9, long)
  
  ab <- 0
  
  for(ii in 2:long){
    # verifica que si hay NA y se salta al siguiente tiempo
    if( anyNA( c(signal[c(ii-1,ii)], maRet[ii]) ) ){ next()
    }else if(ab == 0){ # Para comprar  
      if(signal[ii] == 1 & signal[ii-1] == -1){
        orden[ii] <- 1
        valSL <- ma[ii] * (1 - porLoss) # SL inicial
        ab <- 1
      }
    }else if(ab == 1){ # Para vender
      condInd <- signal[ii] == -1 & signal[ii-1] ==  1
      condSL <- price[ii] <= valSL
      if(condInd | condSL){
        orden[ii] <- 0
        ab  <- 0
      }
      else if(maRet[ii] > 0){ # Si no sale de la transacción, cambia el SL
        newSL <- ma[ii] * (1 - porLoss) # nuevo SL
        if(newSL > valSL) valSL <- newSL
      }
    }
  }
  return(orden)
}

# # -- Corre ordenes - movil stop loss
OrdenesSLMovil <- function(signal, datos, vloss){
  isCont <- sum(!is.na(unique(signal))) == 2
  if(is.na(vloss)) vloss <- 0.999
  
  if(!isCont){# Discontinuas
    infOrdenISL <- vencomDisSLM(signal, datos, vloss)
  }else{# Continuas
    # cat('Es continua \n')
    infOrdenISL <- vencomConSLM(signal, datos, vloss)
  }
  return(infOrdenISL)
}

