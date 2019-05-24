
# Datos

En este archivo describe de donde en la carpeta bot del drive se tomaron los datos a leer.

> Falta actualizar los datos, con el script 01DataPoloniex.R


    rm(list=ls(all=TRUE))
    
    library(PoloniexR) # R3.4.1
    library(tidyverse)
    library(lubridate)
    
    # Paths -------------------------------------------------------------------
    outPath <- file.path('Data') 
    
    
    # functions ---------------------------------------------------------------
    leeDat <- function(pairs ,periods){
      chart.data <- ReturnChartData(theObject = poloniex.public,
                                    pair      = pairs,
                                    from      = from,
                                    to        = to,
                                    period    = periods)
      
      chart.data01 <- chart.data %>%
        as.data.frame(.) %>%
        mutate(date   = row.names(.)) %>%
        arrange(date) %>% 
        as_tibble()
      
      return(chart.data01)
      
    }
    
    
    # Data --------------------------------------------------------------------
    poloniex.public <- PoloniexPublicAPI()
    Sys.setenv(tz="UTC")
    ticker.info <- ReturnTicker(poloniex.public)
    
    from <- today() %>% 
      update(year = year(.)-2, mday = 01) %>% 
      paste0(., ' 00:00:00 UTC') %>% 
      as.POSIXct()
    
    to <- today() %>% 
      paste0(., ' 00:00:00 UTC') %>% 
      as.POSIXct()
    
    pairs   <- row.names(ticker.info)
    
    from <- today() %>% 
      update(year = year(.)-0, month = month(.)-9, mday = 01) %>% 
      paste0(., ' 00:00:00 UTC') %>% 
      as.POSIXct()
    
    to <- today() %>% 
      update(mday = 01) %>% 
      paste0(., ' 00:00:00 UTC') %>%
      as.POSIXct(format = '%Y')
      
    # from <- as.POSIXct('2018-01-01 UTC')
    # to <- as.POSIXct('2019-01-31 UTC')
    # --------------------------------------------------------------------
    ################################################################################
    # # Descarga
    ################################################################################
    # periodsInt <- c("5M", "15M", "30M", "2H", "4H", "D")
    periodsInt <- c("15M", "30M", "2H", "4H", "D")
    
    datoList <- list_along(periodsInt)
    names(datoList) <- periodsInt
    
    for(period in periodsInt){
      tictoc::tic()
      cat("Descargando: periodo", period, as.character(now()), '\n')
      data <- tibble(name = pairs) %>% 
        filter(seq_along(name) %in% c(1)) %>% 
        mutate(data = map(name, leeDat, period))
      tictoc::toc()
      
      datoList[[period]] <- data
      
      # sale <- paste0(outPath, "dataPoloniex", period, ".RData")
      # save(data, file = sale)
    }





