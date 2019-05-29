# Script.info ---- 
# file: GraphUtilities.R
# date: 28-05-2019
# description: Funciones para crear diferentes graficas de trading
#              utilizando evaluación tidy. Se recomienda el uso del 
#              paquete(patchwork) para combinar ggplot gráficas en
#              una sola grafica, con el operador `/` para ponerlos
#              en una columna.
# gráficas: 
#   - plot_3means: Gráfica tres medias moviles
#   - plot_oscilador: Gráfica un oscilador con lineas horizontales.
#   - plot_bandas: Gráfica bandas y una linea media.
#---


plot_3means <- function(datos, mSlow, mFast, mFree, semanas){
  # Gráfica el close con tres medias moviles de distinto color
  # Argumentos: 
  #   datos: tabla de datos
  #   mSlow: Nombre en la datos de media movil lenta, es pintada en rojo.
  #   mFast: Nombre en la datos de media movil rapida, es pintada en azul.
  #   mFree: Nombre en la datos de media movil libre, es pintada en verde.
  #   semanas: vector con los numeros de semana a graficar.
  
  mSlow <- enexpr(mSlow)
  mFast <- enexpr(mFast)
  mFree <- enexpr(mFree)
  
  datos %>%
    filter(format(date, "%V") %in%  sprintf("%02d", semanas)) %>%
    ggplot() +
    geom_line(aes(date, close)) +
    geom_line(aes(date, !!mSlow), colour = 'red') +
    geom_line(aes(date, !!mFast), colour = 'blue') +
    geom_line(aes(date, !!mFree), colour = 'darkgreen')
}

plot_oscilador <- function(datos, oscilador, semanas, hlineas = NA_real_){
  # Gráfica un indicador oscilador, por ejemplo: RSI. Y si se desea con lineas
  # horizontales punteadas.
  # Argumentos: 
  #   datos: tabla de datos.
  #   oscilador: Nombre del oscilador en los datos.
  #   semanas: vector con los numeros de semana a graficar.
  #   hlineas: vector con el valor de las lineas horizontales. Dependiendo
  #            de cuantos elementos tenga este
  
  oscilador <- enexpr(oscilador)
  
  datos %>%
    filter(format(date, "%V") %in% sprintf("%02d", semanas) ) %>%
    ggplot()  +
    geom_line(aes(date, !!oscilador)) +
    geom_hline(yintercept = hlineas, linetype="dashed", na.rm = T)
}

plot_bandas <- function(datos, lower, middle, upper, semanas){
  # Gráfica el close con bandas y un una linea media.
  # Argumentos: 
  #   datos: tabla de datos.
  #   lower: Nombre de la banda inferior en los datos.
  #   middle: Nombre de la linea media en los datos. Ej. SMA
  #   upper: Nombre de la banda superior en los datos.
  #   semanas: vector con los numeros de semana a graficar.
  
  lower <- enexpr(lower)
  middle <- enexpr(middle)
  upper <- enexpr(upper)
  
  datos %>% 
    filter(format(date, "%V") %in% sprintf("%02d", semanas)) %>%
    ggplot() +
    geom_ribbon( aes(x = date, ymin = !!lower, ymax = !!upper), 
                 colour='#a1cece',fill="#f3f9f9", alpha=1) +
    geom_line(aes(date, close)) +
    geom_line(aes(date, !!middle), colour = '#ceb1b1') +
    theme_light()
}
