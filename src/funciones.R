#Para leer los archivos, previamente guardados en csv:
# setwd("C:/Users/Luc�a/Desktop/ProyectoFinMasterDataScientist")


suppressMessages({
  library(data.table)
  library(reshape)
  library(zoo)
  library(lubridate)
  library(readr) 
  library(stats)
  library(xts)
  library(tidyquant)
  library(forecast)
})


# Vamos a hacer lo mismo a todos los ficheros, pues tienen la misma estructura (excepto el de los datos globales).
#A todos los ficheros de autopista les har�  lo mismo:
lee_autopista <- function(autopista){

  #borro columnas sin informaci�n:
  autopista$V7 <- autopista$V8 <- NULL
  #borro filas al principio y final de la 1 a 6 y de la 362 a 368. Adem�s la 34 que separa datos anuales de mensuales:
  autopista <- autopista[!c(1:6, 34, 362:368) , ]
  #separo la primera columna en Mes/A�o y relleno las de a�o incompleto
  autopista <- as.data.frame(autopista)
  
  autopista <-  transform(autopista, V1 = colsplit(V1, split = " ", names = c('Year', 'Mes')))
  #renombro columnas
  autopista$Year <- autopista$V1$Year
  autopista$Mes <- autopista$V1$Mes
  autopista$V1 <- NULL
  colnames(autopista) <- c("Longitud", "IMD_total", "variacion_IMD_tot.porcentaje", "IMD_pesados", "variacion_IMD_pes.porcentaje", "Year", "Mes")
  #voy a poner los tipos de las columnas ok:
  #tipo num�rico
  autopista$Longitud <- as.numeric(gsub(",", "\\.", autopista$Longitud))
  
  autopista$IMD_total <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_total)))
  
  autopista$variacion_IMD_tot.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_tot.porcentaje))
  
  autopista$IMD_pesados <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_pesados)))
  
  autopista$variacion_IMD_pes.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_pes.porcentaje))
  #tipo character y relleno huecos:
  autopista$Year <- gsub('[a-z]', NA, autopista$Year)
  autopista <- autopista %>% mutate(Year = na.locf(Year, na.rm = FALSE,fromLast=FALSE))
  
  autopista$Mes <- gsub('[0-9]', NA, autopista$Mes)
  
  
  
  autopista_ANUAL <- autopista[1:27, 1:6]
  autopista_MENSUAL <- autopista[28:354, ]
  
  # Porque los 0s los pilla como NAs
  autopista_ANUAL <- autopista_ANUAL %>%
    mutate(variacion_IMD_pes.porcentaje = ifelse(is.na(variacion_IMD_pes.porcentaje),
                                                 100*(IMD_pesados-lead(IMD_pesados))/IMD_pesados, variacion_IMD_pes.porcentaje))
  
  
  
  # S�lo deber�a de tener NAs en los datos de 1990 de variaciones, porque no tengo datos del 1989 con los que comparar.
  
  # Ahora vamos a empezar a tratarlos como series temporales:
  
  # autopista_ANUAL$Year <- years(autopista_ANUAL$Year)
  # autopista_MENSUAL$Year <- years(autopista_MENSUAL$Year)
  
  autopista_MENSUAL$yearmon <- paste(autopista_MENSUAL$Mes, " ", autopista_MENSUAL$Year)
  autopista_MENSUAL$yearmon <- as.yearmon(paste(autopista_MENSUAL$Mes, " ",
                                                autopista_MENSUAL$Year), "%b %Y")
  # autopista_MENSUAL$Mes <- years(autopista_MENSUAL$Mes)
  
  autopista_MENSUAL$Mes_numeric <- autopista_MENSUAL$Mes
  autopista_MENSUAL$Mes_numeric <- paste(tolower(autopista_MENSUAL$Mes_numeric),".")
  autopista_MENSUAL$Mes_numeric <- gsub('sep', "sept", autopista_MENSUAL$Mes_numeric)
  autopista_MENSUAL$Mes_numeric <- gsub(' ', "", autopista_MENSUAL$Mes_numeric)
  autopista_MENSUAL$Mes_numeric <- paste(autopista_MENSUAL$Mes_numeric," ", autopista_MENSUAL$Year)
  
  autopista_MENSUAL$Mes_numeric <- as.yearmon(parse_date(autopista_MENSUAL$Mes_numeric,
                                                     "%b %Y",locale=locale("es")))
  autopista_MENSUAL$yearmon <- NULL
  return(autopista_MENSUAL[, c(8, 2)])
  
}
# sevilla_cadiz <- fread("dat/06020203_sevilla_cadiz.csv")
# autopista <- sevilla_cadiz
# table_sevilla_cadiz <- lee_autopista(autopista)