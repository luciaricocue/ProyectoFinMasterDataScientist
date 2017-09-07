#'###FUNCIONES
#'
#' 
#Para leer los archivos, previamente guardados en csv:



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
  library(ggplot2)
  library(knitr)
  library(highcharter)
  # library(imputeTS)
})

#' ###FUNCION QUE LEE SOLO LOS DATOS DE 1 AUTOPISTA MENSUALES: 
# Vamos a hacer lo mismo a todos los ficheros, pues tienen la misma estructura (excepto el de los datos globales).
#A todos los ficheros de autopista les haré  lo mismo:
lee_autopista <- function(autopista, nombre){

  #borro columnas sin información:
  autopista$V7 <- autopista$V8 <- NULL
  #borro filas al principio y final de la 1 a 6 y de la 362 a 368. Además la 34 que separa datos anuales de mensuales:
  # autopista <- autopista[!c(1:6, 34, 362:368) , ]
  autopista <- autopista[complete.cases(autopista$V3),]
  #separo la primera columna en Mes/Año y relleno las de año incompleto
  autopista <- as.data.frame(autopista)
  
  autopista <-  transform(autopista, V1 = colsplit(V1, split = " ", names = c('Year', 'Mes')))
  #renombro columnas
  autopista$Year <- autopista$V1$Year
  autopista$Mes <- autopista$V1$Mes
  autopista$V1 <- NULL
  colnames(autopista) <- c("Longitud", "IMD_total", "variacion_IMD_tot.porcentaje", "IMD_pesados", "variacion_IMD_pes.porcentaje", "Year", "Mes")
  #voy a poner los tipos de las columnas ok:
  #tipo numérico
  autopista$Longitud <- as.numeric(gsub(",", "\\.", autopista$Longitud))
  
  # este es el que quita los 0s finales y falla en algunos valores, porque trata el . como ,decimal, por lo que los 0s finales no aportan información
  # autopista$IMD_total <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_total)))
  
  # lo cambio por esto:
  # autopista$IMD_total <- autopista$IMD_total*1000
  autopista$IMD_total[grep("\\.", as.character(autopista$IMD_total))] <- autopista$IMD_total[grep("\\.", as.character(autopista$IMD_total))]*1000
  
  autopista$variacion_IMD_tot.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_tot.porcentaje))
  
  autopista$IMD_pesados <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_pesados)))
  
  autopista$variacion_IMD_pes.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_pes.porcentaje))
  #tipo character y relleno huecos:
  autopista$Year <- gsub('[a-z]', NA, autopista$Year)
  autopista <- autopista %>% mutate(Year = na.locf(Year, na.rm = FALSE,fromLast=FALSE))
  
  autopista$Mes <- gsub('[0-9]', NA, autopista$Mes)
  
  
  
  # autopista_ANUAL <- autopista[1:27, 1:6]
  # autopista_MENSUAL <- autopista[28:354, ]
  autopista_MENSUAL <- autopista[complete.cases(autopista$Mes),]
  
  # Porque los 0s los pilla como NAs
  # autopista_ANUAL <- autopista_ANUAL %>%
  #   mutate(variacion_IMD_pes.porcentaje = ifelse(is.na(variacion_IMD_pes.porcentaje),
  #                                                100*(IMD_pesados-lead(IMD_pesados))/IMD_pesados, variacion_IMD_pes.porcentaje))
  # 
  
  
  # Sólo debería de tener NAs en los datos de 1990 de variaciones, porque no tengo datos del 1989 con los que comparar.
  
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
  autopista_MENSUAL <- autopista_MENSUAL %>% arrange(Mes_numeric)
  autopista_MENSUAL$nombre <- nombre
  # autopista_MENSUAL$Mes_numeric <- as.Date(autopista_MENSUAL$Mes_numeric)
  return(autopista_MENSUAL[, c(8, 2, 9)])
  
}

#' ###FUNCION QUE LEE LOS DATOS MENSUALES DE TODAS LAS AUTOPISTAS Y DEVUELVE UN DATA FRAME CON TODA LA INFORMACION: 
lee_TODAS_autopista <- function(){

  nombres_autopistas <- fread("../dat/nombres_autopistas.txt", header = F)
  autopista_TODAS <- data.frame(NA, NA, NA) #va a introducir una primera fila blanca
  colnames(autopista_TODAS) <- c("Mes_numeric", "IMD_total", "nombre")
  
  for(i in nombres_autopistas$V1){
    ruta <- paste("../dat/",i,".csv", sep="")
    autopista <- lee_autopista(fread(ruta, header = F), i)
    autopista_TODAS <- rbind(autopista_TODAS, autopista)
  }
  autopista_TODAS$Mes_numeric <- as.yearmon(autopista_TODAS$Mes_numeric)
  # autopista_TODAS$Mes_numeric <- as.Date(autopista_TODAS$Mes_numeric)
  # autopista_TODAS$Mes_numeric <- format(as.Date(autopista_TODAS$Mes_numeric, "%Y-%m-%d"), "%Y-%m")#character
  
  autopista_TODAS <- autopista_TODAS[complete.cases(autopista_TODAS$nombre),]
  
  return(autopista_TODAS)
  #unique(autopista_TODAS$nombre)comprobacion
  
}
  

#' ###FUNCION QUE LEE SOLO LOS DATOS DE 1 AUTOPISTA ANUALES: 
lee_autopista_anual <- function(autopista){
  
  #borro columnas sin información:
  autopista$V7 <- autopista$V8 <- NULL
  #borro filas al principio y final de la 1 a 6 y de la 362 a 368. Además la 34 que separa datos anuales de mensuales:
  # autopista <- autopista[!c(1:6, 34, 362:368) , ]
  autopista <- autopista[complete.cases(autopista$V3),]
  #separo la primera columna en Mes/Año y relleno las de año incompleto
  autopista <- as.data.frame(autopista)
  
  autopista <-  transform(autopista, V1 = colsplit(V1, split = " ", names = c('Year', 'Mes')))
  #renombro columnas
  autopista$Year <- autopista$V1$Year
  autopista$Mes <- autopista$V1$Mes
  autopista$V1 <- NULL
  colnames(autopista) <- c("Longitud", "IMD_total", "variacion_IMD_tot.porcentaje", "IMD_pesados", "variacion_IMD_pes.porcentaje", "Year", "Mes")
  #voy a poner los tipos de las columnas ok:
  #tipo numérico
  autopista$Longitud <- as.numeric(gsub(",", "\\.", autopista$Longitud))
  
  # este es el que quita los 0s finales y falla en algunos valores, porque trata el . como ,decimal, por lo que los 0s finales no aportan información
  # autopista$IMD_total <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_total)))
  
  # lo cambio por esto:
  # autopista$IMD_total <- autopista$IMD_total*1000
  autopista$IMD_total[grep("\\.", as.character(autopista$IMD_total))] <- autopista$IMD_total[grep("\\.", as.character(autopista$IMD_total))]*1000
  
  autopista$variacion_IMD_tot.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_tot.porcentaje))
  
  autopista$IMD_pesados <- as.numeric(gsub( "\\.", "",as.character(autopista$IMD_pesados)))
  
  autopista$variacion_IMD_pes.porcentaje <- as.numeric(gsub( ",", "\\.",
                                                             autopista$variacion_IMD_pes.porcentaje))
  #tipo character y relleno huecos:
  autopista$Year <- gsub('[a-z]', NA, autopista$Year)
  autopista <- autopista %>% mutate(Year = na.locf(Year, na.rm = FALSE,fromLast=FALSE))
  
  autopista$Mes <- gsub('[0-9]', NA, autopista$Mes)
  
  
  
  autopista_ANUAL <- autopista[!complete.cases(autopista$Mes), ]
  autopista_ANUAL$Year <- as.numeric(autopista_ANUAL$Year)
 
 
  return(autopista_ANUAL[, c(2,6)])
  
}


#' ###FUNCION QUE PREPARA LOS DATOS DE LAS PREDICCIONES DE LOS 4 MODELOS PARA PRESENTARLOS EN UNA TABLA COMPARATIVA: 
construye_tabla_comparativa <- function(prediccion_ets, prediccion_arima, prediccion_ets_log, prediccion_arima_log, IME_prom_years){
  # Pasamos a la funcion 
  # IME_prom_years
  # prediccion_ets, prediccion_arima, prediccion_ets_log, prediccion_arima_log
  
  #Necesitamos promediar los datos de test para calcular los errores porcentuales (IME_prom_years).
  

  
   
  a <- as.data.frame(accuracy(prediccion_ets, test))[2, 2:3]
  a$modelo <- "ETS"
  a <- a %>% mutate(RMSE_porc = RMSE*100 / IME_prom_years, MAE_porc = MAE*100 / IME_prom_years)
  
  b <- as.data.frame(accuracy(prediccion_arima, test))[2, 2:3]
  b$modelo <- "ARIMA"
  b <- b %>% mutate(RMSE_porc = RMSE*100 / IME_prom_years, MAE_porc = MAE*100 / IME_prom_years)
  
  
  
  # Deshago log y recalculo MAE y RMSE
  # 
  # Añadir valores reales
  
  # #prediccion con ETS si aplicar log
  # tabla_ets <- as.data.frame(prediccion_ets)
  # #prediccion con ARIMA si aplicar log
  # tabla_arima <- as.data.frame(prediccion_arima)
  #prediccion con ETS aplicando log
  tabla_ets_log <- as.data.frame(prediccion_ets_log)
  #prediccion con ARIMA aplicando log
  tabla_arima_log <- as.data.frame(prediccion_arima_log)
  
  
  tabla_ets_log <- tabla_ets_log %>% 
    mutate(pred_sin_log = 10^`Point Forecast`, test = as.vector(test))
  RMSE_sin_log1 <- tabla_ets_log %>% summarise(sqrt(mean((pred_sin_log-test)^2)))
  MAE_sin_log1 <- tabla_ets_log %>% summarise(mean(abs(pred_sin_log-test)))
  
  c <- as.data.frame(accuracy(prediccion_ets_log, test_log))[2, 2:3]
  c$modelo  <- "ETS_log"
  c_sin_log <- c
  c_sin_log[1, 1] <- RMSE_sin_log1
  c_sin_log[1, 2] <- MAE_sin_log1
  c_sin_log <- c_sin_log %>% mutate(RMSE_porc = RMSE*100 / IME_prom_years, MAE_porc = MAE*100 / IME_prom_years)
  
  
  # Hacemos lo mismo para 'tabla_arima_log'
  
  
  tabla_arima_log <- tabla_arima_log %>% 
    mutate(pred_sin_log = 10^`Point Forecast`, test = as.vector(test))
  RMSE_sin_log2 <- tabla_arima_log %>% summarise(sqrt(mean((pred_sin_log-test)^2)))
  MAE_sin_log2 <- tabla_arima_log %>% summarise(mean(abs(pred_sin_log-test)))
  
  d <- as.data.frame(accuracy(prediccion_arima_log, test_log))[2, 2:3]
  d$modelo <- "ARIMA_log"
  d_sin_log <- d
  d_sin_log[1, 1] <- RMSE_sin_log2
  d_sin_log[1, 2] <- MAE_sin_log2
  d_sin_log <- d_sin_log %>% mutate(RMSE_porc = RMSE*100 / IME_prom_years, MAE_porc = MAE*100 / IME_prom_years)
  

  compara_predicciones_modelos <- rbind(a, b, c_sin_log, d_sin_log)
  return(compara_predicciones_modelos)
}


