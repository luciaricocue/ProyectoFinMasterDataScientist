
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
source("../src/funciones.R")

###CCÓDIGO QUE SACA LA TABLA CON LOS ERRORES MAE/RMSE RESULTANTES DE APLICAR LOS 4 MODELOS PREDICTIVOS (ETS y ARIMA con y sin log) A CADA AUTOPISTA CON UN INTERVALO DE PREDICCIÓN DE 2 AÑOS
nombres_autopistas <- fread("../dat/nombres_autopistas.txt", header = F)
colnames(nombres_autopistas) <- c("codigo_nombre_autopista")
comparando_TODOS <- as.data.frame(t(rep(NA, 6)))
colnames(comparando_TODOS) <- c("RMSE", "MAE", "modelo", "RMSE_porc", "MAE_porc", "nombre")

autopista_TODAS <- lee_TODAS_autopista()
nombres_autopistas <- fread("../dat/nombres_autopistas.txt", header = F)
nombres_autopistas <- as.data.frame(nombres_autopistas)
colnames(nombres_autopistas) <- c("codigo_nombre_autopista")

for(i in nombres_autopistas$codigo_nombre_autopista){
  
  # nombre_elegido <- "06020101_montmelo_la_junquera"
  # nombre_elegido <-"06020102_barcelona_tarragona_A_7"
  # nombre_elegido <- "06020103_montmelo_papiol_A_7"
  # nombre_elegido <- "06020201_zaragoza_mediterraneo_A_2"
  
  # i <- nombre_elegido
  
  autopista <- autopista_TODAS %>% filter(nombre == i)
  autopista <- autopista[, 1:2]
  
  year_inicio <- year(autopista[1, 1])
  mes_inicio  <- month(autopista[1, 1])
  year_fin <- year(autopista[length(autopista$Mes_numeric), 1])
  mes_fin <- month(autopista[length(autopista$Mes_numeric), 1])
  
  ts_mensual <- ts(autopista$IMD_total, start=c(year_inicio, mes_inicio), end=c(year_fin, mes_fin), frequency=12)
  # xts_mensual <- as_xts(autopista, date_col = Mes_numeric)
  
  
  years_a_predecir <- 2
  inicio_train <- year_inicio
  inicio_test <- year_fin - years_a_predecir
  fin_train <- inicio_test - 1
  
  fin_test <- year_fin - 1
  
  train <- window(ts_mensual, start=c(inicio_train, 01), end=c(fin_train, 12))
  test <- window(ts_mensual, start=c(inicio_test, 01), end=c(fin_test, 12))
  
  
  fit_ets <- ets(train)
  
  prediccion_ets <- forecast(fit_ets, h = years_a_predecir*12)
  
  
  fit_arima <- auto.arima(train)
  
  
  prediccion_arima <- forecast(fit_arima, h = years_a_predecir*12)
  
  
  ##Voy a probar a hacer lo mismo pero aplicando primero log:
  
  
  log_ts_mensual <- round(log10(ts_mensual), 2)
  
  train_log <- window(log_ts_mensual, start=c(inicio_train, 01), end=c(fin_train, 12))
  test_log <- window(log_ts_mensual, start=c(inicio_test, 01), end=c(fin_test, 12))
  
  
  fit_ets_log <- ets(train_log)
  prediccion_ets_log <- forecast(fit_ets_log, h = 12*years_a_predecir)
  
  
  
  fit_arima_log <- auto.arima(train_log)
  prediccion_arima_log <- forecast(fit_arima_log, h = 12*years_a_predecir)
  
  ruta<- paste("../dat/",i,".csv", sep="")
  autopista_cmd <- fread(ruta, header = F)
  datos_anuales <- lee_autopista_anual(autopista_cmd)
  
  a <- c(datos_anuales[datos_anuales$Year == inicio_test,1],
         datos_anuales[datos_anuales$Year == fin_test,1])
  IME_prom_years <- mean(a)
  
  
  compara_predicciones_modelos <- construye_tabla_comparativa(prediccion_ets, prediccion_arima, prediccion_ets_log, prediccion_arima_log, IME_prom_years)
  #pasamos los datos de test para calcular el error porcentual (dividiendo error MAE o RMSE entre el promedio de los datos de test)
  
  compara_predicciones_modelos$nombre <- i
  
  comparando_TODOS <- rbind(comparando_TODOS, compara_predicciones_modelos)
  
}
comparando_TODOS <- comparando_TODOS[complete.cases(comparando_TODOS), ]
write.csv(comparando_TODOS, "../dat/comparando_TODOS.txt")



