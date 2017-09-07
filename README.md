# ProyectoFinMasterDataScientist


Proyecto de fin de Master: Predicción y representación interactiva de series temporales.

El propósito de este proyecto es aplicar varios modelos predictivos a un conjunto de series temporales (dividos en conjuntos de test y train) y encontrar el modelo óptimo que minimiza el error en cada caso. 
Además se quiere representar todo con gráficos interactivos y, posteriormente en otro documento anexo, representar mediante una aplicación interactiva con la herramienta Shiny las predicciones del 2017 y 2018.

Los datos han sido proporcionados por el Ministerio de Fomento. Son datos del tráfico en 28 autopistas de peaje del Estado Español. 

Lo que intentaremos predecir en los modelos sera el IMD mensual de cada autopista. El IMD es una media de vehículos mensuales, tanto pesados como ligeros, promediados por los días que tiene cada mes. 

Los 4 modelos aplicados han sido ETS y ARIMA con y sin aplicar log en base 10 a los datos. 

Para más detalle ver Memoria_final.Rmd  en carpeta res e implmentación en Shiny en carpeta shiny.

