# ProyectoFinMasterDataScientist


Proyecto de fin de Master: Predicci�n y representaci�n interactiva de series temporales.

El prop�sito de este proyecto es aplicar varios modelos predictivos a un conjunto de series temporales (dividos en conjuntos de test y train) y encontrar el modelo �ptimo que minimiza el error en cada caso. 
Adem�s se quiere representar todo con gr�ficos interactivos y, posteriormente en otro documento anexo, representar mediante una aplicaci�n interactiva con la herramienta Shiny las predicciones del 2017 y 2018.

Los datos han sido proporcionados por el Ministerio de Fomento. Son datos del tr�fico en 28 autopistas de peaje del Estado Espa�ol. 

Lo que intentaremos predecir en los modelos sera el IMD mensual de cada autopista. El IMD es una media de veh�culos mensuales, tanto pesados como ligeros, promediados por los d�as que tiene cada mes. 

Los 4 modelos aplicados han sido ETS y ARIMA con y sin aplicar log en base 10 a los datos. 

Para m�s detalle ver Memoria_final.Rmd  en carpeta res e implmentaci�n en Shiny en carpeta shiny.

