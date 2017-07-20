
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
library(shiny)
library(datasets)
source("../src/funciones.R")

#leo datos con la funcion
autopistas_TODAS <- lee_TODAS_autopista()
#quito algunos outliers
autopistas_TODAS <- autopistas_TODAS %>% mutate(IMD_total = ifelse(IMD_total < 50, IMD_total*1000, IMD_total))
# autopistas_TODAS <- fread("datos_TODAS_autopistas.txt", header=T, select = c(2:4))
autopistas_TODAS <- as.data.frame(autopistas_TODAS)
autopistas_TODAS$nombre <- as.factor(autopistas_TODAS$nombre )
autopistas_TODAS$Mes_numeric <- as.Date(autopistas_TODAS$Mes_numeric )


ui <- navbarPage("VISUALIZACION INTERACTIVA DE SERIES TEMPORALES Y MODELOS DE LOS 4 ESTUDIADOS",
                 
                 tabPanel("Informacion previa",  
                          "Los datos han sido descargados en Julio del 2017, aunque para entrenar los modelos predictivos se han usado solo datos hasta el 2016 y se han predicho los datos de 2017 y 2018.",
                          br(),
                          br(),
                          "Lo que intentaremos predecir en los modelos sera el IMD mensual de los proximos periodos anuales de cada autopista. El IMD es una media de vehiculos mensuales, pesados + ligeros, promediados por los dias que tiene cada mes.",
                          br(),
                          br(),
                          "Los 4 modelos aplicados han sido ETS y ARIMA con y sin aplicar log en base 10 a los datos. Para mas detalle ver ",
                          a("memoria", href="https://github.com/luciaricocue/ProyectoFinMasterDataScientist/tree/master/res"),
                          br(),
                          br(),
                          "En un estudio previo se han obtenidos los errores de aplicar los 4 modelos a cada serie de cada autopista, y se ha escogido el modelo optimo en funcion del error minimo.",
                          br(),
                          br(),
                          "Se deja como posible mejora el estudiar otros modelos sobre las autopistas con mayores errores.",
                          br(),
                          br(),
                          "Para realizar el estudio nos hemos basado en el libro ", 
                          a("Forecasting: principles and practice", href="https://www.otexts.org/fpp/1)"),
                          "de Rob J Hyndman y George Athanasopoulos.",
                          br(),
                          br(),
                          "Los datos han sido proporcionados por el Ministerio de Fomento desde su", 
                          a("web", href="http://www.fomento.gob.es/BE/?nivel=2&orden=06000000)"),
                          ". Son datos del trafico en 28 autopistas de peaje del Estado.",
                          br(),
                          br(),
                          a("Puedes descargarte el codigo y la memoria de Github", href="https://github.com/luciaricocue/ProyectoFinMasterDataScientist"),
                          br(),
                          br(),
                          "Para cualquier duda puedes contactar conmigo:",
                          br(),
                          "Lucia Rico Cuellar: " ,
                          a("luciaricocue@gmail.com", href="luciaricocue@gmail.com")
                          ),
                 
                 tabPanel("Representacion st",

                            titlePanel("Representacion de los datos de todas las autopistas como series temporales"),

                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput('nombre_autopista', 'Nombre Autopista', 
                                            as.character(levels(autopistas_TODAS$nombre)), 
                                            selected="06020301_tarragona_valencia_A_7")

                              ),

                              mainPanel(
                                
                                highchartOutput("hcontainer",height = "500px")
                              )
                            )
                   ), 
                   tabPanel("Representacion ETS",
                            
                            titlePanel("Representacion de las predicciones del IMD de las autopistas con error minimo al aplicar ETS"),
                            

                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput('nombre_autopista2', 'Nombre Autopista', 
                                            as.character(levels(autopistas_TODAS$nombre)), 
                                            selected="06020301_tarragona_valencia_A_7")
   
                              ),
                              

                              mainPanel(
                                # plotOutput("plot2")
                                highchartOutput("hcontainer2",height = "500px")

                              )
                            )
                   ),
                 tabPanel("Representacion ARIMA",
                          
                          titlePanel("Representacion de las predicciones del IMD de las autopistas con error minimo al aplicar ARIMA"),

                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              selectInput('nombre_autopista3', 'Nombre Autopista', 
                                          as.character(levels(autopistas_TODAS$nombre)), 
                                          selected="06020301_tarragona_valencia_A_7")

                              
                            ),
                            

                            
                            mainPanel(
                              # plotOutput("plot3")
                              highchartOutput("hcontainer3",height = "500px")
                            )
                          )
                 ),
                 tabPanel("Representacion ETS con log",
                          
                          titlePanel("Representacion de las predicciones del IMD de las autopistas con error minimo al aplicar ETS con log"),
                          
                        
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              selectInput('nombre_autopista4', 'Nombre Autopista', 
                                          as.character(levels(autopistas_TODAS$nombre)), 
                                          selected="06020301_tarragona_valencia_A_7")

                            ),
                            

                            mainPanel(
                              # plotOutput("plot4")
                              highchartOutput("hcontainer4",height = "500px")
                            )
                          )
                 ),
                 tabPanel("Representacion ARIMA con log",
                          
                          titlePanel("Representacion de las predicciones del IMD de las autopistas con error minimo al aplicar ARIMA con log"),
                          

                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              selectInput('nombre_autopista5', 'Nombre Autopista', 
                                          as.character(levels(autopistas_TODAS$nombre)), 
                                          selected="06020301_tarragona_valencia_A_7")

                            ),
                            

                            mainPanel(
                              # plotOutput("plot5")
                              highchartOutput("hcontainer5",height = "500px")
                            )
                          )
                 )
                  
             
)

        


server <- function(input, output) {
 
#sobre el primer tabPanel no hay nada en esta funcion 
#tabPanel "Representacion st":
  formulaText <- reactive({
    input$nombre_autopista
  })
  

  output$caption <- renderText({
    formulaText()
  })
  
  selectedDataI <- reactive({
    if(is.null(input$nombre_autopista))return()
    
    autopistas_TODAS <- autopistas_TODAS %>% 
      filter(nombre == input$nombre_autopista) %>% 
      select(Mes_numeric, IMD_total) %>% arrange(Mes_numeric)
    
    start_year <- as.numeric(year(autopistas_TODAS$Mes_numeric[1]))
    end_year <- as.numeric(year(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    start_month <- as.numeric(month(autopistas_TODAS$Mes_numeric[1]))
    end_month <- as.numeric(month(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    
    autopista <- ts(autopistas_TODAS$IMD_total, start=c(start_year, start_month), end=c(end_year, end_month), frequency=12)
    
    return(autopista)
    
  })

  output$hcontainer <- renderHighchart({
  
    a <- highchart(type = "stock") %>%
      hc_title(text = "Time Serie") %>% 
      hc_subtitle(text = input$nombre_autopista) %>% 
      hc_add_series(selectedDataI(), name = "IMD") %>% 
      hc_rangeSelector(inputEnabled = TRUE) %>% 
      hc_scrollbar(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_gridlight()) 
    
    return(a)
    
  })

#tabPanel "Representacion ETS":  
  formulaText2 <- reactive({
    
    input$nombre_autopista2
  })
  

  output$caption <- renderText({
    
    formulaText2()
  })
  
  
  selectedDataI2 <- reactive({
    if(is.null(input$nombre_autopista2))return()
    
    autopistas_TODAS <- autopistas_TODAS %>% 
      filter(nombre == input$nombre_autopista2) %>% 
      select(Mes_numeric, IMD_total) %>% arrange(Mes_numeric)
    
    
    start_year2 <- as.numeric(year(autopistas_TODAS$Mes_numeric[1]))
    end_year2 <- as.numeric(year(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    start_month2 <- as.numeric(month(autopistas_TODAS$Mes_numeric[1]))
    end_month2 <- as.numeric(month(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    
    autopista <- ts(autopistas_TODAS$IMD_total, start=c(start_year2, start_month2), end=c(end_year2, end_month2), frequency=12)
    
    return(autopista)
  })
  
  
  output$hcontainer2 <- renderHighchart({

    prediccion_ets2 <- forecast(ets(selectedDataI2()), h = 2*12)
    b <- highchart(type = "stock") %>%
      hc_title(text = "Time Serie") %>%
      hc_subtitle(text = input$nombre_autopista2) %>%
      hc_add_series(selectedDataI2(), name = "IMD") %>%
      hc_add_series(prediccion_ets2, name = "IMD") %>%
      hc_rangeSelector(inputEnabled = TRUE) %>%
      hc_scrollbar(enabled = TRUE) %>%
      hc_add_theme(hc_theme_gridlight())
 
    return(b)
    
    
  })

#tabPanel "Representacion ARIMA":  
  formulaText3 <- reactive({
    
    input$nombre_autopist3
  })
  

  output$caption <- renderText({
    
    formulaText3()
  })
  
  
  selectedDataI3 <- reactive({
    
    if(is.null(input$nombre_autopista))return()
    
    autopistas_TODAS <- autopistas_TODAS %>% 
      filter(nombre == input$nombre_autopista3) %>% 
      select(Mes_numeric, IMD_total) %>% arrange(Mes_numeric)
    
    start_year3 <- as.numeric(year(autopistas_TODAS$Mes_numeric[1]))
    end_year3 <- as.numeric(year(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    start_month3 <- as.numeric(month(autopistas_TODAS$Mes_numeric[1]))
    end_month3 <- as.numeric(month(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    
    autopista <- ts(autopistas_TODAS$IMD_total, start=c(start_year3, start_month3), end=c(end_year3, end_month3), frequency=12)
    
    return(autopista)
  })

  output$hcontainer3 <- renderHighchart({
    
    # 
    # c <- highchart(type = "stock") %>%
    #   hc_title(text = "Time Serie") %>% 
    #   hc_subtitle(text = input$nombre_autopista3) %>% 
    #   hc_add_series(selectedDataI3(), name = "IMD") %>% 
    #   hc_rangeSelector(inputEnabled = TRUE) %>% 
    #   hc_scrollbar(enabled = TRUE) %>% 
    #   hc_add_theme(hc_theme_gridlight()) 
    
    prediccion_ets3 <- forecast(auto.arima(selectedDataI3()), h = 2*12)
    c <- highchart(type = "stock") %>%
      hc_title(text = "Time Serie") %>%
      hc_subtitle(text = input$nombre_autopista3) %>%
      hc_add_series(selectedDataI3(), name = "IMD") %>%
      hc_add_series(prediccion_ets3, name = "IMD") %>%
      hc_rangeSelector(inputEnabled = TRUE) %>%
      hc_scrollbar(enabled = TRUE) %>%
      hc_add_theme(hc_theme_gridlight())
    
    
    return(c)
  })
  

#tabPanel "Representacion ETS con log":  
  formulaText4 <- reactive({
    
    input$nombre_autopist4
  })
  

  output$caption <- renderText({
    
    formulaText4()
  })
  
  
  selectedDataI4 <- reactive({
    
    if(is.null(input$nombre_autopista))return()
    
    autopistas_TODAS <- autopistas_TODAS %>% 
      filter(nombre == input$nombre_autopista4) %>% 
      select(Mes_numeric, IMD_total) %>% arrange(Mes_numeric)
    
    start_year4 <- as.numeric(year(autopistas_TODAS$Mes_numeric[1]))
    end_year4 <- as.numeric(year(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    start_month4 <- as.numeric(month(autopistas_TODAS$Mes_numeric[1]))
    end_month4 <- as.numeric(month(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
  
    autopista <- ts(autopistas_TODAS$IMD_total, start=c(start_year4, start_month4), end=c(end_year4, end_month4), frequency=12)
    
    log_xts_mensual <- round(log10(autopista), 2)
    
    return(log_xts_mensual)
  })

  output$hcontainer4 <- renderHighchart({
    
    # d <- highchart(type = "stock") %>%
    #   hc_title(text = "Time Serie") %>% 
    #   hc_subtitle(text = input$nombre_autopista4) %>% 
    #   hc_add_series(selectedDataI4(), name = "IMD") %>% 
    #   hc_rangeSelector(inputEnabled = TRUE) %>% 
    #   hc_scrollbar(enabled = TRUE) %>% 
    #   hc_add_theme(hc_theme_gridlight()) 
    
    prediccion_ets4 <- forecast(ets(selectedDataI4()), h = 2*12)
    d <- highchart(type = "stock") %>%
      hc_title(text = "Time Serie") %>%
      hc_subtitle(text = input$nombre_autopista4) %>%
      hc_add_series(selectedDataI4(), name = "IMD") %>%
      hc_add_series(prediccion_ets4, name = "IMD") %>%
      hc_rangeSelector(inputEnabled = TRUE) %>%
      hc_scrollbar(enabled = TRUE) %>%
      hc_add_theme(hc_theme_gridlight())
    return(d)
  })
  
  
#tabPanel "Representacion ARIMA con log":   
  formulaText5 <- reactive({
    
    input$nombre_autopist5
  })
  

  output$caption <- renderText({
    
    formulaText5()
  })
  
  
  selectedDataI5 <- reactive({
    
    if(is.null(input$nombre_autopista))return()
    
    autopistas_TODAS <- autopistas_TODAS %>% 
      filter(nombre == input$nombre_autopista5) %>% 
      select(Mes_numeric, IMD_total) %>% arrange(Mes_numeric)
    
    start_year5 <- as.numeric(year(autopistas_TODAS$Mes_numeric[1]))
    end_year5 <- as.numeric(year(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    start_month5 <- as.numeric(month(autopistas_TODAS$Mes_numeric[1]))
    end_month5 <- as.numeric(month(autopistas_TODAS$Mes_numeric[length(autopistas_TODAS$Mes_numeric)]))
    
    autopista <- ts(autopistas_TODAS$IMD_total, start=c(start_year5, start_month5), end=c(end_year5, end_month5), frequency=12)
    
    log_xts_mensual <- round(log10(autopista), 2)
    
    return(log_xts_mensual)
    })


  output$hcontainer5 <- renderHighchart({
    
    prediccion_ets5 <- forecast(auto.arima(selectedDataI5()), h = 2*12)
    e <- highchart(type = "stock") %>%
      hc_title(text = "Time Serie") %>%
      hc_subtitle(text = input$nombre_autopista5) %>%
      hc_add_series(selectedDataI5(), name = "IMD") %>%
      hc_add_series(prediccion_ets5, name = "IMD") %>%
      hc_rangeSelector(inputEnabled = TRUE) %>%
      hc_scrollbar(enabled = TRUE) %>%
      hc_add_theme(hc_theme_gridlight())
    
    return(e)
  })
}

shinyApp(server = server, ui = ui)
