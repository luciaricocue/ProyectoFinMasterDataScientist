

server <- function(input, output) {
  output$kk <- renderPlot({
    hist(rnorm(input$cosa))#SE ACCEDE AL VALOR INPUT CON input$ y se pone num porque inputId =   "num"
  })#se crea Create reactivity by using Inputs to build rendered Outputs
}

https://github.com/jbkunst/highcharter

devtools::install_github("jbkunst/highcharter")
install_github("luciaricocue/PracticaR")

highcharter