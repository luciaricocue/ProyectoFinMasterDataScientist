server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))#SE ACCEDE AL VALOR INPUT CON input$ y se pone num porque inputId =   "num"
  })#se crea Create reactivity by using Inputs to build rendered Outputs
}