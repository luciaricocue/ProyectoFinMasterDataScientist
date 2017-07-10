# library(shiny)
# ui <- fluidPage("Hello word")
# server <- function(input, output){}
# shinyApp(ui=ui, server=server)
# 
# 
# ui <- fluidPage(
#   # *Input() functions,
#   # *Output() functions
# )
# 
# #Create an input with an *Input() function.
# sliderInput(inputId = "num",
#             label = "Choose a number",
#             value = 25, min = 1, max = 100)
# 
# #esto no muestra nada en la emergente porque está vacío el fluidPage
# ui <- fluidPage(
# )
# server <- function(input, output) {}
# shinyApp(server = server, ui = ui)
# 
# #esto si muestra, puedes elegir un número
# ui <- fluidPage(
#   sliderInput(inputId = "num",
#               label = "Choose a number",
#               value = 25, min = 1, max = 100)
# )
# server <- function(input, output) {}
# shinyApp(server = server, ui = ui)
# 
# #ejemplo con entrada y salida
# ui <- fluidPage(
#   sliderInput(inputId = "num",
#               label = "Choose a number",
#               value = 25, min = 1, max = 100),
#   plotOutput("hist")
# )
# 
# server <-function(input, output) {
#   output$hist <- renderPlot({
#     hist(rnorm(100))
#   })
# }
# shinyApp(server = server, ui = ui)
# 
# ###########OTRO EJEMPLO IGUAL QUE EL ANTERIOR PERO CON TITULO#############
# ui <- fluidPage(
#   sliderInput(inputId = "num",
#               label = "Choose a number",
#               value = 25, min = 1, max = 100),
#   plotOutput("hist")
# )
# 
# server <- function(input, output) {
#   output$hist <- renderPlot({
#     title <- "100 random normal values"
#     hist(rnorm(100), main = title)
#   })
# }
# shinyApp(server = server, ui = ui)
#####ESTE SI SE MUEVE LA SALIDA SEGÚN LAS ENTRADAS!!!################################# 
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))#SE ACCEDE AL VALOR INPUT CON input$ y se pone num porque inputId =   "num"
  })#se crea Create reactivity by using Inputs to build rendered Outputs
}
shinyApp(server = server, ui = ui)



