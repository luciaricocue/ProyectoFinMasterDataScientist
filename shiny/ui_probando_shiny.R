ui <- fluidPage(
  sliderInput(inputId = "cosa",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("kk")
)

