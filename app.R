library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Moving Average", ma_ui("ma"))
)

server <- function(input, output, session) {
  learning_server("learning")
  ma_server("ma")
}

shinyApp(ui, server)
