library(shiny)

ui <- navbarPage("Production Planning - Group 17",
  tabPanel("Learning Curve", learning_ui("learning")),
  forecasting_ui("forecasting")
)

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
}

shinyApp(ui, server)
