library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning"))
)

server <- function(input, output, session) {
  learning_server("learning")
}

shinyApp(ui, server)
