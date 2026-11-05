library(shiny)
library(readxl)

# Source all project modules
source("R/aggregate_planning.R")

ui <- navbarPage("Production Planning - Group 17",
  tabPanel("Learning Curve", learning_ui("learning")),
  forecasting_ui("forecasting"),
  # Appending your module panel to the master application menu
  aggregate_planning_ui("aggregate_planning")
)

server <- function(input, output, session) {
  # Share the reactively loaded data stream across modules
  shared_data <- forecasting_server("forecasting")
  
  learning_server("learning")
  

  aggregate_planning_server("aggregate_planning", shared_data)
}

shinyApp(ui, server)
