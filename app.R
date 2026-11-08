library(shiny)
library(readxl)

# Load the module files from the R folder

source("R/learning_curve.R", local = TRUE)
source("R/forecasting.R", local = TRUE)
source("R/forecasting_stationary.R", local = TRUE)
source("R/forecasting_trend.R", local = TRUE)
source("R/forecasting_seasonal.R", local = TRUE)
source("R/forecasting_boxjenkins.R", local = TRUE)
source("R/aggregate_planning.R", local = TRUE)
source("R/aggregate_heuristics.R", local = TRUE)
source("R/aggregate_lp.R", local = TRUE)
source("R/aggregate_transport.R", local = TRUE)
source("R/aggregate_disagg.R", local = TRUE)

ui <- navbarPage(
  title = "Production Planning & Forecasting System - Group 17",

  # 1. Learning curve
  tabPanel("Learning Curve", learning_ui("learning")),

  # 2. Forecasting menu (includes the Data page)
  forecasting_ui("forecasting"),

  # 3. Aggregate planning menu (includes its own Data & Costs page)
  aggregate_planning_ui("aggregate")
)

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
  aggregate_planning_server("aggregate")
}

shinyApp(ui, server)
