library(shiny)
library(readxl)

# Diğer R dosyalarının uygulamaya dahil edilmesi (Source işlemi)

source("R/ogrenme.R", local = TRUE)
source("R/forecasting.R", local = TRUE)
source("R/forecasting_stationary.R", local = TRUE)
source("R/forecasting_trend.R", local = TRUE)
source("R/forecasting_seasonal.R", local = TRUE)
source("R/forecasting_boxjenkins.R", local = TRUE)
source("R/aggregate_planning.R", local = TRUE)

ui <- navbarPage(
  title = "Production Planning & Forecasting System - Group 17",
  
  # 1. Sekme: Öğrenme Eğrisi
  tabPanel("Learning Curve", learning_ui("learning")),
  
  # 2. Sekme Grubu: Tahmin Modülleri (Data alt sekmesini içerir)
  forecasting_ui("forecasting"),
  
  # 3. Sekme: Üretim Planlama ve Ayrıştırma (Eksik olan kısım eklendi)
  aggregate_planning_ui("aggregate")
)

server <- function(input, output, session) {
  # Modüllerin çalıştırılması
  learning_server("learning")
  
  # Forecasting modülü çağrılır ve ürettiği reaktif veri nesnesi saklanır
  shared_demand_data <- forecasting_server("forecasting")
  
  # Üretim planlama modülüne tahmin/veri sayfasından gelen reaktif veri bağlanır
  aggregate_planning_server("aggregate", shared_data = shared_demand_data)
}

shinyApp(ui, server)
