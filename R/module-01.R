library(shiny)
library(readxl)

ma_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fileInput(ns("veri_dosyasi"), "Talep Verisi Yükle (CSV veya Excel):", 
              accept = c(".csv", ".xlsx", ".xls")),
    tags$hr(),
    
    radioButtons(ns("mod"), "Hesaplama Modu:", 
                 choices = c("Manuel" = "manual", "Otomatik" = "auto")),
    
    sliderInput(inputId = ns("N"), 
                label = "Moving Average Period (N):", 
                min = 1, max = 20, value = 5, step = 1),
    
    tags$hr(),
    
    sliderInput(ns("max_N"), "Moving Average Period Maximum Trial (max N):", 
                min = 1, max = 20, value = 5, step = 1),
    
    radioButtons(ns("error_type"), "Error Type (MAD, MSE, MAPE):", 
                 choices = c("MAD" = "mad", "MSE" = "mse", "MAPE" = "mape")),
    
    plotOutput(ns("ma_grafik"), width = "500px", height = "500px"),
    verbatimTextOutput(ns("hata_sonuclari"))
  )
}


ma_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demand_veri <- reactive({
      req(input$veri_dosyasi)
      
      dosya_yolu <- input$veri_dosyasi$datapath
      uzanti <- tools::file_ext(input$veri_dosyasi$name)
      

      if (uzanti == "csv") {
        veri <- read.csv(dosya_yolu)
      } else if (uzanti %in% c("xls", "xlsx")) {
        veri <- readxl::read_excel(dosya_yolu)
      } else {
        stop("Lütfen geçerli bir format yükleyin.")
      }
      
      return(as.numeric(veri[[1]])) 
    })
    
    
    hata_hesapla <- function(N_degeri, dinamik_demand) {
      forecasts <- rep(NA, length(dinamik_demand))
      for (t in (N_degeri + 1):length(dinamik_demand)) { 
        sum = 0
        for (i in 1:N_degeri){
          sum = sum + dinamik_demand[t-i]
        }
        forecasts[t]= sum/N_degeri
      }
      
      gercek_degerler <- dinamik_demand[(N_degeri + 1):length(dinamik_demand)]
      tahmin_edilenler <- forecasts[(N_degeri + 1):length(forecasts)]
      hatalar <- gercek_degerler - tahmin_edilenler
      
      MAD <- mean(abs(hatalar))
      MSE <- mean(hatalar^2)
      MAPE <- mean(abs(hatalar) / gercek_degerler) * 100
      
      return(list(mad = MAD, mse = MSE, mape = MAPE))
    }
    
  
    aktif_N <- reactive({
      req(demand_veri()) 
      current_demand <- demand_veri() 
      
      if (input$mod == "manual") {
        return(input$N)
      } else {
        en_iyi_hata_skoru <- list(mad = Inf, mse = Inf, mape = Inf) 
        en_iyi_n <- list(mad = 1, mse = 1, mape = 1)
        
        
        güvenli_max_N <- min(input$max_N, length(current_demand) - 1)
        
        for (m in 1:güvenli_max_N) {
          suanki_hata <- hata_hesapla(m, current_demand)
          
          if (suanki_hata$mad < en_iyi_hata_skoru$mad) {
            en_iyi_n$mad <- m
            en_iyi_hata_skoru$mad <- suanki_hata$mad
          }
          if (suanki_hata$mse < en_iyi_hata_skoru$mse) {
            en_iyi_n$mse <- m
            en_iyi_hata_skoru$mse <- suanki_hata$mse
          }
          if (suanki_hata$mape < en_iyi_hata_skoru$mape) { 
            en_iyi_n$mape <- m
            en_iyi_hata_skoru$mape <- suanki_hata$mape
          }
        }
        return(en_iyi_n[[input$error_type]])
      }
    })
    
    output$ma_grafik <- renderPlot({
      req(demand_veri())
      current_demand <- demand_veri()
      N <- aktif_N() 
      
      forecasts <- rep(NA, length(current_demand))
      for (t in (N + 1):length(current_demand)) { 
        sum = 0
        for (i in 1:N){
          sum = sum + current_demand[t-i]
        }
        forecasts[t]= sum/N
      }
      

      max_y <- max(current_demand, na.rm = TRUE) + 5 
      
      plot(current_demand, type = "o", col = "black", ylim = c(0, max_y), lwd = 2,
           ylab = "Talep", xlab = "Zaman", main = "Hareketli Ortalama Tahmini")
      lines(forecasts, type = "o", col = "red", lwd = 2)
      legend("topleft", legend = c("Gerçek Talep", paste("MA ( N =", N, ")")), 
             col = c("black", "red"), lty = 1, pch = 1, lwd = 2)
    })
    
    output$hata_sonuclari <- renderText({
      req(demand_veri())
      current_demand <- demand_veri()
      N <- aktif_N()
      sonuclar <- hata_hesapla(N, current_demand)
      
      paste(
        "Hesaplama Modu:", toupper(input$mod), "\n",
        "Kullanılan N Değeri:", N, "\n",
        "--------------------------\n",
        "MAD  :", round(sonuclar$mad, 2), "\n",
        "MSE  :", round(sonuclar$mse, 2), "\n",
        "MAPE : %", round(sonuclar$mape, 2)
      )
    })
  })
}

ui <- fluidPage(
  titlePanel("Talep Tahminleme Yazılımı (Forecasting)"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      ma_ui("ma_modulu")
    ),
    mainPanel(
      width = 8,
      h4("Hoş Geldiniz!"),
      p("Lütfen sol taraftan tek sütunlu bir Excel (.xlsx) veya CSV dosyası yükleyerek tahminlemeye başlayın.")
    )
  )
)

server <- function(input, output, session) {
  ma_server("ma_modulu")
}

shinyApp(ui = ui, server = server)