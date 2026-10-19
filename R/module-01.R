ma_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    radioButtons(ns("mod"), "Hesaplama Modu:", 
                 choices = c("Manuel" = "manual", "Otomatik" = "auto")),
    
    sliderInput(
      inputId = ns("N"), 
      label = "Moving Average Period (N):", 
      min = 1, 
      max = 20, 
      value = 5, 
      step = 1
    ),
    tags$hr(),
    sliderInput(ns("max_N"), "Moving Average Period Maximum Trial (max N):", min = 1, max = 20, value = 5, step = 1),
    radioButtons(ns("error_type"), "Error Type (MAD, MSE, MAPE):", 
                 choices = c("MAD" = "mad", "MSE" = "mse", "MAPE" = "mape")),
    plotOutput(ns("ma_grafik"), width = "500px", height = "500px"),
    verbatimTextOutput(ns("hata_sonuclari"))
  )
}


ma_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demand <- c(
      10, 14, 12, 16, 14, 18, 17, 20, 19, 22,
      21, 24, 23, 26, 24, 28, 27, 30, 29, 32
    )
    
    # SET 1: Sabit ve rastgele dalgalanan talep (Durağan)
    demand <- c(20, 22, 19, 21, 24, 20, 18, 23, 21, 22, 25, 19, 20, 22, 24, 21, 23, 20, 18, 22)
    
    # SET 2: Sürekli yükselen talep (Trend)
    demand <- c(10, 12, 15, 14, 18, 20, 22, 25, 24, 28, 30, 32, 35, 34, 38, 40, 42, 45, 48, 50)
    
    # SET 3: Belirli aralıklarla inip çıkan talep (Mevsimsel/Döngüsel)
    demand <- c(15, 30, 45, 20, 15, 32, 48, 22, 16, 35, 50, 25, 18, 33, 46, 21, 14, 31, 47, 24)
    
    demand <- c(10, 10, 10, 10, 10, 100, 10, 10, 10, 10)
    
    hata_hesapla <- function(N_degeri) {
      forecasts <- rep(NA, length(demand))
      for (t in (N_degeri + 1):length(demand)) { 
        sum = 0
        for (i in 1:N_degeri){
          sum = sum + demand[t-i]
        }
        forecasts[t]= sum/N_degeri
      }
      
      gercek_degerler <- demand[(N_degeri + 1):length(demand)]
      tahmin_edilenler <- forecasts[(N_degeri + 1):length(forecasts)]
      hatalar <- gercek_degerler - tahmin_edilenler
      
      MAD <- 0
      MSE <- 0
      MAPE <- 0 
      
      for (i in 1:length(hatalar)) {
        MAD <- MAD + abs(hatalar[i])
        MSE <- MSE + hatalar[i]^2
        MAPE <- MAPE + (abs(hatalar[i]) / gercek_degerler[i])
      }
      
      MAD = MAD / length(hatalar)
      MSE = MSE / length(hatalar)
      MAPE = 100 * (MAPE / length(hatalar))
      
      return(list(mad = MAD, mse = MSE, mape = MAPE))
    }
    
    aktif_N <- reactive({
      if (input$mod == "manual") {
        return(input$N)
      } else {
        
        en_iyi_hata_skoru <- list(mad = Inf, mse = Inf, mape = Inf) 
        en_iyi_n <- list(mad = 1, mse = 1, mape = 1)
        
        for (m in 1:input$max_N) {
          suanki_hata <- hata_hesapla(m)
          
          # MAD Kontrolü
          if (suanki_hata$mad < en_iyi_hata_skoru$mad) {
            en_iyi_n$mad <- m
            en_iyi_hata_skoru$mad <- suanki_hata$mad  # Atama yönü düzeltildi!
          }
          # MSE Kontrolü
          if (suanki_hata$mse < en_iyi_hata_skoru$mse) {
            en_iyi_n$mse <- m
            en_iyi_hata_skoru$mse <- suanki_hata$mse
          }
          # MAPE Kontrolü
          if (suanki_hata$mape < en_iyi_hata_skoru$mape) { # Typo (map) düzeltildi!
            en_iyi_n$mape <- m
            en_iyi_hata_skoru$mape <- suanki_hata$mape
          }
        }
        
        return(en_iyi_n[[input$error_type]])
      }
    })
    output$ma_grafik <- renderPlot({
      N <- aktif_N() 
      
      forecasts <- rep(NA, length(demand))
      for (t in (N + 1):length(demand)) { 
        sum = 0
        for (i in 1:N){
          sum = sum + demand[t-i]
        }
        forecasts[t]= sum/N
      }
      
      plot(demand, type = "o", col = "black", ylim = c(0, 50), lwd = 2,
           ylab = "Talep", xlab = "Zaman (Gün)", main = "Hareketli Ortalama Tahmini")
      lines(forecasts, type = "o", col = "red", lwd = 2)
      legend("topleft", legend = c("Gerçek Talep", paste("MA ( N =", N, ")")), 
             col = c("black", "red"), lty = 1, pch = 1, lwd = 2)
    })
    
    output$hata_sonuclari <- renderText({
      N <- aktif_N()
      sonuclar <- hata_hesapla(N)
      
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
  titlePanel("Modular Shiny Application"),
  sidebarLayout(
    # Sidebar: Control inputs go here
    sidebarPanel(
      width = 8,
      ma_ui("ma_modulu")
    ),
    # Main Panel: Descriptive text or main outputs go here
    mainPanel(
      width = 4,
      p("Hello there!")
    )
  )
)

server <- function(input, output, session) {
  # Fixed: Server ID matches the UI ID exactly
  ma_server("ma_modulu")
}

shinyApp(ui = ui, server = server)