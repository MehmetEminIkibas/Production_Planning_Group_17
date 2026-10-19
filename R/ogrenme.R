learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("L"), 
      label = "Öğrenme Eğrisi Katsayısı (L):", 
      min = 0.60, 
      max = 0.99, 
      value = 0.80, 
      step = 0.01
    ),
    plotOutput(ns("grafik"), width = "500px", height = "400px")
  )
}

learning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive calculation: updates automatically when input$L changes
    chart_data <- reactive({
      req(input$L)
      b <- -log(input$L) / log(2)
      u <- 1:50
      y <- 100 * u^(-b)
      
      data.frame(u = u, y = y)
    })
    
    # Grafik
    output$grafik <- renderPlot({
      df <- chart_data()
      
      plot(
        x = df$u, 
        y = df$y, 
        type = "l", 
        col = "blue", 
        lwd = 2,                      
        xlim = c(1, 50), 
        ylim = c(0, 100), 
        xlab = "Birim Sayısı (u)", 
        ylab = "Performans / Skor",
        main = "Öğrenme Eğrisi"       
      )
      
      grid()
    })
    
  })
}

ui <- fluidPage(
  titlePanel("Modular Shiny Application"),
  sidebarLayout(
    # Sidebar: Control inputs go here
    sidebarPanel(
      width = 8,
      learning_ui("ogrenme_modulu")
    ),
    # Main Panel: Descriptive text or main outputs go here
    mainPanel(
      width = 4,
      p("The control input and plot are managed inside the module.")
    )
  )
)

server <- function(input, output, session) {
  # Fixed: Server ID matches the UI ID exactly
  learning_server("ogrenme_modulu")
}

shinyApp(ui = ui, server = server)