learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("L"),
      label = "Learning curve rate (L):",
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

    # Plot
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
        xlab = "Unit number (u)",
        ylab = "Time per unit",
        main = "Learning Curve"
      )

      grid()
    })

  })
}
