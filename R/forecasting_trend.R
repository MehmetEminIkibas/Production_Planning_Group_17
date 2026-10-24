# Forecasting method for trend series: Holt's method (double exponential smoothing)


fc_holt <- function(y, alpha, beta, level0, slope0, h) {
  n <- length(y)
  fitted <- numeric(n)
  level <- level0
  slope <- slope0
  for (t in 1:n) {
    fitted[t] <- level + slope
    new_level <- alpha * y[t] + (1 - alpha) * (level + slope)
    slope <- beta * (new_level - level) + (1 - beta) * slope
    level <- new_level
  }
  list(fitted = fitted, future = level + seq_len(h) * slope, level = level, slope = slope)
}


fc_holt_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Holt's Method (Double Exponential Smoothing)"),
    helpText("S(t) = alpha * D(t) + (1 - alpha) * (S(t-1) + G(t-1))",
             tags$br(),
             "G(t) = beta * (S(t) - S(t-1)) + (1 - beta) * G(t-1)",
             tags$br(),
             "F(t+k) = S(t) + k * G(t)"),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      sliderInput(ns("alpha"), "Level smoothing constant (alpha):", min = 0.01, max = 1, value = 0.2, step = 0.01),
      sliderInput(ns("beta"), "Trend smoothing constant (beta):", min = 0.01, max = 1, value = 0.1, step = 0.01)
    ),
    fc_metric_input(ns),
    fc_init_input(ns, slope = TRUE),
    fc_horizon_input(ns)
  ))
}

fc_holt_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    fc_prefill_init(data, session)

    res <- reactive({
      y <- fc_require(data)
      h <- fc_horizon(input)
      init <- fc_initial_values(input, y)
      if (input$mode == "manual") {
        alpha <- input$alpha
        beta <- input$beta
        note <- "manual"
      } else {
        f <- function(p) fc_objective(y, fc_holt(y, p[1], p[2], init[["level"]], init[["slope"]], 1)$fitted,
                                      input$metric)
        best <- fc_search(f, lower = c(0.01, 0.01), upper = c(1, 1))
        alpha <- best[1]
        beta <- best[2]
        note <- paste("auto, minimum", fc_metric_label(input$metric))
      }
      r <- fc_holt(y, alpha, beta, init[["level"]], init[["slope"]], h)
      r$params <- fc_param_table(
        "Alpha" = alpha,
        "Beta" = beta,
        "Selection" = note,
        "Initial level" = init[["level"]],
        "Initial slope" = init[["slope"]],
        "Initial values from" = if (input$init == "reg") "regression" else "manual",
        "Final level" = r$level,
        "Final slope" = r$slope
      )
      r
    })
    fc_render_results(output, data, res, "Holt's Method")
  })
}
