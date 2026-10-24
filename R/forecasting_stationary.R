# Forecasting methods for stationary series:
# Moving Average, Weighted Moving Average, Simple Exponential Smoothing,
# Seasonal Factors (quick and dirty method)


# ---------------------------------------------------------------------------
# Calculations
# ---------------------------------------------------------------------------

# Weighted moving average; w[1] is the weight of the most recent period.
# A simple moving average is the special case w = rep(1 / N, N).
fc_wma <- function(y, w, h) {
  n <- length(y)
  N <- length(w)
  w <- w / sum(w)
  fitted <- rep(NA, n)
  if (N < n) {
    for (t in (N + 1):n) {
      fitted[t] <- sum(w * y[(t - 1):(t - N)])
    }
  }
  future <- rep(sum(w * y[n:(n - N + 1)]), h)
  list(fitted = fitted, future = future, weights = w)
}

fc_ma <- function(y, N, h) fc_wma(y, rep(1 / N, N), h)

fc_ses <- function(y, alpha, level0, h) {
  n <- length(y)
  fitted <- numeric(n)
  level <- level0
  for (t in 1:n) {
    fitted[t] <- level
    level <- alpha * y[t] + (1 - alpha) * level
  }
  list(fitted = fitted, future = rep(level, h), level = level)
}

# Quick and dirty seasonal factors: season average / overall average
fc_seasonal_factors <- function(y, m, h) {
  n <- length(y)
  pos <- fc_season_pos(seq_len(n), m)
  overall <- mean(y)
  factors <- tapply(y / overall, pos, mean)
  factors <- factors * m / sum(factors)
  fitted <- overall * factors[pos]
  future <- overall * factors[fc_season_pos(n + seq_len(h), m)]
  list(fitted = unname(fitted), future = unname(future), factors = factors, overall = overall)
}

fc_parse_weights <- function(text) {
  w <- suppressWarnings(as.numeric(strsplit(text, "[,; ]+")[[1]]))
  w[!is.na(w)]
}

fc_softmax <- function(p) exp(p) / sum(exp(p))


# ---------------------------------------------------------------------------
# Moving Average
# ---------------------------------------------------------------------------

fc_ma_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Moving Average"),
    helpText("Forecast = average of the last N observations."),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      sliderInput(ns("N"), "Moving average period (N):", min = 1, max = 20, value = 3, step = 1)
    ),
    conditionalPanel(
      condition = "input.mode == 'auto'", ns = ns,
      sliderInput(ns("max_N"), "Largest N to try:", min = 1, max = 20, value = 10, step = 1)
    ),
    fc_metric_input(ns),
    fc_horizon_input(ns)
  ))
}

fc_ma_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      y <- fc_require(data)
      n <- length(y)
      h <- fc_horizon(input)
      if (input$mode == "manual") {
        N <- input$N
        validate(need(N < n, "N must be smaller than the number of observations."))
        note <- "manual"
      } else {
        candidates <- 1:min(input$max_N, n - 1)
        scores <- sapply(candidates, function(N) fc_objective(y, fc_ma(y, N, 1)$fitted, input$metric))
        N <- candidates[which.min(scores)]
        note <- paste("auto, minimum", fc_metric_label(input$metric))
      }
      r <- fc_ma(y, N, h)
      r$params <- fc_param_table("N" = N, "Selection" = note)
      r
    })
    fc_render_results(output, data, res, "Moving Average")
  })
}


# ---------------------------------------------------------------------------
# Weighted Moving Average
# ---------------------------------------------------------------------------

fc_wma_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Weighted Moving Average"),
    helpText("Forecast = weighted average of the last N observations.",
             "Weights are normalized to sum to 1."),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      textInput(ns("weights"), "Weights, most recent period first (comma separated):",
                value = "0.5, 0.3, 0.2")
    ),
    conditionalPanel(
      condition = "input.mode == 'auto'", ns = ns,
      sliderInput(ns("max_N"), "Largest N to try:", min = 1, max = 10, value = 4, step = 1),
      helpText("For each N the best weights are searched, then the best N is kept.")
    ),
    fc_metric_input(ns),
    fc_horizon_input(ns)
  ))
}

fc_wma_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      y <- fc_require(data)
      n <- length(y)
      h <- fc_horizon(input)
      if (input$mode == "manual") {
        w <- fc_parse_weights(input$weights)
        validate(need(length(w) >= 1 && all(w >= 0) && sum(w) > 0,
                      "Enter non-negative weights, e.g. 0.5, 0.3, 0.2"))
        validate(need(length(w) < n, "Number of weights must be smaller than the number of observations."))
        note <- "manual"
      } else {
        best <- list(score = Inf, w = 1)
        for (N in 1:min(input$max_N, n - 1)) {
          if (N == 1) {
            w_N <- 1
          } else {
            f <- function(p) fc_objective(y, fc_wma(y, fc_softmax(p), 1)$fitted, input$metric)
            w_N <- fc_softmax(optim(rep(0, N), f)$par)
          }
          score <- fc_objective(y, fc_wma(y, w_N, 1)$fitted, input$metric)
          if (score < best$score) best <- list(score = score, w = w_N)
        }
        w <- best$w
        note <- paste("auto, minimum", fc_metric_label(input$metric))
      }
      r <- fc_wma(y, w, h)
      weights <- as.list(r$weights)
      names(weights) <- paste0("Weight of period t-", seq_along(weights))
      r$params <- rbind(fc_param_table("N" = length(w), "Selection" = note),
                        do.call(fc_param_table, weights))
      r
    })
    fc_render_results(output, data, res, "Weighted Moving Average")
  })
}


# ---------------------------------------------------------------------------
# Simple Exponential Smoothing
# ---------------------------------------------------------------------------

fc_ses_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Simple Exponential Smoothing"),
    helpText("F(t+1) = alpha * D(t) + (1 - alpha) * F(t)"),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      sliderInput(ns("alpha"), "Smoothing constant (alpha):", min = 0.01, max = 1, value = 0.2, step = 0.01)
    ),
    fc_metric_input(ns),
    fc_init_input(ns, slope = FALSE),
    fc_horizon_input(ns)
  ))
}

fc_ses_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    fc_prefill_init(data, session)

    res <- reactive({
      y <- fc_require(data)
      h <- fc_horizon(input)
      level0 <- fc_initial_values(input, y)[["level"]]
      if (input$mode == "manual") {
        alpha <- input$alpha
        note <- "manual"
      } else {
        f <- function(a) fc_objective(y, fc_ses(y, a, level0, 1)$fitted, input$metric)
        alpha <- fc_search(f, lower = 0.01, upper = 1, grid_steps = 100)
        note <- paste("auto, minimum", fc_metric_label(input$metric))
      }
      r <- fc_ses(y, alpha, level0, h)
      r$params <- fc_param_table(
        "Alpha" = alpha,
        "Selection" = note,
        "Initial level" = level0,
        "Initial values from" = if (input$init == "reg") "regression" else "manual"
      )
      r
    })
    fc_render_results(output, data, res, "Simple Exponential Smoothing")
  })
}


# ---------------------------------------------------------------------------
# Seasonal Factors (quick and dirty)
# ---------------------------------------------------------------------------

fc_sf_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Seasonal Factors (Quick and Dirty)"),
    helpText("For a stationary series with seasonality:",
             "factor = average of the season / overall average,",
             "forecast = overall average x seasonal factor."),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      numericInput(ns("m"), "Season length (periods per cycle):", value = 4, min = 2)
    ),
    conditionalPanel(
      condition = "input.mode == 'auto'", ns = ns,
      numericInput(ns("max_m"), "Largest season length to try:", value = 12, min = 2),
      helpText("The season length with the highest autocorrelation is chosen.")
    ),
    fc_horizon_input(ns)
  ), extra = tagList(h4("Seasonal factors"), tableOutput(NS(id, "factors"))))
}

fc_sf_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      y <- fc_require(data, min_n = 4)
      h <- fc_horizon(input)
      if (input$mode == "manual") {
        m <- input$m
        note <- "manual"
      } else {
        m <- fc_detect_season(y, input$max_m)
        note <- "auto (autocorrelation)"
      }
      validate(need(!is.na(m) && m >= 2 && m <= length(y),
                    "Season length must be between 2 and the number of observations."))
      r <- fc_seasonal_factors(y, m, h)
      r$params <- fc_param_table("Season length" = m, "Selection" = note,
                                 "Overall average" = r$overall)
      r
    })
    output$factors <- renderTable({
      f <- res()$factors
      data.frame(Season = seq_along(f), Factor = as.numeric(f))
    }, digits = 4)
    fc_render_results(output, data, res, "Seasonal Factors")
  })
}
