# Forecasting methods for seasonal series:
# Seasonal decomposition with moving averages, Winters' method (multiplicative)


# ---------------------------------------------------------------------------
# Calculations
# ---------------------------------------------------------------------------

# Centered moving average of length m (2 x m moving average when m is even)
fc_centered_ma <- function(y, m) {
  if (m %% 2 == 1) {
    w <- rep(1 / m, m)
  } else {
    w <- c(0.5, rep(1, m - 1), 0.5) / m
  }
  as.numeric(stats::filter(y, w, sides = 2))
}

fc_decompose <- function(y, m, h) {
  n <- length(y)
  t <- seq_len(n)
  pos <- fc_season_pos(t, m)
  cma <- fc_centered_ma(y, m)
  ratios <- y / cma
  index <- tapply(ratios, pos, mean, na.rm = TRUE)
  index <- index * m / sum(index)
  deseason <- y / index[pos]
  trend <- lm(deseason ~ t)
  a <- unname(coef(trend)[1])
  b <- unname(coef(trend)[2])
  fitted <- (a + b * t) * index[pos]
  future_t <- n + seq_len(h)
  future <- (a + b * future_t) * index[fc_season_pos(future_t, m)]
  list(fitted = unname(fitted), future = unname(future), index = index,
       intercept = a, slope = b, cma = cma)
}

# Initial seasonal factors: data divided by the initial line, averaged per season
fc_winters_init_season <- function(y, m, level0, slope0) {
  t <- seq_along(y)
  ratios <- y / (level0 + slope0 * t)
  s <- tapply(ratios, fc_season_pos(t, m), mean)
  s * m / sum(s)
}

fc_winters <- function(y, m, alpha, beta, gamma, level0, slope0, h) {
  n <- length(y)
  season <- as.numeric(fc_winters_init_season(y, m, level0, slope0))
  season_init <- season
  level <- level0
  slope <- slope0
  fitted <- numeric(n)
  for (t in 1:n) {
    p <- fc_season_pos(t, m)
    fitted[t] <- (level + slope) * season[p]
    new_level <- alpha * y[t] / season[p] + (1 - alpha) * (level + slope)
    slope <- beta * (new_level - level) + (1 - beta) * slope
    level <- new_level
    season[p] <- gamma * y[t] / level + (1 - gamma) * season[p]
  }
  k <- seq_len(h)
  future <- (level + k * slope) * season[fc_season_pos(n + k, m)]
  list(fitted = fitted, future = future, level = level, slope = slope,
       season_init = season_init, season = season)
}

fc_season_length <- function(input, y) {
  if (input$mode_m == "manual") {
    m <- input$m
  } else {
    m <- fc_detect_season(y, input$max_m)
  }
  validate(need(!is.na(m) && m >= 2 && 2 * m <= length(y),
                "Season length must be at least 2 and the data must cover at least two full seasons."))
  m
}

fc_season_input <- function(ns) {
  tagList(
    radioButtons(ns("mode_m"), "Season length:",
                 choices = c("Enter manually" = "manual", "Detect automatically" = "auto"),
                 inline = TRUE),
    conditionalPanel(
      condition = "input.mode_m == 'manual'", ns = ns,
      numericInput(ns("m"), "Periods per season cycle (m):", value = 4, min = 2)
    ),
    conditionalPanel(
      condition = "input.mode_m == 'auto'", ns = ns,
      numericInput(ns("max_m"), "Largest season length to try:", value = 12, min = 2),
      helpText("The season length with the highest autocorrelation is chosen.")
    )
  )
}


# ---------------------------------------------------------------------------
# Seasonal decomposition with moving averages
# ---------------------------------------------------------------------------

fc_decomp_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Seasonal Decomposition (Moving Averages)"),
    helpText("1. Centered moving average of length m.",
             tags$br(), "2. Seasonal index = average of demand / moving average for each season.",
             tags$br(), "3. Deseasonalize the data and fit a regression line (trend).",
             tags$br(), "4. Forecast = trend x seasonal index."),
    fc_season_input(ns),
    fc_horizon_input(ns, value = 8)
  ), extra = tagList(h4("Seasonal indices"), tableOutput(NS(id, "index"))))
}

fc_decomp_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      y <- fc_require(data, min_n = 4)
      h <- fc_horizon(input)
      m <- fc_season_length(input, y)
      r <- fc_decompose(y, m, h)
      r$params <- fc_param_table(
        "Season length" = m,
        "Season length from" = if (input$mode_m == "auto") "autocorrelation" else "manual",
        "Trend intercept" = r$intercept,
        "Trend slope" = r$slope
      )
      r
    })
    output$index <- renderTable({
      idx <- res()$index
      data.frame(Season = seq_along(idx), `Seasonal index` = as.numeric(idx), check.names = FALSE)
    }, digits = 4)
    fc_render_results(output, data, res, "Seasonal Decomposition")
  })
}


# ---------------------------------------------------------------------------
# Winters' method (triple exponential smoothing, multiplicative seasonality)
# ---------------------------------------------------------------------------

fc_winters_ui <- function(id) {
  ns <- NS(id)
  fc_method_layout(ns, tagList(
    h4("Winters' Method"),
    helpText("S(t) = alpha * D(t) / c(t-m) + (1 - alpha) * (S(t-1) + G(t-1))",
             tags$br(), "G(t) = beta * (S(t) - S(t-1)) + (1 - beta) * G(t-1)",
             tags$br(), "c(t) = gamma * D(t) / S(t) + (1 - gamma) * c(t-m)",
             tags$br(), "F(t+k) = (S(t) + k * G(t)) * c(t+k-m)"),
    fc_season_input(ns),
    fc_mode_input(ns),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      sliderInput(ns("alpha"), "Level smoothing constant (alpha):", min = 0.01, max = 1, value = 0.2, step = 0.01),
      sliderInput(ns("beta"), "Trend smoothing constant (beta):", min = 0.01, max = 1, value = 0.1, step = 0.01),
      sliderInput(ns("gamma"), "Seasonal smoothing constant (gamma):", min = 0.01, max = 1, value = 0.1, step = 0.01)
    ),
    fc_metric_input(ns),
    fc_init_input(ns, slope = TRUE),
    helpText("Initial seasonal factors = demand / initial line, averaged per season."),
    fc_horizon_input(ns, value = 8)
  ), extra = tagList(h4("Seasonal factors"), tableOutput(NS(id, "season"))))
}

fc_winters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    fc_prefill_init(data, session)

    res <- reactive({
      y <- fc_require(data, min_n = 4)
      h <- fc_horizon(input)
      m <- fc_season_length(input, y)
      init <- fc_initial_values(input, y)
      validate(need(all(init[["level"]] + init[["slope"]] * seq_along(y) > 0),
                    "The initial line (level + slope x t) must stay positive for Winters' method."))
      if (input$mode == "manual") {
        par <- c(input$alpha, input$beta, input$gamma)
        note <- "manual"
      } else {
        f <- function(p) fc_objective(
          y, fc_winters(y, m, p[1], p[2], p[3], init[["level"]], init[["slope"]], 1)$fitted, input$metric)
        par <- fc_search(f, lower = rep(0.01, 3), upper = rep(1, 3), grid_steps = 6)
        note <- paste("auto, minimum", fc_metric_label(input$metric))
      }
      r <- fc_winters(y, m, par[1], par[2], par[3], init[["level"]], init[["slope"]], h)
      r$params <- fc_param_table(
        "Season length" = m,
        "Alpha" = par[1],
        "Beta" = par[2],
        "Gamma" = par[3],
        "Selection" = note,
        "Initial level" = init[["level"]],
        "Initial slope" = init[["slope"]],
        "Initial values from" = if (input$init == "reg") "regression" else "manual",
        "Final level" = r$level,
        "Final slope" = r$slope
      )
      r
    })
    output$season <- renderTable({
      r <- res()
      data.frame(Season = seq_along(r$season), Initial = r$season_init, Final = r$season)
    }, digits = 4)
    fc_render_results(output, data, res, "Winters' Method")
  })
}
