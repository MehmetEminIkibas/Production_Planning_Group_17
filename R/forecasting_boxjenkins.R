# Box-Jenkins forecasting: AR, MA, ARMA and ARIMA models.
# One module is reused for the four menu items; `type` decides which orders
# (p, d, q) the user can set. Models are fitted with stats::arima.


# ---------------------------------------------------------------------------
# Calculations
# ---------------------------------------------------------------------------

fc_arima_fit <- function(y, p, d, q) {
  tryCatch(
    suppressWarnings(arima(y, order = c(p, d, q), include.mean = (d == 0), method = "CSS-ML")),
    error = function(e) NULL
  )
}

# Differencing order with the smallest standard deviation (over-differencing check)
fc_choose_d <- function(y, max_d) {
  sds <- sapply(0:max_d, function(d) if (d == 0) sd(y) else sd(diff(y, differences = d)))
  which.min(sds) - 1
}

# Orders allowed for each model type
fc_bj_orders <- function(type, max_p, max_d, max_q) {
  grid <- expand.grid(p = 0:max_p, d = 0:max_d, q = 0:max_q)
  grid <- switch(type,
    AR = grid[grid$p >= 1 & grid$q == 0 & grid$d == 0, ],
    MA = grid[grid$p == 0 & grid$q >= 1 & grid$d == 0, ],
    ARMA = grid[grid$p >= 1 & grid$q >= 1 & grid$d == 0, ],
    ARIMA = grid
  )
  grid
}

# Fits every allowed order and keeps the model with the lowest AIC
fc_arima_auto <- function(y, type, max_p, max_d, max_q) {
  d <- if (type == "ARIMA") fc_choose_d(y, max_d) else 0
  orders <- fc_bj_orders(type, max_p, d, max_q)
  orders <- orders[orders$d == d, ]
  best <- NULL
  for (i in seq_len(nrow(orders))) {
    fit <- fc_arima_fit(y, orders$p[i], orders$d[i], orders$q[i])
    if (!is.null(fit) && is.finite(fit$aic) && (is.null(best) || fit$aic < best$fit$aic)) {
      best <- list(fit = fit, p = orders$p[i], d = orders$d[i], q = orders$q[i])
    }
  }
  best
}

fc_arima_result <- function(y, fit, d, h) {
  n <- length(y)
  fitted <- as.numeric(y - residuals(fit))
  if (d > 0) fitted[seq_len(d)] <- NA
  pred <- predict(fit, n.ahead = h)
  list(
    fitted = fitted,
    future = as.numeric(pred$pred),
    lower = as.numeric(pred$pred - 1.96 * pred$se),
    upper = as.numeric(pred$pred + 1.96 * pred$se)
  )
}


# ---------------------------------------------------------------------------
# Module
# ---------------------------------------------------------------------------

fc_bj_ui <- function(id, type) {
  ns <- NS(id)
  has_p <- type %in% c("AR", "ARMA", "ARIMA")
  has_q <- type %in% c("MA", "ARMA", "ARIMA")
  has_d <- type == "ARIMA"
  title <- switch(type,
    AR = "Autoregressive Model AR(p)",
    MA = "Moving Average Model MA(q)",
    ARMA = "ARMA(p, q) Model",
    ARIMA = "ARIMA(p, d, q) Model"
  )

  fc_method_layout(ns, tagList(
    h4(title),
    helpText("Use the ACF and PACF plots to identify the model orders.",
             "AR(p): PACF cuts off after lag p. MA(q): ACF cuts off after lag q."),
    radioButtons(ns("mode"), "Model orders:",
                 choices = c("Enter manually" = "manual", "Search best model (lowest AIC)" = "auto")),
    conditionalPanel(
      condition = "input.mode == 'manual'", ns = ns,
      if (has_p) numericInput(ns("p"), "AR order (p):", value = 1, min = if (type == "ARIMA") 0 else 1, max = 10),
      if (has_d) numericInput(ns("d"), "Differencing order (d):", value = 1, min = 0, max = 2),
      if (has_q) numericInput(ns("q"), "MA order (q):", value = 1, min = if (type == "ARIMA") 0 else 1, max = 10)
    ),
    conditionalPanel(
      condition = "input.mode == 'auto'", ns = ns,
      if (has_p) numericInput(ns("max_p"), "Largest p to try:", value = 3, min = 1, max = 5),
      if (has_d) numericInput(ns("max_d"), "Largest d to try:", value = 2, min = 0, max = 2),
      if (has_q) numericInput(ns("max_q"), "Largest q to try:", value = 3, min = 1, max = 5),
      if (has_d) helpText("d is chosen as the differencing order with the smallest standard deviation;",
                          "then p and q are chosen by the lowest AIC.")
    ),
    numericInput(ns("lags"), "Number of lags in ACF / PACF:", value = 12, min = 2, max = 50),
    fc_horizon_input(ns, value = 6)
  ), extra = tagList(
    h4("ACF and PACF"),
    plotOutput(ns("acf"), height = "300px"),
    h4("Parameter estimates"),
    tableOutput(ns("coef"))
  ))
}

fc_bj_server <- function(id, data, type) {
  moduleServer(id, function(input, output, session) {

    model <- reactive({
      y <- fc_require(data, min_n = 8)
      if (input$mode == "manual") {
        p <- if (type %in% c("AR", "ARMA", "ARIMA")) input$p else 0
        d <- if (type == "ARIMA") input$d else 0
        q <- if (type %in% c("MA", "ARMA", "ARIMA")) input$q else 0
        validate(need(!any(is.na(c(p, d, q))) && all(c(p, d, q) >= 0), "Enter non-negative orders."))
        fit <- fc_arima_fit(y, p, d, q)
        validate(need(!is.null(fit), "The model could not be fitted. Try other orders."))
        list(fit = fit, p = p, d = d, q = q, note = "manual")
      } else {
        max_p <- if (is.null(input$max_p)) 0 else input$max_p
        max_d <- if (is.null(input$max_d)) 0 else input$max_d
        max_q <- if (is.null(input$max_q)) 0 else input$max_q
        best <- fc_arima_auto(y, type, max_p, max_d, max_q)
        validate(need(!is.null(best), "No model could be fitted. Try other limits."))
        best$note <- "auto, lowest AIC"
        best
      }
    })

    res <- reactive({
      y <- fc_require(data, min_n = 8)
      m <- model()
      r <- fc_arima_result(y, m$fit, m$d, fc_horizon(input))
      lb_lag <- min(10, floor(length(y) / 4))
      lb <- Box.test(residuals(m$fit), lag = max(lb_lag, m$p + m$q + 1),
                     type = "Ljung-Box", fitdf = m$p + m$q)
      r$params <- fc_param_table(
        "Model" = sprintf("ARIMA(%d, %d, %d)", m$p, m$d, m$q),
        "Selection" = m$note,
        "AIC" = m$fit$aic,
        "Log-likelihood" = m$fit$loglik,
        "Residual variance (sigma^2)" = m$fit$sigma2,
        "Ljung-Box p-value (residuals)" = lb$p.value
      )
      r
    })

    output$acf <- renderPlot({
      y <- fc_require(data, min_n = 8)
      d <- if (type == "ARIMA") model()$d else 0
      x <- if (d > 0) diff(y, differences = d) else y
      lags <- min(input$lags, length(x) - 1)
      label <- if (d > 0) sprintf("(differenced %d time%s)", d, if (d > 1) "s" else "") else ""
      par(mfrow = c(1, 2))
      acf(x, lag.max = lags, main = paste("ACF", label))
      pacf(x, lag.max = lags, main = paste("PACF", label))
    })

    output$coef <- renderTable({
      fit <- model()$fit
      est <- coef(fit)
      validate(need(length(est) > 0, "The model has no estimated coefficients."))
      se <- sqrt(diag(fit$var.coef))
      data.frame(
        Coefficient = names(est),
        Estimate = as.numeric(est),
        `Std. error` = as.numeric(se),
        `z value` = as.numeric(est / se),
        `p value` = as.numeric(2 * pnorm(-abs(est / se))),
        check.names = FALSE
      )
    }, digits = 4)

    fc_render_results(output, data, res, paste(type, "Model"))
  })
}
