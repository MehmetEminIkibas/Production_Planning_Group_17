# Forecasting module
# Groups all forecasting methods under one "Forecasting" menu of the app.
# Data is uploaded once on the "Data" page and shared by every method page.
# Method pages live in forecasting_stationary.R, forecasting_trend.R,
# forecasting_seasonal.R and forecasting_boxjenkins.R.

library(shiny)
library(readxl)

forecasting_ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Forecasting",
    tabPanel("Data", fc_data_ui(ns("data"))),
    "----",
    "Stationary series",
    tabPanel("Moving Average", fc_ma_ui(ns("ma"))),
    tabPanel("Weighted Moving Average", fc_wma_ui(ns("wma"))),
    tabPanel("Simple Exponential Smoothing", fc_ses_ui(ns("ses"))),
    tabPanel("Seasonal Factors (Quick and Dirty)", fc_sf_ui(ns("sf"))),
    "----",
    "Trend series",
    tabPanel("Holt's Method", fc_holt_ui(ns("holt"))),
    "----",
    "Seasonal series",
    tabPanel("Seasonal Decomposition", fc_decomp_ui(ns("decomp"))),
    tabPanel("Winters' Method", fc_winters_ui(ns("winters"))),
    "----",
    "Box-Jenkins",
    tabPanel("AR", fc_bj_ui(ns("ar"), "AR")),
    tabPanel("MA", fc_bj_ui(ns("bjma"), "MA")),
    tabPanel("ARMA", fc_bj_ui(ns("arma"), "ARMA")),
    tabPanel("ARIMA", fc_bj_ui(ns("arima"), "ARIMA"))
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- fc_data_server("data")

    fc_ma_server("ma", data)
    fc_wma_server("wma", data)
    fc_ses_server("ses", data)
    fc_sf_server("sf", data)
    fc_holt_server("holt", data)
    fc_decomp_server("decomp", data)
    fc_winters_server("winters", data)
    fc_bj_server("ar", data, "AR")
    fc_bj_server("bjma", data, "MA")
    fc_bj_server("arma", data, "ARMA")
    fc_bj_server("arima", data, "ARIMA")
  })
}


# ---------------------------------------------------------------------------
# Data page: upload a CSV / Excel file and select the series (column)
# ---------------------------------------------------------------------------

fc_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput(ns("file"), "Upload demand data (CSV or Excel):",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      checkboxInput(ns("header"), "First row contains column names", TRUE),
      conditionalPanel(
        condition = "output.is_csv", ns = ns,
        radioButtons(ns("sep"), "CSV separator:",
                     choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                     inline = TRUE),
        radioButtons(ns("dec"), "Decimal mark:",
                     choices = c("Point (.)" = ".", "Comma (,)" = ","),
                     inline = TRUE)
      ),
      uiOutput(ns("sheet_ui")),
      selectInput(ns("series"), "Select series (column):", choices = NULL),
      helpText("Each column is one time series, one row per period.",
               "The selected series is used by every method in the Forecasting menu.")
    ),
    mainPanel(
      width = 8,
      plotOutput(ns("plot"), height = "350px"),
      h4("Summary"),
      tableOutput(ns("summary")),
      h4("Data preview (first 10 rows)"),
      tableOutput(ns("preview"))
    )
  )
}

fc_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    file_ext <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name))
    })

    output$is_csv <- reactive(!is.null(input$file) && file_ext() %in% c("csv", "txt"))
    outputOptions(output, "is_csv", suspendWhenHidden = FALSE)

    output$sheet_ui <- renderUI({
      req(file_ext() %in% c("xlsx", "xls"))
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput(session$ns("sheet"), "Excel sheet:", choices = sheets)
    })

    raw_data <- reactive({
      req(input$file)
      path <- input$file$datapath
      if (file_ext() %in% c("csv", "txt")) {
        df <- read.csv(path, header = input$header, sep = input$sep,
                       dec = input$dec, check.names = FALSE)
      } else if (file_ext() %in% c("xlsx", "xls")) {
        sheet <- if (is.null(input$sheet)) 1 else input$sheet
        df <- as.data.frame(readxl::read_excel(path, sheet = sheet,
                                               col_names = input$header))
      } else {
        validate("Unsupported file type. Please upload a .csv, .xlsx or .xls file.")
      }
      df
    })

    numeric_cols <- reactive({
      df <- raw_data()
      is_num <- vapply(df, function(col) {
        vals <- suppressWarnings(as.numeric(col))
        sum(!is.na(vals)) >= 2
      }, logical(1))
      names(df)[is_num]
    })

    observeEvent(numeric_cols(), {
      cols <- numeric_cols()
      updateSelectInput(session, "series", choices = cols,
                        selected = if (length(cols) > 0) cols[1] else NULL)
    })

    series <- reactive({
      if (is.null(input$file)) return(NULL)
      df <- raw_data()
      if (is.null(input$series) || !(input$series %in% names(df))) return(NULL)
      y <- suppressWarnings(as.numeric(df[[input$series]]))
      y <- y[!is.na(y)]
      if (length(y) < 2) return(NULL)
      list(y = y, name = input$series)
    })

    output$plot <- renderPlot({
      d <- series()
      validate(need(!is.null(d), "Upload a file and select a numeric column."))
      plot(d$y, type = "o", pch = 16, lwd = 2, xlab = "Period", ylab = "Demand",
           main = paste("Series:", d$name))
      grid()
    })

    output$summary <- renderTable({
      d <- series()
      req(d)
      data.frame(
        Measure = c("Number of periods", "Mean", "Standard deviation", "Minimum", "Maximum"),
        Value = c(length(d$y), mean(d$y), sd(d$y), min(d$y), max(d$y))
      )
    }, digits = 2)

    output$preview <- renderTable(head(raw_data(), 10))

    series
  })
}


# ---------------------------------------------------------------------------
# Shared helpers used by every method page
# ---------------------------------------------------------------------------

# Returns the selected series or stops the output with a friendly message
fc_require <- function(data, min_n = 3) {
  d <- data()
  validate(need(!is.null(d), "Please upload a data file and select a series in Forecasting > Data."))
  validate(need(length(d$y) >= min_n,
                paste("This method needs at least", min_n, "observations.")))
  d$y
}

# MAD, MSE and MAPE over the periods that have a forecast
fc_errors <- function(y, fitted) {
  idx <- !is.na(fitted)
  e <- y[idx] - fitted[idx]
  nz <- y[idx] != 0
  c(
    mad = mean(abs(e)),
    mse = mean(e^2),
    mape = if (any(nz)) mean(abs(e[nz]) / abs(y[idx][nz])) * 100 else NA
  )
}

# Error value used as the objective in automatic parameter search
fc_objective <- function(y, fitted, metric) {
  if (all(is.na(fitted))) return(Inf)
  v <- fc_errors(y, fitted)[[metric]]
  if (!is.finite(v)) Inf else v
}

# Searches parameters in [lower, upper] that minimize an error function:
# a coarse grid first, then refined with optim (L-BFGS-B)
fc_search <- function(f, lower, upper, grid_steps = 10) {
  k <- length(lower)
  grids <- lapply(seq_len(k), function(i) seq(lower[i], upper[i], length.out = grid_steps))
  combos <- as.matrix(expand.grid(grids))
  vals <- apply(combos, 1, f)
  start <- combos[which.min(vals), ]
  if (k == 1) {
    best <- optimize(f, lower = lower, upper = upper)
    if (best$objective < min(vals)) return(best$minimum)
    return(start)
  }
  opt <- tryCatch(optim(start, f, method = "L-BFGS-B", lower = lower, upper = upper),
                  error = function(e) NULL)
  if (!is.null(opt) && opt$value <= min(vals)) opt$par else start
}

# Initial level and slope from a regression line y = a + b t (level at t = 0)
fc_regression_init <- function(y) {
  t <- seq_along(y)
  fit <- lm(y ~ t)
  c(level = unname(coef(fit)[1]), slope = unname(coef(fit)[2]))
}

# Season position (1..m) of period t
fc_season_pos <- function(t, m) ((t - 1) %% m) + 1

# Picks the season length with the highest autocorrelation
fc_detect_season <- function(y, max_m) {
  max_m <- min(max_m, floor(length(y) / 2))
  if (max_m < 2) return(2)
  a <- acf(y, lag.max = max_m, plot = FALSE)$acf[-1]
  which.max(a[2:max_m]) + 1
}

fc_param_table <- function(...) {
  p <- list(...)
  data.frame(
    Parameter = names(p),
    Value = vapply(p, function(v) {
      if (is.numeric(v)) format(round(v, 4), nsmall = 0) else as.character(v)
    }, character(1)),
    check.names = FALSE
  )
}

fc_metric_label <- function(metric) c(mad = "MAD", mse = "MSE", mape = "MAPE")[[metric]]


# ---------------------------------------------------------------------------
# Shared UI pieces
# ---------------------------------------------------------------------------

fc_mode_input <- function(ns) {
  radioButtons(ns("mode"), "Parameters:",
               choices = c("Enter manually" = "manual", "Search best value (auto)" = "auto"))
}

fc_metric_input <- function(ns) {
  conditionalPanel(
    condition = "input.mode == 'auto'", ns = ns,
    radioButtons(ns("metric"), "Error measure to minimize:",
                 choices = c("MAD" = "mad", "MSE" = "mse", "MAPE" = "mape"), inline = TRUE)
  )
}

fc_horizon_input <- function(ns, value = 4) {
  numericInput(ns("h"), "Forecast horizon (periods ahead):", value = value, min = 1, max = 100)
}

fc_init_input <- function(ns, slope = TRUE) {
  tagList(
    radioButtons(ns("init"), "Initial values:",
                 choices = c("Fit a regression line" = "reg", "Enter manually" = "manual")),
    conditionalPanel(
      condition = "input.init == 'manual'", ns = ns,
      numericInput(ns("level0"), "Initial level:", value = 0),
      if (slope) numericInput(ns("slope0"), "Initial slope (trend):", value = 0)
    )
  )
}

# Initial level/slope chosen by the user (regression or manual)
fc_initial_values <- function(input, y) {
  if (input$init == "manual") {
    validate(need(!is.na(input$level0), "Enter an initial level."))
    slope <- if (is.null(input$slope0) || is.na(input$slope0)) 0 else input$slope0
    c(level = input$level0, slope = slope)
  } else {
    fc_regression_init(y)
  }
}

# Pre-fills the manual initial value boxes with the regression estimates
fc_prefill_init <- function(data, session) {
  observeEvent(data(), {
    init <- fc_regression_init(data()$y)
    updateNumericInput(session, "level0", value = round(init[["level"]], 2))
    updateNumericInput(session, "slope0", value = round(init[["slope"]], 2))
  })
}

fc_horizon <- function(input) {
  h <- input$h
  if (is.null(h) || is.na(h) || h < 1) 1 else round(h)
}

# Standard layout of a method page: controls on the left, results on the right
fc_method_layout <- function(ns, controls, extra = NULL) {
  sidebarLayout(
    sidebarPanel(width = 4, controls),
    mainPanel(
      width = 8,
      plotOutput(ns("plot"), height = "380px"),
      fluidRow(
        column(6, h4("Parameters"), tableOutput(ns("params"))),
        column(6, h4("Error measures"), tableOutput(ns("errors")))
      ),
      extra,
      h4("Forecasts"),
      tableOutput(ns("table"))
    )
  )
}


# ---------------------------------------------------------------------------
# Shared outputs: plot, parameter table, error table, forecast table
# res() must return list(fitted, future, params, lower = NULL, upper = NULL)
# ---------------------------------------------------------------------------

fc_render_results <- function(output, data, res, method_name) {

  output$plot <- renderPlot({
    r <- res()
    y <- data()$y
    n <- length(y)
    h <- length(r$future)
    all_vals <- c(y, r$fitted, r$future, r$lower, r$upper)
    plot(seq_len(n), y, type = "o", pch = 16, lwd = 2,
         xlim = c(1, n + h), ylim = range(all_vals, na.rm = TRUE),
         xlab = "Period", ylab = "Demand",
         main = paste(method_name, "-", data()$name))
    lines(seq_len(n), r$fitted, type = "o", col = "red", lwd = 2, pch = 1)
    if (!is.null(r$lower)) {
      polygon(c(n + seq_len(h), rev(n + seq_len(h))), c(r$lower, rev(r$upper)),
              col = adjustcolor("blue", 0.15), border = NA)
    }
    lines(n + seq_len(h), r$future, type = "o", col = "blue", lwd = 2, lty = 2, pch = 17)
    abline(v = n + 0.5, col = "grey50", lty = 3)
    legend("topleft", bty = "n",
           legend = c("Actual demand", "Fitted forecast", "Future forecast"),
           col = c("black", "red", "blue"), lty = c(1, 1, 2), pch = c(16, 1, 17), lwd = 2)
    grid()
  })

  output$params <- renderTable(res()$params)

  output$errors <- renderTable({
    r <- res()
    err <- fc_errors(data()$y, r$fitted)
    data.frame(
      Measure = c("MAD", "MSE", "MAPE (%)", "Periods evaluated"),
      Value = c(sprintf("%.3f", c(err[["mad"]], err[["mse"]], err[["mape"]])),
                sum(!is.na(r$fitted)))
    )
  }, align = "lr")

  output$table <- renderTable({
    r <- res()
    y <- data()$y
    n <- length(y)
    h <- length(r$future)
    tab <- data.frame(
      Period = seq_len(n + h),
      Actual = c(y, rep(NA, h)),
      Forecast = c(r$fitted, r$future)
    )
    tab$Error <- tab$Actual - tab$Forecast
    if (!is.null(r$lower)) {
      tab$`Lower 95%` <- c(rep(NA, n), r$lower)
      tab$`Upper 95%` <- c(rep(NA, n), r$upper)
    }
    tab
  }, digits = 3, na = "")
}
