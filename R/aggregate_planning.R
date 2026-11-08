# Aggregate Planning module
# The first version of this module (assignment 3) was made by Koray Örnek, student number: 21831457.
#
# Groups all aggregate planning methods under one "Aggregate Planning" menu of the app.
# Data, costs and plant data are entered once on the "Data & Costs" page and shared
# by every method page.
# Method pages live in aggregate_heuristics.R (chase, level, mixed), aggregate_lp.R
# (linear programming), aggregate_transport.R (transportation method) and
# aggregate_disagg.R (disaggregation).

library(shiny)
library(readxl)

aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Aggregate Planning",
    tabPanel("Data & Costs", ap_data_ui(ns("data"))),
    "----",
    "Heuristic plans",
    tabPanel("Zero-Inventory (Chase) Plan", ap_chase_ui(ns("chase"))),
    tabPanel("Constant Workforce (Level) Plan", ap_level_ui(ns("level"))),
    tabPanel("Mixed Plan", ap_mixed_ui(ns("mixed"))),
    "----",
    "Optimization",
    tabPanel("Linear Programming", ap_lp_ui(ns("lp"))),
    tabPanel("Transportation Method", ap_tr_ui(ns("tr"))),
    "----",
    "Disaggregation",
    tabPanel("Disaggregation (Square-Root Rule)", ap_disagg_ui(ns("disagg")))
  )
}

aggregate_planning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    setup <- ap_data_server("data")

    plans <- list(
      chase = ap_chase_server("chase", setup),
      level = ap_level_server("level", setup),
      mixed = ap_mixed_server("mixed", setup),
      lp = ap_lp_server("lp", setup),
      tr = ap_tr_server("tr", setup)
    )

    ap_disagg_server("disagg", setup, plans)

    invisible(setup)
  })
}


# ---------------------------------------------------------------------------
# Demo data: Densepack example (Nahmias, Production and Operations Analysis)
# ---------------------------------------------------------------------------

ap_demo_data <- function() {
  data.frame(
    Period = c("January", "February", "March", "April", "May", "June"),
    Demand = c(1280, 640, 900, 1200, 2000, 1400),
    WorkingDays = c(20, 24, 18, 26, 22, 15),
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# Data & Costs page
# ---------------------------------------------------------------------------

ap_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Demand data"),
      fileInput(ns("file"), "Upload data (CSV or Excel):",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      helpText("One row per period. Columns: period names, demand forecasts and working days.",
               "Until a file is uploaded, the Densepack example data is used."),
      checkboxInput(ns("header"), "First row contains column names", TRUE),
      conditionalPanel(
        condition = "output.is_csv", ns = ns,
        radioButtons(ns("sep"), "CSV separator:",
                     choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"), inline = TRUE),
        radioButtons(ns("dec"), "Decimal mark:",
                     choices = c("Point (.)" = ".", "Comma (,)" = ","), inline = TRUE)
      ),
      uiOutput(ns("sheet_ui")),
      selectInput(ns("col_period"), "Period name column:", choices = NULL),
      selectInput(ns("col_demand"), "Demand forecast column:", choices = NULL),
      selectInput(ns("col_days"), "Working days column:", choices = NULL),
      conditionalPanel(
        condition = "input.col_days == '(none)'", ns = ns,
        numericInput(ns("const_days"), "Working days in every period:", value = 21, min = 1)
      ),
      textAreaInput(ns("period_names"), "Period names (comma separated, editable):",
                    value = "", rows = 2),

      hr(),
      h4("2. Costs"),
      numericInput(ns("c_hire"), "Hiring cost ($ per worker):", value = 500, min = 0),
      numericInput(ns("c_fire"), "Firing cost ($ per worker):", value = 1000, min = 0),
      numericInput(ns("c_hold"), "Holding cost ($ per unit per period):", value = 80, min = 0),
      numericInput(ns("c_reg"), "Regular time cost ($ per unit):", value = 0, min = 0),
      numericInput(ns("c_ot"), "Overtime cost ($ per unit):", value = 1500, min = 0),
      numericInput(ns("c_sub"), "Subcontracting cost ($ per unit):", value = 2000, min = 0),
      numericInput(ns("c_idle"), "Idle time cost ($ per unit of unused capacity):", value = 0, min = 0),
      helpText("Regular time cost is charged on the full regular capacity of the workforce",
               "(K x working days x workers), because workers are paid even when idle."),

      hr(),
      h4("3. Plant data"),
      numericInput(ns("w0"), "Starting workforce (workers):", value = 300, min = 0),
      numericInput(ns("i0"), "Starting inventory (units):", value = 500, min = 0),
      numericInput(ns("iend"), "Ending inventory (units):", value = 600, min = 0),
      numericInput(ns("ot_pct"), "Overtime limit (% of regular capacity):", value = 20, min = 0),
      numericInput(ns("sub_cap"), "Subcontracting limit (units per period):", value = 500, min = 0),

      hr(),
      h4("4. Productivity (K)"),
      radioButtons(ns("k_mode"), "K (units per worker per day):",
                   choices = c("Enter directly" = "direct", "Compute from past data" = "compute")),
      conditionalPanel(
        condition = "input.k_mode == 'direct'", ns = ns,
        numericInput(ns("k_direct"), "K:", value = 0.14653, min = 0, step = 0.001)
      ),
      conditionalPanel(
        condition = "input.k_mode == 'compute'", ns = ns,
        numericInput(ns("past_units"), "Units produced:", value = 22850, min = 0),
        numericInput(ns("past_workers"), "Number of workers:", value = 76, min = 0),
        numericInput(ns("past_days"), "Working days:", value = 2052, min = 0),
        helpText("K = units produced / (workers x working days)")
      )
    ),
    mainPanel(
      width = 8,
      uiOutput(ns("status")),
      h4("Planning data"),
      tableOutput(ns("data_table")),
      h4("Productivity and plant summary"),
      tableOutput(ns("summary")),
      plotOutput(ns("demand_plot"), height = "320px")
    )
  )
}

# Reads a CSV / Excel upload (same rules as the Forecasting > Data page)
ap_read_file <- function(path, name, header, sep, dec, sheet) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "txt")) {
    read.csv(path, header = header, sep = sep, dec = dec, check.names = FALSE,
             stringsAsFactors = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet,
                                     col_names = header))
  } else {
    validate("Unsupported file type. Please upload a .csv, .xlsx or .xls file.")
  }
}

ap_is_numeric_col <- function(col) sum(!is.na(suppressWarnings(as.numeric(col)))) >= 1

ap_split_list <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text)) return(character(0))
  parts <- trimws(strsplit(text, "[,;\n]")[[1]])
  parts[nzchar(parts)]
}

ap_num <- function(x, default = 0) if (is.null(x) || length(x) == 0 || is.na(x)) default else x

ap_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    file_ext <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name))
    })
    output$is_csv <- reactive(!is.null(input$file) && file_ext() %in% c("csv", "txt"))
    outputOptions(output, "is_csv", suspendWhenHidden = FALSE)

    output$sheet_ui <- renderUI({
      req(file_ext() %in% c("xlsx", "xls"))
      selectInput(session$ns("sheet"), "Excel sheet:",
                  choices = readxl::excel_sheets(input$file$datapath))
    })

    raw_data <- reactive({
      if (is.null(input$file)) return(ap_demo_data())
      ap_read_file(input$file$datapath, input$file$name, input$header,
                   ap_num(input$sep, ","), ap_num(input$dec, "."), input$sheet)
    })

    # Guess the columns after each new file
    observeEvent(raw_data(), {
      df <- raw_data()
      cols <- names(df)
      num <- cols[vapply(df, ap_is_numeric_col, logical(1))]
      text <- setdiff(cols, num)
      low <- tolower(cols)
      pick <- function(pattern, pool, fallback) {
        hit <- cols[grepl(pattern, low) & cols %in% pool]
        if (length(hit) > 0) hit[1] else fallback
      }
      period_sel <- pick("period|month|week|name", cols, if (length(text) > 0) text[1] else "(none)")
      demand_sel <- pick("demand|forecast|talep", num, if (length(num) > 0) num[1] else NULL)
      days_sel <- pick("day", num, "(none)")
      updateSelectInput(session, "col_period", choices = c("(none)", cols), selected = period_sel)
      updateSelectInput(session, "col_demand", choices = num, selected = demand_sel)
      updateSelectInput(session, "col_days", choices = c("(none)", num), selected = days_sel)
    }, priority = 10)

    # Period names read from the file (before the user edits them)
    file_periods <- reactive({
      df <- raw_data()
      dcol <- input$col_demand
      req(dcol, dcol %in% names(df))
      keep <- !is.na(suppressWarnings(as.numeric(df[[dcol]])))
      pcol <- input$col_period
      if (!is.null(pcol) && pcol %in% names(df)) {
        as.character(df[[pcol]][keep])
      } else {
        paste("Period", seq_len(sum(keep)))
      }
    })

    observeEvent(file_periods(), {
      updateTextAreaInput(session, "period_names", value = paste(file_periods(), collapse = ", "))
    })

    K <- reactive({
      if (input$k_mode == "direct") {
        validate(need(ap_num(input$k_direct) > 0, "K must be greater than zero."))
        return(input$k_direct)
      }
      denom <- ap_num(input$past_workers) * ap_num(input$past_days)
      validate(need(denom > 0 && ap_num(input$past_units) > 0,
                    "Enter positive past production, workers and working days to compute K."))
      input$past_units / denom
    })

    setup <- reactive({
      df <- raw_data()
      dcol <- input$col_demand
      validate(need(!is.null(dcol) && dcol %in% names(df), "Select the demand forecast column."))
      demand <- suppressWarnings(as.numeric(df[[dcol]]))
      keep <- !is.na(demand)
      demand <- demand[keep]
      validate(need(length(demand) >= 2, "The plan needs at least 2 periods of demand."))
      validate(need(all(demand >= 0), "Demand forecasts cannot be negative."))

      dayscol <- input$col_days
      if (!is.null(dayscol) && dayscol %in% names(df)) {
        days <- suppressWarnings(as.numeric(df[[dayscol]]))[keep]
      } else {
        days <- rep(ap_num(input$const_days, 21), length(demand))
      }
      validate(need(all(!is.na(days) & days > 0), "Working days must be positive numbers in every period."))

      periods <- ap_split_list(input$period_names)
      names_note <- NULL
      if (length(periods) != length(demand)) {
        if (length(periods) > 0) {
          names_note <- sprintf("%d period names were entered for %d periods; file names are used instead.",
                                length(periods), length(demand))
        }
        periods <- file_periods()
        if (length(periods) != length(demand)) periods <- paste("Period", seq_along(demand))
      }
      periods <- make.unique(periods, sep = " ")

      list(
        periods = periods, D = demand, n = days, K = K(),
        T = length(demand),
        W0 = ap_num(input$w0), I0 = ap_num(input$i0), Iend = ap_num(input$iend),
        ot_pct = ap_num(input$ot_pct), sub_cap = ap_num(input$sub_cap),
        cost = list(hire = ap_num(input$c_hire), fire = ap_num(input$c_fire),
                    hold = ap_num(input$c_hold), reg = ap_num(input$c_reg),
                    ot = ap_num(input$c_ot), sub = ap_num(input$c_sub),
                    idle = ap_num(input$c_idle)),
        names_note = names_note,
        source = if (is.null(input$file)) "Densepack example data (no file uploaded)" else input$file$name
      )
    })

    output$status <- renderUI({
      s <- setup()
      tagList(
        p(strong("Data source: "), s$source),
        if (!is.null(s$names_note)) div(class = "alert alert-warning", s$names_note)
      )
    })

    output$data_table <- renderTable({
      s <- setup()
      data.frame(
        Period = s$periods,
        `Demand forecast` = s$D,
        `Working days` = s$n,
        `Units per worker (K x days)` = s$K * s$n,
        `Net demand` = ap_net_demand(s),
        `Cumulative net demand` = cumsum(ap_net_demand(s)),
        check.names = FALSE
      )
    }, digits = 3)

    output$summary <- renderTable({
      s <- setup()
      data.frame(
        Item = c("K (units per worker per day)", "Number of periods", "Total demand",
                 "Starting workforce", "Starting inventory", "Ending inventory"),
        Value = c(sprintf("%.5f", s$K), s$T, sum(s$D), s$W0, s$I0, s$Iend)
      )
    })

    output$demand_plot <- renderPlot({
      s <- setup()
      barplot(s$D, names.arg = s$periods, col = "steelblue", las = 2,
              ylab = "Units", main = "Demand forecast by period")
    })

    setup
  })
}


# ---------------------------------------------------------------------------
# Shared calculations
# ---------------------------------------------------------------------------

# Net demand: starting inventory is subtracted in the first period and the
# required ending inventory is added in the last period
ap_net_demand <- function(s) {
  net <- s$D
  net[1] <- net[1] - s$I0
  net[s$T] <- net[s$T] + s$Iend
  net
}

# Units one worker makes in each period
ap_units_per_worker <- function(s) s$K * s$n

# Builds a full plan from the workforce of each period and the optional
# overtime (O), idle time (U) and subcontracted (S) units, then prices it.
ap_build_plan <- function(s, W, O = 0, U = 0, S = 0, name = "") {
  T <- s$T
  W <- pmax(as.numeric(W), 0)
  O <- rep_len(as.numeric(O), T)
  U <- rep_len(as.numeric(U), T)
  S <- rep_len(as.numeric(S), T)
  W_prev <- c(s$W0, W[-T])
  H <- pmax(W - W_prev, 0)
  F <- pmax(W_prev - W, 0)
  reg_cap <- ap_units_per_worker(s) * W
  P <- reg_cap - U + O
  I <- s$I0 + cumsum(P + S - s$D)

  tol <- 1e-6 * max(1, s$D)
  cost <- c(
    Hiring = s$cost$hire * sum(H),
    Firing = s$cost$fire * sum(F),
    `Regular time` = s$cost$reg * sum(reg_cap),
    Overtime = s$cost$ot * sum(O),
    `Idle time` = s$cost$idle * sum(U),
    Subcontracting = s$cost$sub * sum(S),
    Holding = s$cost$hold * sum(pmax(I, 0))
  )
  cost <- c(cost, Total = sum(cost))

  warnings <- character(0)
  short <- which(I < -tol)
  if (length(short) > 0) {
    warnings <- c(warnings, sprintf(
      "Shortage: demand is not met in %s (largest shortage %.1f units). Shortages are not allowed, so this plan is not feasible; no cost is charged for the missing units.",
      paste(s$periods[short], collapse = ", "), max(-I[short])))
  }
  if (I[T] < s$Iend - tol) {
    warnings <- c(warnings, sprintf("Ending inventory %.1f is below the required %.1f units.", I[T], s$Iend))
  }

  list(name = name, W = W, H = H, F = F, reg_cap = reg_cap, O = O, U = U, S = S,
       P = P, I = I, cost = cost, warnings = warnings,
       feasible = length(short) == 0 && I[T] >= s$Iend - tol)
}

# Whole-worker version of a workforce vector
ap_round_up <- function(W) ceiling(W - 1e-9)


# ---------------------------------------------------------------------------
# Shared output: plan tables, cumulative plot, cost tables
# plans() must return list(a = plan, b = plan, labels = c(label_a, label_b), note = NULL)
# ---------------------------------------------------------------------------

ap_results_ui <- function(ns, extra_top = NULL, extra_bottom = NULL) {
  tagList(
    extra_top,
    uiOutput(ns("warnings")),
    uiOutput(ns("title_a")),
    div(style = "overflow-x: auto;", tableOutput(ns("plan_a"))),
    uiOutput(ns("title_b")),
    div(style = "overflow-x: auto;", tableOutput(ns("plan_b"))),
    h4("Cost of each item (both plans side by side)"),
    tableOutput(ns("costs")),
    h4("Cumulative production vs. cumulative net demand"),
    plotOutput(ns("cum_plot"), height = "380px"),
    extra_bottom
  )
}

ap_plan_table <- function(s, p, show) {
  tab <- data.frame(
    Period = s$periods,
    Demand = s$D,
    Workers = p$W,
    Hired = p$H,
    Fired = p$F,
    Production = p$P,
    check.names = FALSE
  )
  if (show$O) tab$Overtime <- p$O
  if (show$U) tab$`Idle time` <- p$U
  if (show$S) tab$Subcontract <- p$S
  tab$Inventory <- p$I
  total <- tab[1, ]
  total[] <- NA
  total$Period <- "Total"
  for (col in setdiff(names(tab), c("Period", "Workers", "Inventory"))) total[[col]] <- sum(tab[[col]])
  rbind(tab, total)
}

ap_render_plan <- function(output, setup, plans) {

  used <- reactive({
    r <- plans()
    any_pos <- function(f) any(abs(r$a[[f]]) > 1e-9) || any(abs(r$b[[f]]) > 1e-9)
    list(O = any_pos("O"), U = any_pos("U"), S = any_pos("S"))
  })

  output$warnings <- renderUI({
    r <- plans()
    items <- list()
    if (!is.null(r$note)) items <- c(items, list(div(class = "alert alert-info", r$note)))
    for (k in 1:2) {
      for (w in r[[c("a", "b")[k]]]$warnings) {
        items <- c(items, list(div(class = "alert alert-warning", strong(paste0(r$labels[k], ": ")), w)))
      }
    }
    tagList(items)
  })

  output$title_a <- renderUI(h4(plans()$labels[1]))
  output$title_b <- renderUI(h4(plans()$labels[2]))
  output$plan_a <- renderTable(ap_plan_table(setup(), plans()$a, used()), digits = 2, na = "")
  output$plan_b <- renderTable(ap_plan_table(setup(), plans()$b, used()), digits = 2, na = "")

  output$costs <- renderTable({
    r <- plans()
    a <- r$a$cost
    b <- r$b$cost
    tab <- data.frame(Item = names(a), A = unname(a), B = unname(b), Difference = unname(b - a),
                      check.names = FALSE)
    names(tab)[2:3] <- paste(r$labels, "($)")
    tab
  }, digits = 2)

  output$cum_plot <- renderPlot({
    s <- setup()
    r <- plans()
    x <- seq_len(s$T)
    net <- cumsum(ap_net_demand(s))
    pa <- cumsum(r$a$P + r$a$S)
    pb <- cumsum(r$b$P + r$b$S)
    plot(x, net, type = "o", pch = 16, lwd = 2, col = "black", xaxt = "n",
         ylim = range(0, net, pa, pb), xlab = "Period", ylab = "Cumulative units",
         main = "Cumulative production vs. cumulative net demand")
    axis(1, at = x, labels = s$periods)
    lines(x, pa, type = "o", pch = 1, lwd = 2, col = "red")
    lines(x, pb, type = "o", pch = 17, lwd = 2, col = "blue", lty = 2)
    legend("topleft", bty = "n", lwd = 2, pch = c(16, 1, 17), lty = c(1, 1, 2),
           col = c("black", "red", "blue"),
           legend = c("Cumulative net demand",
                      paste("Cumulative production -", r$labels[1]),
                      paste("Cumulative production -", r$labels[2])))
    grid()
  })
}
