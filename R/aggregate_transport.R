# Aggregate planning with the transportation method.
# Sources: starting inventory and the regular time, overtime and subcontracting
# capacity of each period. Destinations: the demand of each period, the required
# ending inventory and a dummy column for unused capacity.
# The workforce is fixed, so regular time capacity is known in advance.


# ---------------------------------------------------------------------------
# Calculations
# ---------------------------------------------------------------------------

# Builds the transportation tableau: supplies, demands and unit costs
# (NA = not allowed, because demand cannot be met from a later period)
ap_tr_tableau <- function(s, W, use_ot, use_sub) {
  T <- s$T
  upw <- ap_units_per_worker(s)
  h <- s$cost$hold
  rows <- list()
  add_row <- function(label, type, period, supply, unit_cost) {
    rows[[length(rows) + 1]] <<- list(label = label, type = type, period = period,
                                      supply = supply, unit_cost = unit_cost)
  }
  if (s$I0 > 0) add_row("Starting inventory", "I0", 0, s$I0, 0)
  for (t in seq_len(T)) {
    add_row(paste("Regular time -", s$periods[t]), "RT", t, upw[t] * W, s$cost$reg)
    if (use_ot) add_row(paste("Overtime -", s$periods[t]), "OT", t, s$ot_pct / 100 * upw[t] * W, s$cost$ot)
    if (use_sub) add_row(paste("Subcontract -", s$periods[t]), "SC", t, s$sub_cap, s$cost$sub)
  }

  has_end <- s$Iend > 0
  col_labels <- c(s$periods, if (has_end) "Ending inventory", "Unused capacity")
  demand <- c(s$D, if (has_end) s$Iend)
  supply <- sapply(rows, `[[`, "supply")
  excess <- sum(supply) - sum(demand)
  demand <- c(demand, max(excess, 0))

  nr <- length(rows)
  nc <- length(col_labels)
  cost <- matrix(NA_real_, nr, nc)
  for (i in seq_len(nr)) {
    r <- rows[[i]]
    for (t in seq_len(T)) {
      if (r$type == "I0") {
        cost[i, t] <- h * (t - 1)
      } else if (t >= r$period) {
        cost[i, t] <- r$unit_cost + h * (t - r$period)
      }
    }
    start <- if (r$type == "I0") 1 else r$period
    if (has_end) cost[i, T + 1] <- r$unit_cost + h * (T - start + 1)
    # Unused capacity: idle regular time is still paid; unused starting
    # inventory stays in stock until the end of the horizon
    cost[i, nc] <- switch(r$type,
                          I0 = h * T,
                          RT = s$cost$reg + s$cost$idle,
                          0)
  }
  dimnames(cost) <- list(sapply(rows, `[[`, "label"), col_labels)
  list(rows = rows, supply = supply, demand = demand, cost = cost, excess = excess,
       has_end = has_end)
}

# North-west corner starting solution
ap_tr_northwest <- function(tb) {
  sup <- tb$supply
  dem <- tb$demand
  x <- matrix(0, length(sup), length(dem), dimnames = dimnames(tb$cost))
  i <- 1
  j <- 1
  bad <- FALSE
  tol <- 1e-9 * max(1, sup, dem)
  while (i <= length(sup) && j <= length(dem)) {
    q <- min(sup[i], dem[j])
    if (q > tol && is.na(tb$cost[i, j])) bad <- TRUE
    x[i, j] <- q
    sup[i] <- sup[i] - q
    dem[j] <- dem[j] - q
    if (sup[i] <= tol) i <- i + 1 else j <- j + 1
  }
  list(x = x, uses_forbidden = bad)
}

# Cheapest cell first: repeatedly fill the cheapest allowed cell of the real
# demand columns; capacity left over at the end goes to the unused-capacity column
ap_tr_cheapest <- function(tb) {
  sup <- tb$supply
  dem <- tb$demand
  x <- matrix(0, length(sup), length(dem), dimnames = dimnames(tb$cost))
  tol <- 1e-9 * max(1, sup, dem)
  dummy <- length(dem)
  cells <- which(!is.na(tb$cost), arr.ind = TRUE)
  cells <- cells[order(cells[, 2] == dummy, tb$cost[cells], cells[, 1], cells[, 2]), , drop = FALSE]
  for (k in seq_len(nrow(cells))) {
    i <- cells[k, 1]
    j <- cells[k, 2]
    q <- min(sup[i], dem[j])
    if (q > tol) {
      x[i, j] <- q
      sup[i] <- sup[i] - q
      dem[j] <- dem[j] - q
    }
  }
  list(x = x, uses_forbidden = any(dem > tol))
}

# Exact optimum of the same tableau with the simplex method (optimality check)
ap_tr_optimal <- function(tb) {
  cells <- which(!is.na(tb$cost), arr.ind = TRUE)
  nr <- length(tb$supply)
  nc <- length(tb$demand)
  A <- matrix(0, nr + nc, nrow(cells))
  for (k in seq_len(nrow(cells))) {
    A[cells[k, 1], k] <- 1
    A[nr + cells[k, 2], k] <- 1
  }
  sol <- ap_simplex(tb$cost[cells], A, c(tb$supply, tb$demand), rep("=", nr + nc))
  if (sol$status != "optimal") return(NULL)
  x <- matrix(0, nr, nc, dimnames = dimnames(tb$cost))
  x[cells] <- sol$x
  x
}

ap_tr_cost <- function(tb, x) sum(x * tb$cost, na.rm = TRUE)

# Turns a tableau solution into a plan (production, idle time, overtime, subcontracting)
ap_tr_plan <- function(s, tb, x, W, name) {
  T <- s$T
  dummy <- ncol(x)
  O <- U <- S <- numeric(T)
  for (i in seq_along(tb$rows)) {
    r <- tb$rows[[i]]
    if (r$type == "I0") next
    used <- sum(x[i, -dummy])
    if (r$type == "RT") U[r$period] <- x[i, dummy]
    if (r$type == "OT") O[r$period] <- used
    if (r$type == "SC") S[r$period] <- used
  }
  ap_build_plan(s, rep(W, T), O, U, S, name = name)
}

ap_tr_format <- function(tb, x) {
  out <- matrix("", nrow(x), ncol(x), dimnames = dimnames(x))
  out[] <- ifelse(is.na(tb$cost), "x", ifelse(x > 1e-9, formatC(x, format = "f", digits = 1), ""))
  out <- cbind(out, Supply = formatC(tb$supply, format = "f", digits = 1))
  out <- rbind(out, Demand = c(formatC(tb$demand, format = "f", digits = 1), ""))
  data.frame(Source = rownames(out), out, check.names = FALSE, row.names = NULL)
}


# ---------------------------------------------------------------------------
# Page
# ---------------------------------------------------------------------------

ap_tr_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Transportation Method"),
      helpText("Each period's regular time, overtime and subcontracting capacity is a source;",
               "each period's demand is a destination. Unit cost = production cost + holding cost",
               "for every period the unit is stored. Cells marked x are not allowed",
               "(demand cannot be met by later production)."),
      numericInput(ns("W"), "Workforce (fixed in every period):", value = 300, min = 0),
      helpText("A fractional workforce is rounded up to whole workers."),
      checkboxInput(ns("use_ot"), "Use overtime as a source", TRUE),
      checkboxInput(ns("use_sub"), "Use subcontracting as a source", TRUE),
      helpText("Overtime and subcontracting limits and all costs are set on the Data & Costs page.")
    ),
    mainPanel(
      width = 9,
      ap_results_ui(ns, extra_bottom = tagList(
        h4("Unit costs ($ per unit)"),
        div(style = "overflow-x: auto;", tableOutput(ns("cost_tab"))),
        h4("North-west corner solution"),
        div(style = "overflow-x: auto;", tableOutput(ns("nw_tab"))),
        h4("Cheapest cell first solution"),
        div(style = "overflow-x: auto;", tableOutput(ns("ch_tab")))
      ))
    )
  )
}

ap_tr_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    observeEvent(setup()$W0, {
      updateNumericInput(session, "W", value = setup()$W0)
    })

    solved <- reactive({
      s <- setup()
      W_in <- ap_num(input$W)
      validate(need(W_in >= 0, "Enter a non-negative workforce."))
      W <- ap_round_up(W_in)
      tb <- ap_tr_tableau(s, W, isTRUE(input$use_ot), isTRUE(input$use_sub))
      validate(need(tb$excess >= -1e-6 * max(1, sum(tb$demand)), sprintf(
        "Total capacity is %.1f units short of total demand plus ending inventory. Increase the workforce or allow overtime / subcontracting.",
        -tb$excess)))
      nw <- ap_tr_northwest(tb)
      ch <- ap_tr_cheapest(tb)
      opt <- ap_tr_optimal(tb)
      list(s = s, W_in = W_in, W = W, tb = tb, nw = nw, ch = ch, opt = opt)
    })

    plans <- reactive({
      r <- solved()
      s <- r$s
      a <- ap_tr_plan(s, r$tb, r$nw$x, r$W, "North-west corner")
      b <- ap_tr_plan(s, r$tb, r$ch$x, r$W, "Cheapest cell first")
      hire_fire <- a$cost[["Hiring"]] + a$cost[["Firing"]]
      notes <- sprintf("Workforce: %s workers in every period%s. Tableau cost: north-west corner $%s, cheapest cell first $%s (plus $%s hiring/firing to reach the workforce).",
                       r$W, if (r$W != r$W_in) sprintf(" (rounded up from %s)", r$W_in) else "",
                       format(round(ap_tr_cost(r$tb, r$nw$x), 2), big.mark = ","),
                       format(round(ap_tr_cost(r$tb, r$ch$x), 2), big.mark = ","),
                       format(round(hire_fire, 2), big.mark = ","))
      if (r$nw$uses_forbidden) {
        a$warnings <- c(a$warnings, "The north-west corner had to use a not-allowed cell: some demand is met by later production (shortage).")
      }
      if (r$ch$uses_forbidden) {
        b$warnings <- c(b$warnings, "Some demand could not be met without using later production: capacity in the early periods is too small.")
      }
      if (!is.null(r$opt)) {
        gap <- ap_tr_cost(r$tb, r$ch$x) - ap_tr_cost(r$tb, r$opt)
        notes <- paste(notes, if (gap <= 1e-6 * max(1, abs(ap_tr_cost(r$tb, r$opt)))) {
          "Optimality check with the simplex method: the cheapest cell first solution is optimal."
        } else {
          sprintf("Optimality check with the simplex method: the optimal tableau cost is $%s, so the cheapest cell first solution is $%s above the optimum.",
                  format(round(ap_tr_cost(r$tb, r$opt), 2), big.mark = ","), format(round(gap, 2), big.mark = ","))
        })
      }
      list(a = a, b = b, labels = c("North-west corner solution", "Cheapest cell first solution"),
           note = notes)
    })

    ap_render_plan(output, setup, plans)

    output$cost_tab <- renderTable({
      cst <- solved()$tb$cost
      out <- ifelse(is.na(cst), "x", formatC(cst, format = "f", digits = 2))
      data.frame(Source = rownames(cst), out, check.names = FALSE, row.names = NULL)
    })
    output$nw_tab <- renderTable(ap_tr_format(solved()$tb, solved()$nw$x))
    output$ch_tab <- renderTable(ap_tr_format(solved()$tb, solved()$ch$x))

    plans
  })
}
