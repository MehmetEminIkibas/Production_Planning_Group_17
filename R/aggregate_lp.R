# Aggregate planning with linear programming.
# The LP is solved with a two-phase simplex method written in base R
# (no extra packages are needed for deployment).


# ---------------------------------------------------------------------------
# Simplex solver
# minimize obj'x  subject to  A x (<=, =, >=) b,  x >= 0
# Returns list(status = "optimal" | "infeasible" | "unbounded", x, value)
# ---------------------------------------------------------------------------

ap_simplex <- function(obj, A, b, dir, eps = 1e-9, max_iter = 20000) {
  A <- as.matrix(A)
  m <- nrow(A)
  nv <- ncol(A)
  b <- as.numeric(b)
  dir <- as.character(dir)

  # Make every right-hand side non-negative
  neg <- b < 0
  A[neg, ] <- -A[neg, ]
  b[neg] <- -b[neg]
  dir[neg & dir == "<="] <- "tmp"
  dir[neg & dir == ">="] <- "<="
  dir[dir == "tmp"] <- ">="

  # Slack (<=) and surplus (>=) columns, then artificial columns
  n_slack <- sum(dir != "=")
  S <- matrix(0, m, n_slack)
  basis <- integer(m)
  k <- 0
  for (i in seq_len(m)) {
    if (dir[i] == "<=") { k <- k + 1; S[i, k] <- 1; basis[i] <- nv + k }
    if (dir[i] == ">=") { k <- k + 1; S[i, k] <- -1 }
  }
  need_art <- which(basis == 0)
  R <- matrix(0, m, length(need_art))
  for (j in seq_along(need_art)) {
    R[need_art[j], j] <- 1
    basis[need_art[j]] <- nv + n_slack + j
  }
  tab <- cbind(A, S, R)
  rhs <- b
  n_all <- ncol(tab)
  art_cols <- if (length(need_art) > 0) (nv + n_slack + 1):n_all else integer(0)

  pivot <- function(r, c) {
    pr <- tab[r, ] / tab[r, c]
    rr <- rhs[r] / tab[r, c]
    f <- tab[, c]
    tab <<- tab - outer(f, pr)
    rhs <<- rhs - f * rr
    tab[r, ] <<- pr
    rhs[r] <<- rr
    tab[, c] <<- 0
    tab[r, c] <<- 1
    basis[r] <<- c
  }

  # Runs simplex iterations (Bland's rule, so it cannot cycle)
  run <- function(cost, allowed) {
    for (iter in seq_len(max_iter)) {
      reduced <- cost - as.numeric(cost[basis] %*% tab)
      cand <- which(allowed & reduced < -eps)
      if (length(cand) == 0) return("optimal")
      c <- cand[1]
      col <- tab[, c]
      rows <- which(col > eps)
      if (length(rows) == 0) return("unbounded")
      ratios <- rhs[rows] / col[rows]
      best <- rows[abs(ratios - min(ratios)) <= eps * max(1, abs(min(ratios)))]
      r <- best[which.min(basis[best])]
      pivot(r, c)
    }
    "iteration limit"
  }

  # Phase 1: minimize the sum of artificial variables
  if (length(art_cols) > 0) {
    cost1 <- numeric(n_all)
    cost1[art_cols] <- 1
    run(cost1, rep(TRUE, n_all))
    if (sum(rhs[basis %in% art_cols]) > 1e-7 * max(1, abs(b))) {
      return(list(status = "infeasible", x = NULL, value = NA))
    }
    # Drive artificial variables that stay in the basis (at zero) out of it
    drop_rows <- integer(0)
    for (i in which(basis %in% art_cols)) {
      cand <- which(abs(tab[i, seq_len(nv + n_slack)]) > eps)
      if (length(cand) > 0) pivot(i, cand[1]) else drop_rows <- c(drop_rows, i)
    }
    if (length(drop_rows) > 0) {
      tab <- tab[-drop_rows, , drop = FALSE]
      rhs <- rhs[-drop_rows]
      basis <- basis[-drop_rows]
      m <- nrow(tab)
    }
  }

  # Phase 2: original objective, artificial columns may not enter
  cost2 <- c(obj, numeric(n_all - nv))
  allowed <- rep(TRUE, n_all)
  allowed[art_cols] <- FALSE
  status <- run(cost2, allowed)
  if (status != "optimal") return(list(status = status, x = NULL, value = NA))

  x_all <- numeric(n_all)
  x_all[basis] <- rhs
  x <- pmax(x_all[seq_len(nv)], 0)
  list(status = "optimal", x = x, value = sum(obj * x))
}


# ---------------------------------------------------------------------------
# Aggregate planning LP
# Variables of each period t: W, H, F, I, O, U, S
#   W(t) = W(t-1) + H(t) - F(t)
#   I(t) = I(t-1) + K n(t) W(t) + O(t) - U(t) + S(t) - D(t)
#   O(t) <= overtime% x K n(t) W(t),  U(t) <= K n(t) W(t),  S(t) <= limit
#   I(T) >= ending inventory
# fixed_W: optional workforce vector (used for the whole-worker plan)
# ---------------------------------------------------------------------------

ap_lp_solve <- function(s, allow_ot, allow_idle, allow_sub, fixed_W = NULL) {
  T <- s$T
  upw <- ap_units_per_worker(s)
  vars <- c("W", "H", "F", "I", "O", "U", "S")
  nv <- length(vars) * T
  idx <- function(v, t) (match(v, vars) - 1) * T + t

  obj <- numeric(nv)
  for (t in seq_len(T)) {
    obj[idx("W", t)] <- s$cost$reg * upw[t]
    obj[idx("H", t)] <- s$cost$hire
    obj[idx("F", t)] <- s$cost$fire
    obj[idx("I", t)] <- s$cost$hold
    obj[idx("O", t)] <- s$cost$ot
    obj[idx("U", t)] <- s$cost$idle
    obj[idx("S", t)] <- s$cost$sub
  }

  rows <- list()
  add <- function(coefs, dir, rhs) {
    a <- numeric(nv)
    for (nm in names(coefs)) a[as.integer(nm)] <- a[as.integer(nm)] + coefs[[nm]]
    rows[[length(rows) + 1]] <<- list(a = a, dir = dir, rhs = rhs)
  }
  co <- function(...) {
    x <- list(...)
    v <- unlist(x[c(FALSE, TRUE)])
    names(v) <- unlist(x[c(TRUE, FALSE)])
    as.list(v)
  }

  for (t in seq_len(T)) {
    # Workforce balance
    if (t == 1) {
      add(co(idx("W", 1), 1, idx("H", 1), -1, idx("F", 1), 1), "=", s$W0)
    } else {
      add(co(idx("W", t), 1, idx("W", t - 1), -1, idx("H", t), -1, idx("F", t), 1), "=", 0)
    }
    # Inventory balance
    if (t == 1) {
      add(co(idx("I", 1), 1, idx("W", 1), -upw[1], idx("O", 1), -1, idx("U", 1), 1, idx("S", 1), -1),
          "=", s$I0 - s$D[1])
    } else {
      add(co(idx("I", t), 1, idx("I", t - 1), -1, idx("W", t), -upw[t], idx("O", t), -1,
             idx("U", t), 1, idx("S", t), -1), "=", -s$D[t])
    }
    # Overtime, idle time and subcontracting options
    if (allow_ot) {
      add(co(idx("O", t), 1, idx("W", t), -s$ot_pct / 100 * upw[t]), "<=", 0)
    } else {
      add(co(idx("O", t), 1), "=", 0)
    }
    if (allow_idle) {
      add(co(idx("U", t), 1, idx("W", t), -upw[t]), "<=", 0)
    } else {
      add(co(idx("U", t), 1), "=", 0)
    }
    if (allow_sub) {
      add(co(idx("S", t), 1), "<=", s$sub_cap)
    } else {
      add(co(idx("S", t), 1), "=", 0)
    }
    if (!is.null(fixed_W)) add(co(idx("W", t), 1), "=", fixed_W[t])
  }
  add(co(idx("I", T), 1), ">=", s$Iend)

  A <- do.call(rbind, lapply(rows, `[[`, "a"))
  sol <- ap_simplex(obj, A, sapply(rows, `[[`, "rhs"), sapply(rows, `[[`, "dir"))
  if (sol$status != "optimal") return(sol)
  get <- function(v) sol$x[idx(v, seq_len(T))]
  sol$W <- get("W")
  sol$O <- get("O")
  sol$U <- get("U")
  sol$S <- get("S")
  sol
}


# ---------------------------------------------------------------------------
# Page
# ---------------------------------------------------------------------------

ap_lp_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Linear Programming"),
      helpText("Finds the plan with the lowest total cost.",
               "Workforce, hiring, firing, inventory, overtime, idle time and subcontracting",
               "are all decision variables. Shortages are not allowed."),
      checkboxInput(ns("allow_ot"), "Allow overtime", TRUE),
      checkboxInput(ns("allow_idle"), "Allow idle time", TRUE),
      checkboxInput(ns("allow_sub"), "Allow subcontracting", TRUE),
      helpText("Overtime and subcontracting limits and all costs are set on the Data & Costs page."),
      helpText("Whole-worker plan: the LP workforce is rounded up in every period and the LP is",
               "solved again for the other variables.")
    ),
    mainPanel(
      width = 9,
      ap_results_ui(ns, extra_bottom = tagList(
        h4("LP model"),
        withMathJax(helpText(
          "$$\\min \\sum_t c_H H_t + c_F F_t + c_I I_t + c_R K n_t W_t + c_O O_t + c_U U_t + c_S S_t$$",
          "$$W_t = W_{t-1} + H_t - F_t, \\quad I_t = I_{t-1} + K n_t W_t + O_t - U_t + S_t - D_t$$",
          "$$O_t \\le o\\,K n_t W_t, \\quad U_t \\le K n_t W_t, \\quad S_t \\le s_{max}, \\quad I_T \\ge I_{end}, \\quad \\text{all variables} \\ge 0$$"
        ))
      ))
    )
  )
}

ap_lp_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    plans <- reactive({
      s <- setup()
      opts <- list(isTRUE(input$allow_ot), isTRUE(input$allow_idle), isTRUE(input$allow_sub))
      sol <- do.call(ap_lp_solve, c(list(s), opts))
      validate(need(sol$status == "optimal", paste(
        "The LP has no optimal solution:", sol$status,
        "- check the ending inventory and the overtime / subcontracting limits.")))
      exact <- ap_build_plan(s, sol$W, sol$O, sol$U, sol$S, name = "LP")

      W_whole <- ap_round_up(sol$W)
      sol2 <- do.call(ap_lp_solve, c(list(s), opts, list(fixed_W = W_whole)))
      if (sol2$status == "optimal") {
        whole <- ap_build_plan(s, W_whole, sol2$O, sol2$U, sol2$S, name = "LP whole")
        note <- NULL
      } else {
        whole <- ap_build_plan(s, W_whole, sol$O, sol$U, sol$S, name = "LP whole")
        note <- "With rounded-up workers the LP has no feasible solution; the original overtime, idle time and subcontracting values are kept."
      }
      list(a = exact, b = whole,
           labels = c("LP optimal plan (fractional workers)", "Whole-worker plan (rounded up)"),
           note = note)
    })
    ap_render_plan(output, setup, plans)
    plans
  })
}
