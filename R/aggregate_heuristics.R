# Aggregate planning heuristics:
# Zero-inventory (chase) plan, constant-workforce (level) plan and mixed plan.
# Based on the first version by Koray Örnek (21831457).


# ---------------------------------------------------------------------------
# Calculations
# ---------------------------------------------------------------------------

# Chase: each period hires/fires so that production just covers net demand.
# With whole workers the workforce is rounded up and the small surplus is
# carried as inventory, which lowers the next period's requirement.
ap_chase_workforce <- function(s, whole = FALSE) {
  upw <- ap_units_per_worker(s)
  W <- numeric(s$T)
  inv <- s$I0
  for (t in seq_len(s$T)) {
    need <- s$D[t] - inv + if (t == s$T) s$Iend else 0
    w <- max(0, need / upw[t])
    if (whole) w <- ap_round_up(w)
    W[t] <- w
    inv <- inv + w * upw[t] - s$D[t]
  }
  W
}

# Smallest constant workforce for periods a..b that causes no shortage,
# given the inventory at the start of period a
ap_min_constant_workforce <- function(s, a, b, inv_start) {
  idx <- a:b
  upw <- ap_units_per_worker(s)[idx]
  need <- cumsum(s$D[idx]) - inv_start
  if (b == s$T) need[length(need)] <- need[length(need)] + s$Iend
  max(0, need / cumsum(upw))
}

# Level: one workforce for the whole horizon = max over t of
# cumulative net demand / cumulative units per worker
ap_level_workforce <- function(s, whole = FALSE) {
  w <- ap_min_constant_workforce(s, 1, s$T, s$I0)
  if (whole) w <- ap_round_up(w)
  rep(w, s$T)
}

# Mixed: the workforce changes only in the chosen periods. Each segment uses
# the smallest constant workforce that avoids shortages in that segment.
# If period 1 is not chosen, the first segment keeps the starting workforce.
ap_mixed_workforce <- function(s, change_periods, whole = FALSE) {
  starts <- sort(unique(change_periods))
  starts <- starts[starts >= 1 & starts <= s$T]
  bounds <- sort(unique(c(1, starts)))
  ends <- c(bounds[-1] - 1, s$T)
  upw <- ap_units_per_worker(s)
  W <- numeric(s$T)
  inv <- s$I0
  for (k in seq_along(bounds)) {
    a <- bounds[k]
    b <- ends[k]
    if (a == 1 && !(1 %in% starts)) {
      w <- s$W0
    } else {
      w <- ap_min_constant_workforce(s, a, b, inv)
      if (whole) w <- ap_round_up(w)
    }
    W[a:b] <- w
    inv <- inv + sum(w * upw[a:b] - s$D[a:b])
  }
  W
}


# ---------------------------------------------------------------------------
# Pages
# ---------------------------------------------------------------------------

ap_heuristic_layout <- function(ns, title, help, controls = NULL) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4(title),
      helpText(help),
      controls,
      helpText("Data, costs and productivity are set on the Data & Costs page.",
               "When the plan needs a fractional number of workers, the whole-worker plan",
               "rounds the workforce up and is shown next to the original plan.")
    ),
    mainPanel(width = 9, ap_results_ui(ns))
  )
}

ap_heuristic_plans <- function(s, W_exact, W_whole) {
  list(
    a = ap_build_plan(s, W_exact, name = "Original"),
    b = ap_build_plan(s, W_whole, name = "Whole workers"),
    labels = c("Original plan (fractional workers)", "Whole-worker plan (rounded up)")
  )
}

# Zero-inventory (chase) plan
ap_chase_ui <- function(id) {
  ns <- NS(id)
  ap_heuristic_layout(ns, "Zero-Inventory (Chase) Plan",
                      "The workforce is changed every period so that production equals net demand and no inventory is carried.")
}

ap_chase_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    plans <- reactive({
      s <- setup()
      ap_heuristic_plans(s, ap_chase_workforce(s), ap_chase_workforce(s, whole = TRUE))
    })
    ap_render_plan(output, setup, plans)
    plans
  })
}

# Constant-workforce (level) plan
ap_level_ui <- function(id) {
  ns <- NS(id)
  ap_heuristic_layout(ns, "Constant Workforce (Level) Plan",
                      "One workforce level is kept for the whole horizon: the smallest level for which cumulative production never falls below cumulative net demand. Inventory absorbs the demand changes.")
}

ap_level_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    plans <- reactive({
      s <- setup()
      ap_heuristic_plans(s, ap_level_workforce(s), ap_level_workforce(s, whole = TRUE))
    })
    ap_render_plan(output, setup, plans)
    plans
  })
}

# Mixed plan
ap_mixed_ui <- function(id) {
  ns <- NS(id)
  ap_heuristic_layout(
    ns, "Mixed Plan",
    "The workforce changes only in the periods you choose. Between two changes it stays constant at the smallest level that avoids shortages.",
    controls = tagList(
      selectInput(ns("changes"), "Periods in which the workforce changes:",
                  choices = NULL, multiple = TRUE),
      helpText("If the first period is not chosen, the starting workforce is kept until the first change.")
    )
  )
}

ap_mixed_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    observeEvent(setup()$periods, {
      s <- setup()
      choices <- setNames(seq_len(s$T), s$periods)
      default <- unique(c(1, ceiling(s$T / 2) + 1))
      default <- default[default <= s$T]
      keep <- intersect(as.integer(isolate(input$changes)), seq_len(s$T))
      updateSelectInput(session, "changes", choices = choices,
                        selected = if (length(keep) > 0) keep else default)
    })

    plans <- reactive({
      s <- setup()
      ch <- as.integer(input$changes)
      ch <- ch[!is.na(ch) & ch <= s$T]
      r <- ap_heuristic_plans(s, ap_mixed_workforce(s, ch), ap_mixed_workforce(s, ch, whole = TRUE))
      r$note <- if (length(ch) == 0) {
        "No change period is chosen, so the starting workforce is kept in every period."
      } else {
        paste("Workforce changes in:", paste(s$periods[sort(ch)], collapse = ", "))
      }
      r
    })
    ap_render_plan(output, setup, plans)
    plans
  })
}
