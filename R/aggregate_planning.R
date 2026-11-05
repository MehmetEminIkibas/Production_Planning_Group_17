# This assignment (3) is made by Koray Örnek, student number: 21831457
# Also additions to app.R related with this part is also made by Koray Örnek

# R/aggregate_planning.R

# UI panel for processing aggregate execution strategies and disaggregation rules
aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Aggregate Planning & Disaggregation",
    sidebarLayout(
      sidebarPanel(
        h3("1. Operational Cost Metrics"),
        numericInput(ns("cost_hiring"), "Hiring Cost ($/worker):", value = 500, min = 0),
        numericInput(ns("cost_firing"), "Firing Cost ($/worker):", value = 800, min = 0),
        numericInput(ns("cost_holding"), "Inventory Holding Cost ($/unit/period):", value = 5, min = 0),
        numericInput(ns("cost_regular"), "Regular Production Cost ($/unit):", value = 15, min = 0),
        numericInput(ns("cost_overtime"), "Overtime Production Cost ($/unit):", value = 25, min = 0),
        numericInput(ns("cost_subcontract"), "Subcontracting Cost ($/unit):", value = 35, min = 0),
        
        numericInput(ns("init_workforce"), "Initial Workforce Level:", value = 10, min = 0),
        numericInput(ns("init_inv"), "Initial Inventory Balance:", value = 100, min = 0),
        numericInput(ns("target_end_inv"), "Target Ending Inventory:", value = 100, min = 0),
        
        hr(),
        h3("2. Productivity Constant (K)"),
        radioButtons(ns("prod_mode"), "Productivity Input:", 
                     choices = c("Direct Entry" = "direct", "Compute from Historical Metrics" = "compute")),
        conditionalPanel(condition = sprintf("input['%s'] == 'direct'", ns("prod_mode")),
                         numericInput(ns("k_direct"), "K (Units/worker-day):", value = 4, min = 0.01)),
        conditionalPanel(condition = sprintf("input['%s'] == 'compute'", ns("prod_mode")),
                         numericInput(ns("past_units"), "Historical Production Volume (Units):", value = 5000),
                         numericInput(ns("past_workers"), "Historical Workforce Size:", value = 12),
                         numericInput(ns("past_days"), "Historical Operational Days:", value = 90)),
        
        hr(),
        h3("3. Strategy Profile Selection"),
        selectInput(ns("planning_method"), "Strategy Heuristic:",
                    choices = c("Chase Strategy (Zero-Inventory Model)" = "chase",
                                "Level Strategy (Constant Workforce Model)" = "level",
                                "Mixed Strategy Framework" = "mixed")),
        conditionalPanel(condition = sprintf("input['%s'] == 'mixed'", ns("planning_method")),
                         textInput(ns("mixed_workforce_changes"), "Workforce Allocation Target Sequence:", value = "10, 12, 15, 12, 10, 8")),
        
        hr(),
        h3("4. Plan Disaggregation Parameters"),
        p("Apportion finalized production plan metrics among product families using the square-root rule."),
        textInput(ns("family_names"), "Product Family Identifiers (Comma Separated):", value = "Family A, Family B, Family C"),
        textInput(ns("family_historical_demands"), "Historical Baseline Volumes:", value = "4000, 2500, 1500")
      ),
      
      mainPanel(
        h2("Aggregate Operational Planning System"),
        h4("Strategy Comparison Matrix: Baseline Mathematical Model vs. Whole-Worker Realization"),
        tableOutput(ns("plan_table")),
        hr(),
        plotOutput(ns("cumulative_plot")),
        hr(),
        h3("Financial Evaluation Ledgers"),
        tableOutput(ns("cost_table")),
        hr(),
        h2("Downstream Plan Disaggregation"),
        h4("Master Production Schedule (MPS) Allocation via Square-Root Heuristic"),
        tableOutput(ns("disagg_table"))
      )
    )
  )
}

# Backend computation script linked directly to shared infrastructure parameters
aggregate_planning_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Safely transform the shared team dataset into structured vectors
    plan_data <- reactive({
      # Verify a shared series column choice has been initialized on the data page
      validate(need(!is.null(shared_data()), "Please upload a data file and select a target series under the Forecasting > Data tab first."))
      
      raw_y <- shared_data()$y
      N <- length(raw_y)
      
      # Generate uniform time period markers and baseline historical steps variables
      data.frame(
        Period = paste("Period", 1:N),
        Demand = raw_y,
        WorkingDays = rep(21, N), # Baseline engineering parameter default approximation
        stringsAsFactors = FALSE
      )
    })
    
    # Calculate productivity multiplier K
    reactive_k <- reactive({
      if (input$prod_mode == "direct") return(input$k_direct)
      denom <- input$past_workers * input$past_days
      if (denom <= 0) return(1)
      return(input$past_units / denom)
    })
    
    # Main strategy engine calculating exact paths vs rounded resource parameters
    calculated_results <- reactive({
      df <- plan_data()
      N <- nrow(df)
      K <- reactive_k()
      
      w_exact <- numeric(N); h_exact <- numeric(N); f_exact <- numeric(N); p_exact <- numeric(N); i_exact <- numeric(N)
      w_round <- numeric(N); h_round <- numeric(N); f_round <- numeric(N); p_round <- numeric(N); i_round <- numeric(N)
      
      prev_w_e <- input$init_workforce; prev_i_e <- input$init_inv
      prev_w_r <- input$init_workforce; prev_i_r <- input$init_inv
      
      method <- input$planning_method
      
      # Chase Model Formulations
      if (method == "chase") {
        for (i in 1:N) {
          net_e <- df$Demand[i] - prev_i_e + (if(i == N) input$target_end_inv else 0)
          val_w_e <- max(0, net_e / (K * df$WorkingDays[i]))
          w_exact[i] <- val_w_e
          diff_e <- w_exact[i] - prev_w_e
          if(diff_e > 0) { h_exact[i] <- diff_e } else { f_exact[i] <- abs(diff_e) }
          p_exact[i] <- w_exact[i] * K * df$WorkingDays[i]
          i_exact[i] <- prev_i_e + p_exact[i] - df$Demand[i]
          
          # Whole number rounding conditions tracking block
          w_round[i] <- ceiling(val_w_e)
          diff_r <- w_round[i] - prev_w_r
          if(diff_r > 0) { h_round[i] <- diff_r } else { f_round[i] <- abs(diff_r) }
          p_round[i] <- w_round[i] * K * df$WorkingDays[i]
          i_round[i] <- prev_i_r + p_round[i] - df$Demand[i]
          
          prev_w_e <- w_exact[i]; prev_i_e <- i_exact[i]
          prev_w_r <- w_round[i]; prev_i_r <- i_round[i]
        }
      } 
      # Level Model Formulations
      else if (method == "level") {
        tot_d <- sum(df$Demand) + input$target_end_inv - input$init_inv
        tot_days <- sum(df$WorkingDays)
        avg_w <- tot_d / (K * tot_days)
        
        for(i in 1:N) {
          w_exact[i] <- avg_w
          if(i == 1) { diff_e <- avg_w - input$init_workforce; if(diff_e > 0) h_exact[i] <- diff_e else f_exact[i] <- abs(diff_e) }
          p_exact[i] <- w_exact[i] * K * df$WorkingDays[i]
          i_exact[i] <- prev_i_e + p_exact[i] - df$Demand[i]
          prev_i_e <- i_exact[i]
          
          w_round[i] <- ceiling(avg_w)
          if(i == 1) { diff_r <- w_round[i] - input$init_workforce; if(diff_r > 0) h_round[i] <- diff_r else f_round[i] <- abs(diff_r) }
          p_round[i] <- w_round[i] * K * df$WorkingDays[i]
          i_round[i] <- prev_i_r + p_round[i] - df$Demand[i]
          prev_i_r <- i_round[i]
        }
      }
      # Mixed Allocation Logic
      else {
        custom_w <- as.numeric(strsplit(input$mixed_workforce_changes, ",")[])
        if(length(custom_w) < N) custom_w <- c(custom_w, rep(tail(custom_w, 1), N - length(custom_w)))
        for(i in 1:N) {
          w_exact[i] <- custom_w[i]
          diff_e <- w_exact[i] - prev_w_e
          if(diff_e > 0) h_exact[i] <- diff_e else f_exact[i] <- abs(diff_e)
          p_exact[i] <- w_exact[i] * K * df$WorkingDays[i]
          i_exact[i] <- prev_i_e + p_exact[i] - df$Demand[i]
          prev_w_e <- w_exact[i]; prev_i_e <- i_exact[i]
          
          w_round[i] <- ceiling(custom_w[i])
          diff_r <- w_round[i] - prev_w_r
          if(diff_r > 0) h_round[i] <- diff_r else f_round[i] <- abs(diff_r)
          p_round[i] * K * df$WorkingDays[i]
          p_round[i] <- w_round[i] * K * df$WorkingDays[i]
          i_round[i] <- prev_i_r + p_round[i] - df$Demand[i]
          prev_w_r <- w_round[i]; prev_i_r <- i_round[i]
        }
      }
      
      data.frame(
        Period = df$Period,
        Demand = df$Demand,
        `W (Exact)` = round(w_exact, 2),
        `W (Round Up)` = w_round,
        `Prod (Exact)` = round(p_exact, 1),
        `Prod (Round Up)` = round(p_round, 1),
        `Inv (Exact)` = round(i_exact, 1),
        `Inv (Round Up)` = round(i_round, 1),
        check.names = FALSE
      )
    })
    
    # Financial summation comparing exact theoretical strategies to rounded physical reality
    output$cost_table <- renderTable({
      res <- calculated_results()
      df <- plan_data()
      N <- nrow(df)
      
      compute_total_ledger <- function(w_vec, p_vec, i_vec) {
        h_v <- numeric(N); f_v <- numeric(N)
        pw <- input$init_workforce
        for(j in 1:N) {
          diff <- w_vec[j] - pw
          if(diff > 0) h_v[j] <- diff else f_v[j] <- abs(diff)
          pw <- w_vec[j]
        }
        total <- sum(h_v) * input$cost_hiring + sum(f_v) * input$cost_firing + 
                  sum(p_vec) * input$cost_regular + sum(i_vec) * input$cost_holding
        return(total)
      }
      
      cost_exact <- compute_total_ledger(res$`W (Exact)`, res$`Prod (Exact)`, res$`Inv (Exact)`)
      cost_round <- compute_total_ledger(res$`W (Round Up)`, res$`Prod (Round Up)`, res$`Inv (Round Up)`)
      
      data.frame(
        `Plan Variant Strategy` = c("Baseline Formula Allocation (Fractional Workers)", "Realized Plant Action Plan (Whole-Worker Ceilings)"),
        `Calculated Budget Cost ($)` = c(cost_exact, cost_round),
        check.names = FALSE
      )
    })
        # Downstream disaggregation processor using square-root parameters ratio splitters
output$disagg_table <- renderTable({
res <- calculated_results()
f_names <- strsplit(input$family_names, "[,; ]+")[[1]]
f_demands <- as.numeric(strsplit(input$family_historical_demands, "[,; ]+")[[1]])
if(length(f_names) != length(f_demands) || length(f_names) == 0) {
return(data.frame(Error = "Check length alignment configuration metrics inside parameter variables."))
}
sq_demands <- sqrt(f_demands)
allocation_weights <- sq_demands / sum(sq_demands)
disagg_list <- list()
for(k in 1:length(f_names)) {
family_row <- c(f_names[k])
for(p_idx in 1:nrow(res)) {
total_prod <- res$Prod (Round Up)[p_idx]
allocated_qty <- total_prod * allocation_weights[k]
family_row <- c(family_row, round(allocated_qty, 1))
}
disagg_list[[k]] <- family_row
}
disagg_matrix <- do.call(rbind, disagg_list)
disagg_df <- as.data.frame(disagg_matrix, stringsAsFactors = FALSE)
colnames(disagg_df) <- c("Product Family", res$Period)
return(disagg_df)
})
output$plan_table <- renderTable({ calculated_results() })
output$cumulative_plot <- renderPlot({
res <- calculated_results()
plot(1:nrow(res), cumsum(res$Demand), type="b", col="black", pch=16,
xlab="Operational Timeline Periods", ylab="Inventory Material Units", main="Cumulative Production Trajectory Tracking Strategy")
lines(1:nrow(res), cumsum(res$Prod (Round Up)), type="b", col="red", pch=17)
legend("topleft", legend=c("Cumulative Baseline Demand Volume", "Cumulative Production Volume (Realized)"),
col=c("black","red"), lty=1, pch=c(16,17))
})
})
}
