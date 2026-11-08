# Disaggregation of the aggregate plan among product families (square-root rule).
# Based on the first version by Koray Örnek (21831457).
# Each family receives a share of every period's production proportional to the
# square root of its historical volume: share(j) = sqrt(d_j) / sum_k sqrt(d_k).

ap_disagg_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Disaggregation (Square-Root Rule)"),
      helpText("Splits the production of an aggregate plan among product families.",
               "Share of family j = sqrt(volume j) / sum of sqrt(volumes)."),
      selectInput(ns("plan"), "Aggregate plan to split:",
                  choices = c("Zero-inventory (chase) plan" = "chase",
                              "Constant workforce (level) plan" = "level",
                              "Mixed plan" = "mixed",
                              "Linear programming" = "lp",
                              "Transportation method" = "tr")),
      radioButtons(ns("version"), "Version:",
                   choices = c("Original / first solution" = "a", "Whole-worker / second solution" = "b"),
                   selected = "b"),
      textInput(ns("names"), "Product family names (comma separated):",
                value = "Family A, Family B, Family C"),
      textInput(ns("volumes"), "Historical volume of each family (comma separated):",
                value = "4000, 2500, 1500")
    ),
    mainPanel(
      width = 9,
      uiOutput(ns("info")),
      h4("Family shares"),
      tableOutput(ns("shares")),
      h4("Production of each family by period"),
      tableOutput(ns("table")),
      plotOutput(ns("plot"), height = "350px")
    )
  )
}

ap_disagg_server <- function(id, setup, plans) {
  moduleServer(id, function(input, output, session) {

    families <- reactive({
      nm <- ap_split_list(input$names)
      vol <- suppressWarnings(as.numeric(ap_split_list(input$volumes)))
      validate(need(length(nm) > 0, "Enter at least one product family name."))
      validate(need(length(nm) == length(vol),
                    sprintf("%d family names but %d volumes were entered; enter one volume per family.",
                            length(nm), length(vol))))
      validate(need(all(!is.na(vol) & vol >= 0) && sum(vol) > 0,
                    "Volumes must be non-negative numbers and at least one must be positive."))
      share <- sqrt(vol) / sum(sqrt(vol))
      list(names = make.unique(nm, sep = " "), volume = vol, share = share)
    })

    chosen <- reactive({
      r <- plans[[input$plan]]()
      k <- if (input$version == "a") 1 else 2
      p <- r[[c("a", "b")[k]]]
      list(plan = p, label = r$labels[k], production = p$P + p$S)
    })

    output$info <- renderUI({
      ch <- chosen()
      p(strong("Splitting: "), ch$label, sprintf(" (total production %.1f units)", sum(ch$production)))
    })

    output$shares <- renderTable({
      f <- families()
      data.frame(Family = f$names, `Historical volume` = f$volume,
                 `Square root` = sqrt(f$volume), Share = f$share, check.names = FALSE)
    }, digits = 4)

    alloc <- reactive({
      f <- families()
      prod <- chosen()$production
      m <- outer(f$share, prod)
      dimnames(m) <- list(f$names, setup()$periods)
      m
    })

    output$table <- renderTable({
      m <- alloc()
      out <- rbind(m, Total = colSums(m))
      out <- cbind(out, Total = rowSums(out))
      data.frame(Family = rownames(out), out, check.names = FALSE, row.names = NULL)
    }, digits = 1)

    output$plot <- renderPlot({
      m <- alloc()
      barplot(m, beside = FALSE, col = hcl.colors(nrow(m), "Set 2"), las = 2,
              legend.text = rownames(m), args.legend = list(x = "topleft", bty = "n"),
              ylab = "Units", main = "Production by product family")
    })
  })
}
