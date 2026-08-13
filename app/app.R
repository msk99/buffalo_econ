# Buffalo Bioeconomic Model -- Shiny app (bslib), thin UI over the engine in
# R/. Every sidebar default is resolved from the parameter workbook at the
# chosen herd size (sheet 12_App_Defaults), never hard-coded, so workbook
# edits always reach the app.
#
# Run locally:  R -e 'shiny::runApp("app", launch.browser = TRUE)'

library(shiny)
library(bslib)
library(data.table)
library(ggplot2)
library(DT)

# ---- engine ------------------------------------------------------------------
# Local dev: engine lives in ../R. A deploy bundle copies R/ + params/ flat.
ENGINE_DIR <- if (dir.exists("../R")) "../R" else "R"
for (f in list.files(ENGINE_DIR, pattern = "\\.R$", full.names = TRUE)) source(f)
WORKBOOK <- if (file.exists("../params/parameters_v3_2026-08.xlsx"))
  "../params/parameters_v3_2026-08.xlsx" else "params/parameters_v3_2026-08.xlsx"

RAW <- load_parameters(WORKBOOK)

fmt_inr  <- function(x) paste0("Rs ", format(round(x), big.mark = ",", scientific = FALSE))
fmt_pct  <- function(x) sprintf("%.1f%%", 100 * x)
fmt_r    <- function(x) sprintf("%.2f", x)

# ---- UI ------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Buffalo Bioeconomic Model",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 330,
    numericInput("herd_size", "Adult buffaloes", value = 50, min = 5, max = 1000, step = 1),
    radioButtons("years", "Horizon (years)", choices = c(5, 10), selected = 5, inline = TRUE),
    selectInput("automation", "Automation", c("mechanised", "manual")),
    selectInput("entity_type", "Entity form",
                c("proprietor", "partnership", "company", "cooperative")),
    checkboxInput("use_44ad", "Use s.44AD presumptive tax where eligible", TRUE),
    accordion(
      open = FALSE,
      accordion_panel(
        "Prices and costs (workbook defaults)",
        numericInput("price_per_litre", "Milk price (Rs/L)", NA, min = 30, max = 120, step = 1),
        numericInput("yield", "Milk yield (L/day, herd avg)", NA, min = 4, max = 20, step = 0.5),
        numericInput("purchase_price", "Animal purchase price (Rs)", NA,
                     min = 40000, max = 250000, step = 5000),
        sliderInput("feed_mult", "Feed cost multiplier", min = 0.7, max = 1.3, value = 1, step = 0.05)
      ),
      accordion_panel(
        "Finance",
        numericInput("interest", "Loan interest rate", NA, min = 0.04, max = 0.20, step = 0.005),
        numericInput("subsidy", "Capital subsidy share", 0, min = 0, max = 0.5, step = 0.01),
        sliderInput("moratorium", "Moratorium (months)", min = 0, max = 24, value = 0, step = 6),
        numericInput("discount", "Discount rate", NA, min = 0.05, max = 0.20, step = 0.005)
      )
    ),
    input_task_button("run", "Run model", icon = icon("play")),
    helpText("Blank inputs use the workbook value scaled to the herd size.",
             "The workbook is the source of truth; overrides are listed in the report.")
  ),

  navset_card_tab(
    nav_panel("Summary",
      layout_columns(
        fill = FALSE,
        value_box("NPV (post-tax)", textOutput("vb_npv"), showcase = icon("scale-balanced")),
        value_box("IRR pre / post tax", textOutput("vb_irr"), showcase = icon("percent")),
        value_box("Min DSCR", textOutput("vb_dscr"), showcase = icon("building-columns")),
        value_box("Payback", textOutput("vb_payback"), showcase = icon("clock"))
      ),
      card(card_header("Viability summary"), DTOutput("summary_tbl"), min_height = 480),
      card(card_header("Project cost"), DTOutput("capital_tbl"), min_height = 380)
    ),
    nav_panel("Herd",
      card(card_header("Herd structure by month"), plotOutput("herd_plot", height = 320)),
      card(card_header("Annual herd projection"), DTOutput("herd_tbl"), min_height = 320)
    ),
    nav_panel("Operating",
      card(card_header("Revenue and operating cost by year"), plotOutput("op_plot", height = 320)),
      card(card_header("Operating statement (annual)"), DTOutput("op_tbl"), min_height = 320)
    ),
    nav_panel("Finance",
      card(card_header("Debt service cover by year"), plotOutput("dscr_plot", height = 280)),
      card(card_header("P&L, tax and debt service"), DTOutput("fin_tbl"), min_height = 320),
      card(card_header("Loan schedule"), DTOutput("loan_tbl"), min_height = 320)
    ),
    nav_panel("Cash flows",
      card(card_header("Cumulative post-tax project cash flow"), plotOutput("cf_plot", height = 280)),
      card(card_header("Cash flows (t = 0 first row)"), DTOutput("cf_tbl"), min_height = 320)
    ),
    nav_panel("Sensitivity",
      card(card_header("One-at-a-time tornado (post-tax NPV)"),
           input_task_button("run_tornado", "Compute tornado", icon = icon("chart-bar")),
           plotOutput("tornado_plot", height = 360)),
      card(card_header("Breakeven"),
           input_task_button("run_breakeven", "Compute breakeven milk price"),
           tableOutput("breakeven_tbl"))
    ),
    nav_panel("Parameters",
      card(card_header("Every parameter at this herd size"), DTOutput("param_tbl"), min_height = 600)
    ),
    nav_panel("Download",
      card(card_header("Bankable report"),
           p("Excel workbook with summary, capital, herd, operating statement,",
             "financials, loan, terminal value, cash flows and your overrides."),
           downloadButton("dl_report", "Download Excel report"))
    )
  )
)

# ---- server ---------------------------------------------------------------------
server <- function(input, output, session) {

  # workbook-resolved defaults follow the herd size
  observeEvent(input$herd_size, {
    n <- input$herd_size
    req(is.finite(n), n >= 5)
    p <- scale_parameters(RAW, n)
    updateNumericInput(session, "price_per_litre", value = round(p$price_per_litre, 1))
    updateNumericInput(session, "yield", value = round(p$yield_herd_avg_litres_per_day, 2))
    updateNumericInput(session, "purchase_price", value = round(p$purchase_price))
    updateNumericInput(session, "interest", value = p$loan_interest_rate)
    updateNumericInput(session, "discount", value = p$discount_rate_1)
    updateSelectInput(session, "entity_type", selected = p$entity_type)
  }, ignoreInit = FALSE)

  overrides <- reactive({
    p <- scale_parameters(RAW, input$herd_size)
    ov <- list(
      entity_type = input$entity_type,
      use_presumptive_44ad = as.numeric(isTRUE(input$use_44ad)),
      capital_subsidy_pct = input$subsidy %||% 0,
      moratorium_months = input$moratorium %||% 0,
      feed_cost_multiplier = input$feed_mult %||% 1
    )
    # numeric fields: only override when they differ from the workbook value
    maybe <- function(nm, val, base) if (is.finite(val) && abs(val - base) > 1e-9)
      setNames(list(val), nm) else NULL
    ov <- c(ov,
      maybe("price_per_litre", input$price_per_litre, p$price_per_litre),
      maybe("yield_herd_avg_litres_per_day", input$yield, p$yield_herd_avg_litres_per_day),
      maybe("purchase_price", input$purchase_price, p$purchase_price),
      maybe("loan_interest_rate", input$interest, p$loan_interest_rate),
      maybe("discount_rate_1", input$discount, p$discount_rate_1))
    ov
  })

  result <- eventReactive(input$run, {
    withProgress(message = "Running the model...", value = 0.4, {
      run_buffalo_model(input$herd_size, as.integer(input$years),
                        overrides = overrides(), automation = input$automation,
                        raw = RAW)
    })
  }, ignoreNULL = FALSE)

  sval <- function(m) { s <- result()$summary; s[metric == m, value] }

  output$vb_npv <- renderText(fmt_inr(sval(sprintf("NPV post-tax @ %.0f%%",
                              100 * result()$params$discount_rate_1))))
  output$vb_irr <- renderText(paste0(fmt_pct(sval("IRR pre-tax")), " / ",
                                     fmt_pct(sval("IRR post-tax"))))
  output$vb_dscr <- renderText(fmt_r(sval("DSCR minimum")))
  output$vb_payback <- renderText(sprintf("%.1f years", sval("Payback (years, post-tax)")))

  output$summary_tbl <- renderDT({
    s <- copy(result()$summary)
    s[, value := fifelse(abs(value) > 100, round(value), round(value, 3))]
    datatable(s, options = list(dom = "t", pageLength = 15), rownames = FALSE)
  })
  output$capital_tbl <- renderDT({
    cap <- copy(result()$capital)[, cost := round(cost)]
    datatable(cap, options = list(dom = "t"), rownames = FALSE)
  })

  output$herd_plot <- renderPlot({
    h <- melt(result()$herd_monthly[, .(month, adults, in_milk, heifers, calves)],
              id.vars = "month")
    ggplot(h, aes(month, value, colour = variable)) + geom_line(linewidth = 0.9) +
      labs(x = "Month", y = "Head", colour = NULL) + theme_minimal(base_size = 13)
  })
  output$herd_tbl <- renderDT({
    ha <- copy(result()$herd_annual)
    num <- names(ha)[vapply(ha, is.numeric, TRUE)]
    ha[, (num) := lapply(.SD, function(x) round(x, 1)), .SDcols = num]
    datatable(ha, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$op_plot <- renderPlot({
    oa <- result()$operating_annual
    d <- melt(oa[, .(year, revenue = revenue_total, `operating cost` = opex_total,
                     EBITDA = ebitda)], id.vars = "year")
    ggplot(d, aes(year, value / 1e5, fill = variable)) +
      geom_col(position = "dodge") +
      labs(x = "Year", y = "Rs lakh", fill = NULL) + theme_minimal(base_size = 13)
  })
  output$op_tbl <- renderDT({
    oa <- copy(result()$operating_annual)
    num <- names(oa)[vapply(oa, is.numeric, TRUE)]
    oa[, (num) := lapply(.SD, round), .SDcols = num]
    datatable(oa, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$dscr_plot <- renderPlot({
    f <- result()$financials[!is.na(dscr)]
    ggplot(f, aes(year, dscr)) + geom_col(fill = "#2c7fb8") +
      geom_hline(yintercept = 1.25, linetype = 2, colour = "red") +
      annotate("text", x = Inf, y = 1.25, label = "bank norm 1.25",
               hjust = 1.1, vjust = -0.5, size = 3.5, colour = "red") +
      labs(x = "Year", y = "DSCR") + theme_minimal(base_size = 13)
  })
  output$fin_tbl <- renderDT({
    f <- copy(result()$financials)
    num <- setdiff(names(f)[vapply(f, is.numeric, TRUE)], c("year", "dscr"))
    f[, (num) := lapply(.SD, round), .SDcols = num]
    f[, dscr := round(dscr, 2)]
    datatable(f, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$loan_tbl <- renderDT({
    l <- copy(result()$loan$schedule)
    num <- setdiff(names(l), "year")
    l[, (num) := lapply(.SD, round), .SDcols = num]
    datatable(l, options = list(dom = "t"), rownames = FALSE)
  })

  output$cf_plot <- renderPlot({
    cf <- result()$cashflows$project_post
    d <- data.table(year = seq_along(cf) - 1, cumulative = cumsum(cf) / 1e5)
    ggplot(d, aes(year, cumulative)) + geom_line(linewidth = 1) + geom_point() +
      geom_hline(yintercept = 0, linetype = 2) +
      labs(x = "Year", y = "Cumulative post-tax cash (Rs lakh)") +
      theme_minimal(base_size = 13)
  })
  output$cf_tbl <- renderDT({
    r <- result()
    cf <- data.table(year = seq_along(r$cashflows$project_pre) - 1L,
                     project_pre_tax = round(r$cashflows$project_pre),
                     project_post_tax = round(r$cashflows$project_post),
                     equity_post_tax = round(r$cashflows$equity))
    datatable(cf, options = list(dom = "t"), rownames = FALSE)
  })

  tornado_res <- eventReactive(input$run_tornado, {
    withProgress(message = "Computing tornado (about 20 model runs)...", value = 0.3,
      tornado(RAW, input$herd_size, as.integer(input$years)))
  })
  output$tornado_plot <- renderPlot({
    t <- tornado_res()
    t[, driver := factor(driver, levels = rev(t$driver))]
    ggplot(t, aes(y = driver)) +
      geom_segment(aes(x = npv_low / 1e5, xend = npv_high / 1e5, yend = driver),
                   linewidth = 6, colour = "#74add1") +
      geom_vline(aes(xintercept = npv_base[1] / 1e5), linetype = 2) +
      labs(x = "Post-tax NPV (Rs lakh), driver swung +/-10%", y = NULL) +
      theme_minimal(base_size = 13)
  })

  breakeven_res <- eventReactive(input$run_breakeven, {
    withProgress(message = "Solving breakeven...", value = 0.3, {
      data.frame(
        Measure = c("Milk price for NPV = 0", "Milk price for minimum DSCR = 1.25",
                    "Workbook milk price at this herd size"),
        `Rs per litre` = round(c(
          breakeven_milk_price(RAW, input$herd_size, as.integer(input$years)),
          breakeven_price_for_dscr(RAW, input$herd_size, as.integer(input$years)),
          scale_parameters(RAW, input$herd_size)$price_per_litre), 2),
        check.names = FALSE)
    })
  })
  output$breakeven_tbl <- renderTable(breakeven_res())

  output$param_tbl <- renderDT({
    p <- result()$params
    keep <- vapply(p, function(x) is.numeric(x) && length(x) == 1, TRUE)
    d <- data.table(parameter = names(p)[keep],
                    value = round(unlist(p[keep]), 4))
    datatable(d, options = list(pageLength = 25), rownames = FALSE)
  })

  output$dl_report <- downloadHandler(
    filename = function() sprintf("buffalo_%dhead_%dy_%s.xlsx", input$herd_size,
                                  as.integer(input$years), Sys.Date()),
    content = function(file) {
      # tempfile-safe: never writes into the project folder
      write_report(result(), file)
    }
  )
}

shinyApp(ui, server)
