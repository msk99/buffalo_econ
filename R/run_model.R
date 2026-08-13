# run_model.R -- the orchestrator: workbook -> herd -> economics -> finance.
#
# One timeline everywhere: all capital (both animal batches, civil, plant,
# working capital) is committed at t = 0; operations at end of Years 1..n;
# terminal value inside Year n. Pre-tax and post-tax results are reported side
# by side (plan 3.4); equity returns are post-tax after debt service.

library(data.table)


#' Run the full model
#'
#' @param raw  loaded workbook (load_parameters()); loaded on demand if NULL
#' @param herd_size adult buffaloes (5..1000)
#' @param years horizon, 5 or 10
#' @param overrides named list of parameter overrides (user sidebar)
#' @param automation "mechanised" or "manual"
#' @return list with every intermediate and a summary
run_buffalo_model <- function(herd_size, years = 5, overrides = list(),
                              automation = "mechanised", raw = NULL,
                              workbook = NULL) {
  if (is.null(raw)) raw <- load_parameters(workbook)
  p <- scale_parameters(raw, herd_size, overrides = overrides, automation = automation)

  # ---- physical model -------------------------------------------------------
  herd_m <- project_herd(p, years)
  herd_a <- herd_annual(herd_m)
  op_m   <- operating_statement(herd_m, p)
  op_a   <- operating_annual(op_m)

  # ---- capital, working capital from the first full operating year ----------
  wc_year <- if (years >= 2) 2L else 1L
  working_capital <- op_a[year == wc_year, opex_total] * p$working_capital_months / 12
  capital <- capital_schedule(p, working_capital)
  project_cost <- capital[, sum(cost)]

  loan <- loan_schedule(project_cost, p, years)
  bd   <- book_depreciation(p, capital, years)
  td   <- tax_depreciation(p, capital, herd_a, years)
  tv   <- terminal_value(p, capital, herd_a, years)
  terminal_total <- tv[, sum(value)]

  # ---- annual financials ----------------------------------------------------
  fin <- op_a[, .(year, revenue_total, opex_total, ebitda)]
  fin <- fin[bd[, .(year, book_dep = dep_total)], on = "year"]
  fin <- fin[td[, .(year, tax_dep = tax_dep_total)], on = "year"]
  fin <- fin[loan$schedule[, .(year, interest, principal)], on = "year"]
  fin[, pbt := ebitda - book_dep - interest]
  fin[, turnover := revenue_total]
  fin[, taxable_regular := ebitda - tax_dep - interest]

  tx <- tax_schedule(fin, p)
  fin <- fin[tx[, .(year, tax_basis = basis, tax)], on = "year"]
  fin[, pat := pbt - tax]

  ds <- dscr_table(fin$pat, fin$book_dep, fin$interest, fin$principal)
  fin <- fin[ds, on = "year"]

  # ---- cash flows (t = 0 first element) --------------------------------------
  op_years <- fin$ebitda
  op_years[years] <- op_years[years] + terminal_total
  cf_project_pre  <- c(-project_cost, op_years)
  cf_project_post <- c(-project_cost, op_years - fin$tax)
  cf_equity <- c(-(loan$margin),
                 op_years - fin$tax - fin$interest - fin$principal)

  r1 <- p$discount_rate_1
  r2 <- p$discount_rate_2

  summary <- data.table(
    metric = c("Project cost", "Loan", "Margin (equity)",
               sprintf("NPV pre-tax @ %.0f%%", 100 * r1),
               sprintf("NPV pre-tax @ %.0f%%", 100 * r2),
               sprintf("NPV post-tax @ %.0f%%", 100 * r1),
               "IRR pre-tax", "IRR post-tax", "Equity IRR post-tax",
               sprintf("BCR @ %.0f%%", 100 * r1),
               "Payback (years, post-tax)",
               "DSCR minimum", "DSCR average"),
    value = c(project_cost, loan$loan, loan$margin,
              npv(r1, cf_project_pre), npv(r2, cf_project_pre),
              npv(r1, cf_project_post),
              irr(cf_project_pre), irr(cf_project_post), irr(cf_equity),
              bcr(r1, cf_project_pre),
              payback_years(cf_project_post),
              min(fin$dscr, na.rm = TRUE), mean(fin$dscr, na.rm = TRUE))
  )

  list(params = p, herd_monthly = herd_m, herd_annual = herd_a,
       operating_monthly = op_m, operating_annual = op_a,
       capital = capital, working_capital = working_capital,
       loan = loan, book_dep = bd, tax_dep = td, terminal = tv,
       financials = fin,
       cashflows = list(project_pre = cf_project_pre,
                        project_post = cf_project_post,
                        equity = cf_equity),
       summary = summary, herd_size = herd_size, years = years)
}


#' @export
print.buffalo_result <- function(x, ...) print(x$summary)
