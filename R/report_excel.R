# report_excel.R -- bankable project workbook.
#
# Writes to a caller-supplied path (the app passes a tempfile: shinyapps.io
# has no writable project folder). A NABARD/bank template is still awaited;
# until it arrives the layout follows the agreed tab structure.

library(data.table)
library(openxlsx2)


#' Write the full model result to an Excel workbook
#'
#' @param result list from run_buffalo_model()
#' @param path output .xlsx path (use tempfile(fileext = ".xlsx") in the app)
#' @param sens optional tornado table from tornado()
#' @return the path, invisibly
write_report <- function(result, path, sens = NULL) {

  wb <- wb_workbook(creator = "Buffalo Bioeconomic Model")

  add_sheet <- function(name, dt, title) {
    wb$add_worksheet(name)
    wb$add_data(sheet = name, x = title, start_row = 1)
    wb$add_data(sheet = name, x = dt, start_row = 3, na.strings = "")
    wb$add_font(sheet = name, dims = "A1", bold = TRUE)
    wb$set_col_widths(sheet = name, cols = seq_len(ncol(dt)), widths = "auto")
    invisible(NULL)
  }

  fmt_num <- function(dt) {
    out <- copy(dt)
    for (c in names(out)) if (is.numeric(out[[c]])) out[, (c) := round(get(c), 2)]
    out
  }

  scen <- sprintf("%d adult buffaloes, %d-year horizon, %s",
                  result$herd_size, result$years, result$params$automation)

  add_sheet("Summary",   fmt_num(result$summary),      paste("Viability summary -", scen))
  add_sheet("Capital",   fmt_num(result$capital),      "Project cost at t = 0 (INR)")
  add_sheet("Herd",      fmt_num(result$herd_annual),  "Herd projection (annual; stocks averaged, flows summed)")
  add_sheet("Operating", fmt_num(result$operating_annual), "Operating statement by year (INR)")
  add_sheet("Financials",fmt_num(result$financials),   "P&L, tax and debt service by year (INR)")
  add_sheet("Loan",      fmt_num(result$loan$schedule),
            sprintf("Loan schedule (loan %.0f, margin %.0f, subsidy %.0f)",
                    result$loan$loan, result$loan$margin, result$loan$subsidy))
  add_sheet("Terminal",  fmt_num(result$terminal),     sprintf("Terminal value in year %d", result$years))

  cf <- data.table(year = seq_along(result$cashflows$project_pre) - 1L,
                   project_pre_tax  = result$cashflows$project_pre,
                   project_post_tax = result$cashflows$project_post,
                   equity_post_tax  = c(result$cashflows$equity,
                                        rep(NA, length(result$cashflows$project_pre) -
                                              length(result$cashflows$equity))))
  add_sheet("Cash flows", fmt_num(cf), "Cash flows on one timeline (t = 0 first row)")

  if (!is.null(sens)) add_sheet("Sensitivity", fmt_num(sens),
                                "One-at-a-time tornado: post-tax project NPV")

  # overrides the user applied, so a reviewer can see what was changed
  ov <- result$params$.overrides
  if (!is.null(ov) && length(ov) > 0) {
    add_sheet("Overrides",
              data.table(parameter = names(ov),
                         value = vapply(ov, function(x) as.character(x)[1], character(1))),
              "User overrides applied to the workbook defaults")
  }

  wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}
