# finance_loan.R -- term loan, moratorium, subsidy, debt service.
#
# Equal annual principal repayment on a reducing balance (decision record #9;
# switch to EMI later if a bank template requires it). Interest is paid, not
# capitalised, during the moratorium. Capital subsidy (default 0) reduces the
# loan principal at t = 0; the NABARD back-ended subsidy-reserve convention is
# a recorded future item.

library(data.table)


#' Annual loan schedule
#' @param project_cost total project cost at t = 0
#' @return list(loan, margin, subsidy, schedule = data.table)
loan_schedule <- function(project_cost, p, years) {
  subsidy <- project_cost * p$capital_subsidy_pct
  loan    <- project_cost * p$loan_pct_of_project - subsidy
  margin  <- project_cost - loan - subsidy
  rate    <- p$loan_interest_rate - p$interest_subvention_pct
  tenure  <- as.integer(p$loan_tenure_years)
  morat_y <- as.integer(round(p$moratorium_months / 12))

  opening <- loan
  rows <- vector("list", years)
  for (y in seq_len(years)) {
    interest <- opening * rate
    principal <- if (y > morat_y && opening > 0) min(opening, loan / tenure) else 0
    rows[[y]] <- data.table(year = y, opening = opening,
                            interest = interest, principal = principal,
                            closing = opening - principal)
    opening <- opening - principal
  }
  list(loan = loan, margin = margin, subsidy = subsidy,
       schedule = rbindlist(rows))
}
