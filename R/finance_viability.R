# finance_viability.R -- NPV, IRR, BCR, payback, DSCR on ONE timeline.
#
# Every metric consumes the same convention: capital at t = 0,
# operations at the end of Years 1..n, terminal value inside Year n.

library(data.table)


#' Net present value. cf includes t = 0 as its first element.
npv <- function(rate, cf) sum(cf / (1 + rate)^(seq_along(cf) - 1))


#' Internal rate of return of a cash-flow vector starting at t = 0
irr <- function(cf) {
  if (all(cf >= 0) || all(cf <= 0)) return(NA_real_)
  f <- function(r) npv(r, cf)
  out <- tryCatch(uniroot(f, c(-0.95, 15), tol = 1e-9)$root, error = function(e) NA_real_)
  out
}


#' Benefit-cost ratio: PV of inflows over PV of outflows, same timeline
bcr <- function(rate, cf) {
  t <- seq_along(cf) - 1
  pv <- cf / (1 + rate)^t
  inflow  <- sum(pv[pv > 0])
  outflow <- -sum(pv[pv < 0])
  if (outflow == 0) return(NA_real_)
  inflow / outflow
}


#' Payback period in years (undiscounted, interpolated within the year)
payback_years <- function(cf) {
  cum <- cumsum(cf)
  idx <- which(cum >= 0)[1]
  if (is.na(idx)) return(NA_real_)
  if (idx == 1) return(0)
  prev <- cum[idx - 1]
  (idx - 2) + (-prev) / cf[idx]   # t = 0 is element 1
}


#' Debt-service coverage per year and its summary
dscr_table <- function(pat, book_dep, interest, principal) {
  service <- interest + principal
  # DSCR is meaningful only while debt is actually being serviced (> Re 1;
  # guards against floating-point residue after the final repayment)
  dscr <- ifelse(service > 1, (pat + book_dep + interest) / service, NA_real_)
  data.table(year = seq_along(dscr), dscr = dscr)
}
