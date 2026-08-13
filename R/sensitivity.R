# sensitivity.R -- one-at-a-time tornado and breakeven solvers.
#
# The tornado swings each driver +/- a step and records the
# post-tax project NPV at discount_rate_1; the breakeven solvers invert the
# model on milk price.

library(data.table)

TORNADO_DRIVERS <- c(
  "price_per_litre", "yield_herd_avg_litres_per_day", "feed_cost_multiplier",
  "purchase_price", "calving_interval_months", "wage_per_month",
  "loan_interest_rate", "mortality_rate_adult", "culling_salvage_per_head"
)


#' Project NPV for one override set (helper)
#' @param basis "post" (default, post-tax) or "pre"
npv_for <- function(raw, herd_size, years, overrides = list(), basis = "post") {
  r <- run_buffalo_model(herd_size, years, overrides = overrides, raw = raw)
  cf <- if (identical(basis, "pre")) r$cashflows$project_pre else r$cashflows$project_post
  npv(r$params$discount_rate_1, cf)
}


#' One-at-a-time tornado
#'
#' @param swing proportional change applied down and up (default 10%)
#' @return data.table driver, low, base, high, swing_total; sorted widest first
tornado <- function(raw, herd_size, years = 5, swing = 0.10,
                    drivers = TORNADO_DRIVERS) {
  base_p <- scale_parameters(raw, herd_size)
  base   <- npv_for(raw, herd_size, years)
  rows <- lapply(drivers, function(d) {
    v <- base_p[[d]]
    if (is.null(v) || !is.numeric(v)) return(NULL)
    lo <- npv_for(raw, herd_size, years, setNames(list(v * (1 - swing)), d))
    hi <- npv_for(raw, herd_size, years, setNames(list(v * (1 + swing)), d))
    data.table(driver = d, base_value = v, npv_low = lo, npv_base = base,
               npv_high = hi, swing_total = abs(hi - lo))
  })
  out <- rbindlist(rows)
  setorder(out, -swing_total)
  out[]
}


#' Milk price at which the post-tax project NPV is zero
breakeven_milk_price <- function(raw, herd_size, years = 5) {
  f <- function(price) npv_for(raw, herd_size, years, list(price_per_litre = price))
  tryCatch(uniroot(f, c(10, 200), tol = 0.01)$root, error = function(e) NA_real_)
}


#' Milk price at which the minimum DSCR reaches a target (bank norm 1.25)
breakeven_price_for_dscr <- function(raw, herd_size, years = 5, target = 1.25) {
  f <- function(price) {
    r <- run_buffalo_model(herd_size, years,
                           overrides = list(price_per_litre = price), raw = raw)
    min(r$financials$dscr, na.rm = TRUE) - target
  }
  tryCatch(uniroot(f, c(10, 200), tol = 0.01)$root, error = function(e) NA_real_)
}
